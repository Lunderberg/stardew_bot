use std::collections::HashSet;

use elsa::FrozenMap;
use thiserror::Error;

use crate::{
    runtime_type::{
        DotNetType, FunctionType, IteratorType, RuntimePrimType, RustType,
        TupleType,
    },
    CachedReader, Error, RuntimeType,
};

use super::{ExprKind, OpIndex, SymbolicGraph, SymbolicValue};

pub struct TypeInference<'a> {
    opt_reader: Option<CachedReader<'a>>,
    cache: FrozenMap<OpIndex, Box<RuntimeType>>,
}

#[derive(Error)]
pub enum TypeInferenceError {
    #[error(
        "Within a local-only context, \
         attempted to infer the type of an .NET expression \
         within a remote process."
    )]
    NoRemoteProcess,

    #[error(
        "Attempted to make a function call \
             on an expression not of function type."
    )]
    AttemptedCallOnNonFunction,

    #[error(
        "Interop with native function \
         with signature '{sig}' is not supported, \
         because {reason}."
    )]
    UnsupportedNativeFunction { sig: String, reason: String },

    #[error(
        "Attempted to apply mapping function to iterator, \
         but the provided map was not a function."
    )]
    AttemptedMapOnNonFunction,

    #[error(
        "Vector operations require a vector operand, \
         but were applied to type '{0}'."
    )]
    InvalidVectorType(RuntimeType),

    #[error(
        "Vector are only supported when they contain \
         primitive elements, or rust-native types that are not vectors.  \
         Cannot construct a vector of type '{0}'. "
    )]
    InvalidVectorElementType(RuntimeType),

    #[error(
        "Attempted to collect from type '{0}', \
         but only iterators can be collected."
    )]
    CollectRequiresIterator(RuntimeType),
}

impl<'a> TypeInference<'a> {
    pub fn new(reader: Option<CachedReader<'a>>) -> Self {
        let opt_reader = reader.into();
        Self {
            opt_reader,
            cache: Default::default(),
        }
    }

    fn reader(&self) -> Result<CachedReader<'a>, TypeInferenceError> {
        self.opt_reader.ok_or(TypeInferenceError::NoRemoteProcess)
    }

    pub fn infer_type(
        &'a self,
        graph: &SymbolicGraph,
        value: SymbolicValue,
    ) -> Result<&'a RuntimeType, Error> {
        let op_index = match value {
            SymbolicValue::Bool(_) => {
                return Ok(&RuntimeType::Prim(RuntimePrimType::Bool));
            }
            SymbolicValue::Int(_) => {
                return Ok(&RuntimeType::Prim(RuntimePrimType::NativeUInt));
            }
            SymbolicValue::Ptr(_) => {
                return Ok(&RuntimeType::Prim(RuntimePrimType::Ptr));
            }
            SymbolicValue::Result(op_index) => op_index,
        };

        if let Some(cached) = self.cache.get(&op_index) {
            return Ok(cached);
        }

        // To avoid having the call stack grow as deep as nested
        // expressions may be, first collect the expressions whose
        // types should be inferred.
        let to_infer = {
            let mut reverse_topologic_order: Vec<OpIndex> = vec![];
            let mut seen = HashSet::<OpIndex>::new();
            let mut to_visit = vec![op_index];

            while let Some(visiting) = to_visit.pop() {
                if self.cache.get(&visiting).is_none() {
                    reverse_topologic_order.push(visiting);
                    let expr = &graph[visiting];
                    expr.visit_reachable_nodes(|input_value| {
                        if let SymbolicValue::Result(input_index) = input_value
                        {
                            if !seen.contains(&input_index) {
                                to_visit.push(input_index);
                                seen.insert(input_index);
                            }
                        }
                    });
                }
            }

            reverse_topologic_order
        };

        for index_to_infer in to_infer.into_iter().rev() {
            let expect_cache = |value: SymbolicValue,
                                context: &'static str|
             -> &RuntimeType {
                match value {
                        SymbolicValue::Bool(_) => {
                            &RuntimeType::Prim(RuntimePrimType::Bool)
                        }
                        SymbolicValue::Int(_) => {
                            &RuntimeType::Prim(RuntimePrimType::NativeUInt)
                        }
                        SymbolicValue::Ptr(_) => {
                            &RuntimeType::Prim(RuntimePrimType::Ptr)
                        }
                        SymbolicValue::Result(op_index) => {
                            self.cache.get(&op_index).unwrap_or_else(|| {
                                panic!(
                                    "Internal error: \
                                     No cached value for '{context}'.  \
                                     Topologic sort should ensure that \
                                     all input expressions have their type inferred."
                                )
                            })
                        }
                    }
            };

            let expr_kind = &graph[index_to_infer].kind;
            let inferred_type = match expr_kind {
                ExprKind::Function { params, output } => {
                    let params = Some(
                        params
                            .iter()
                            .map(|param| expect_cache(*param, "function param"))
                            .cloned()
                            .collect(),
                    );
                    let output = Box::new(
                        expect_cache(*output, "function output").clone(),
                    );
                    FunctionType { params, output }.into()
                }
                ExprKind::FunctionArg(ty) => ty.clone(),
                ExprKind::FunctionCall { func, .. } => {
                    match expect_cache(*func, "function definition") {
                        RuntimeType::Function(FunctionType {
                            output, ..
                        }) => Ok(output.as_ref().clone()),
                        _ => {
                            Err(TypeInferenceError::AttemptedCallOnNonFunction)
                        }
                    }?
                }
                ExprKind::Range { .. } => {
                    let item = RuntimeType::Prim(RuntimePrimType::NativeUInt);
                    IteratorType {
                        item: Box::new(item),
                    }
                    .into()
                }
                ExprKind::Map { map, .. } => {
                    let map = expect_cache(*map, "mapping function");
                    match map {
                        RuntimeType::Function(FunctionType {
                            output, ..
                        }) => Ok(IteratorType {
                            item: output.clone(),
                        }
                        .into()),
                        _ => Err(TypeInferenceError::AttemptedMapOnNonFunction),
                    }?
                }
                ExprKind::Filter { iterator, .. } => {
                    let iter =
                        expect_cache(*iterator, "iterator being filtered");
                    iter.clone()
                }
                ExprKind::Collect { iterator } => {
                    let iter =
                        expect_cache(*iterator, "iterator being collected");
                    match iter {
                        RuntimeType::Unknown => RuntimeType::Unknown,
                        RuntimeType::Iterator(IteratorType { item }) => {
                            item.as_ref().vector_type()?
                        }
                        other => {
                            Err(TypeInferenceError::CollectRequiresIterator(
                                other.clone(),
                            ))?
                        }
                    }
                }
                ExprKind::Reduce { initial, .. }
                | ExprKind::SimpleReduce { initial, .. } => {
                    expect_cache(*initial, "initial reduction value").clone()
                }
                ExprKind::NativeFunction(func) => func.signature()?,
                ExprKind::Tuple(elements) => {
                    let elements = elements
                        .iter()
                        .map(|element| expect_cache(*element, "tuple element"))
                        .cloned()
                        .collect();
                    TupleType(elements).into()
                }
                ExprKind::StaticField(static_field) => {
                    let reader = self.reader()?;
                    static_field.runtime_type(reader)?
                }
                ExprKind::FieldAccess { obj, field } => {
                    let obj_type = expect_cache(*obj, "object of field access");

                    let method_table_ptr = obj_type
                        .method_table_for_field_access(|| {
                            format!("{}", graph.print(*obj))
                        })?;
                    let field_type =
                        self.reader()?.field_by_name_to_runtime_type(
                            method_table_ptr,
                            field.as_str(),
                        )?;
                    field_type
                }
                ExprKind::SymbolicDowncast { ty, .. } => {
                    let reader = self.reader()?;
                    let method_table = ty.method_table(reader)?;
                    DotNetType::Class {
                        method_table: Some(method_table),
                    }
                    .into()
                }
                ExprKind::IndexAccess {
                    obj: array,
                    indices,
                } => {
                    let array_type = expect_cache(*array, "array");
                    let num_indices = indices.len();

                    match array_type {
                        RuntimeType::DotNet(DotNetType::Array { .. })
                            if num_indices != 1 =>
                        {
                            Err(Error::IncorrectNumberOfIndices {
                                num_provided: num_indices,
                                num_expected: 1,
                            })
                        }
                        RuntimeType::DotNet(DotNetType::MultiDimArray {
                            rank,
                            ..
                        }) if num_indices != *rank => {
                            return Err(Error::IncorrectNumberOfIndices {
                                num_provided: num_indices,
                                num_expected: *rank,
                            });
                        }
                        RuntimeType::DotNet(
                            DotNetType::MultiDimArray {
                                method_table: None,
                                ..
                            }
                            | DotNetType::Array {
                                method_table: None, ..
                            },
                        ) => {
                            return Err(Error::UnexpectedNullMethodTable(
                                format!("{}", graph.print(*array)),
                            ));
                        }
                        RuntimeType::DotNet(
                            DotNetType::MultiDimArray {
                                method_table: Some(ptr),
                                ..
                            }
                            | DotNetType::Array {
                                method_table: Some(ptr),
                                ..
                            },
                        ) => {
                            let reader = self.reader()?;
                            let method_table = reader.method_table(*ptr)?;
                            method_table
                                .array_element_type()
                                .ok_or(Error::ArrayMissingElementType)
                                .and_then(|ptr| reader.runtime_type(ptr))
                        }

                        other => {
                            Err(Error::IndexAccessRequiresArray(other.clone()))
                        }
                    }?
                }
                ExprKind::ArrayExtent { .. }
                | ExprKind::NumArrayElements { .. } => {
                    RuntimeType::Prim(RuntimePrimType::NativeUInt)
                }
                ExprKind::PointerCast { ty, .. } => ty.clone(),
                ExprKind::IsSome(_) => RuntimeType::Prim(RuntimePrimType::Bool),

                ExprKind::IfElse {
                    if_branch,
                    else_branch,
                    ..
                } => {
                    let if_branch = expect_cache(*if_branch, "if branch");
                    let else_branch = expect_cache(*else_branch, "else branch");

                    match (if_branch, else_branch) {
                        (ty, RuntimeType::Unknown)
                        | (RuntimeType::Unknown, ty) => ty.clone(),
                        (ty, _) => ty.clone(),
                    }
                }

                ExprKind::Equal { .. }
                | ExprKind::NotEqual { .. }
                | ExprKind::LessThan { .. }
                | ExprKind::GreaterThan { .. }
                | ExprKind::LessThanOrEqual { .. }
                | ExprKind::GreaterThanOrEqual { .. } => {
                    RuntimeType::Prim(RuntimePrimType::Bool)
                }

                ExprKind::Add { lhs, rhs } => {
                    let lhs_type = expect_cache(*lhs, "lhs of add");
                    let rhs_type = expect_cache(*rhs, "rhs of add");
                    match (lhs_type, rhs_type) {
                        (
                            ptr,
                            RuntimeType::Prim(RuntimePrimType::NativeUInt),
                        )
                        | (
                            RuntimeType::Prim(RuntimePrimType::NativeUInt),
                            ptr,
                        ) if matches!(
                            ptr.storage_type(),
                            Some(RuntimePrimType::Ptr)
                        ) =>
                        {
                            Ok(RuntimePrimType::Ptr.into())
                        }

                        (
                            RuntimeType::Prim(RuntimePrimType::NativeUInt),
                            RuntimeType::Prim(RuntimePrimType::NativeUInt),
                        ) => Ok(RuntimePrimType::NativeUInt.into()),

                        (RuntimeType::Unknown, _)
                        | (_, RuntimeType::Unknown) => Ok(RuntimeType::Unknown),

                        (other_lhs, other_rhs) => {
                            Err(Error::InvalidOperandsForBinaryOp {
                                op: expr_kind.op_name(),
                                lhs: other_lhs.clone(),
                                rhs: other_rhs.clone(),
                            })
                        }
                    }?
                }

                ExprKind::Mul { lhs, rhs }
                | ExprKind::Div { lhs, rhs }
                | ExprKind::Mod { lhs, rhs } => {
                    let lhs_type = expect_cache(*lhs, "lhs of mul");
                    let rhs_type = expect_cache(*rhs, "rhs of mul");
                    match (lhs_type, rhs_type) {
                        (
                            RuntimeType::Prim(RuntimePrimType::NativeUInt),
                            RuntimeType::Prim(RuntimePrimType::NativeUInt),
                        ) => Ok(RuntimePrimType::NativeUInt.into()),
                        (RuntimeType::Unknown, _)
                        | (_, RuntimeType::Unknown) => Ok(RuntimeType::Unknown),
                        (other_lhs, other_rhs) => {
                            Err(Error::InvalidOperandsForBinaryOp {
                                op: expr_kind.op_name(),
                                lhs: other_lhs.clone(),
                                rhs: other_rhs.clone(),
                            })
                        }
                    }?
                }

                ExprKind::PrimCast { prim_type, .. } => (*prim_type).into(),
                ExprKind::PhysicalDowncast { obj, .. } => {
                    let obj_type = expect_cache(*obj, "obj to downcast");
                    match obj_type {
                        RuntimeType::Prim(RuntimePrimType::Ptr) => Ok(()),
                        other => Err(Error::InvalidOperandForPhysicalDowncast(
                            other.clone(),
                        )),
                    }?;
                    RuntimePrimType::Ptr.into()
                }
                ExprKind::ReadValue { ptr, prim_type } => {
                    let ptr_type = expect_cache(*ptr, "ptr to read");
                    match ptr_type {
                        RuntimeType::Prim(RuntimePrimType::Ptr) => Ok(()),
                        other => Err(Error::InvalidOperandForReadValue(
                            other.clone(),
                        )),
                    }?;
                    (*prim_type).into()
                }
                ExprKind::ReadString { .. } => RustType::new::<String>().into(),
            };

            self.cache.insert(index_to_infer, Box::new(inferred_type));
        }

        Ok(self.cache.get(&op_index).expect(
            "Internal error: \
             Topologic sort should ensure that \
             all input expressions have their type inferred.",
        ))
    }
}

impl std::fmt::Debug for TypeInferenceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
