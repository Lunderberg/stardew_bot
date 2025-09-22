use std::collections::HashSet;

use elsa::FrozenMap;
use thiserror::Error;

use crate::{DSLTypeExt as _, StaticFieldExt as _, SymbolicTypeExt as _};
use dotnet_debugger::{
    CachedReader, DotNetType, RuntimeType, TypeHandlePtrExt as _,
};
use dsl_ir::{
    DSLType, ExprKind, FunctionType, IteratorType, OpIndex, RuntimePrimType,
    RuntimePrimValueExt as _, RustType, SymbolicGraph, SymbolicValue,
    TupleType,
};

pub struct TypeInference<'a> {
    opt_reader: Option<CachedReader<'a>>,
    cache: FrozenMap<OpIndex, Box<DSLType>>,
}

#[derive(Error)]
pub enum TypeInferenceError {
    #[error("dsl::ir::error( {0} )")]
    DslIr(#[from] dsl_ir::Error),

    #[error("dotnet_debugger::Error( {0} )")]
    DotnetDebugger(#[from] dotnet_debugger::Error),

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
    InvalidVectorType(DSLType),

    #[error(
        "Vector are only supported when they contain \
         primitive elements, or rust-native types that are not vectors.  \
         Cannot construct a vector of type '{0}'. "
    )]
    InvalidVectorElementType(DSLType),

    #[error(
        "Attempted to collect from type '{0}', \
         but only iterators can be collected."
    )]
    CollectRequiresIterator(DSLType),

    #[error(
        "Attempted to use .find() method on type '{0}', \
         but .find() method requires an iterator."
    )]
    FindRequiresIterator(DSLType),

    #[error(
        "Attempted to use .first() method on type '{0}', \
         but .first() method requires an iterator."
    )]
    FirstRequiresIterator(DSLType),

    #[error(
        "Attempted to .find_map() method with argument '{0}', \
         but .find_map() method requires an function as argument."
    )]
    FindMapRequiresMappingFunction(DSLType),

    #[error(
        "The .reduce() method requires a reduction function, \
         but instead received an argument of type '{0}'."
    )]
    ReductionMustBeAFunction(DSLType),

    #[error(
        "The .reduce() method requires a reduction function, \
         which accepts two arguments, \
         but instead received a reduction function of type '{0}'."
    )]
    ReductionMustTakeTwoArguments(DSLType),

    #[error(
        "Cannot chain iterator of type '{0}' \
         with iterator of type '{1}'."
    )]
    ChainRequiresSameIteratorType(DSLType, DSLType),

    #[error(
        "The SymbolicOperation::IndexAccess(indices) operation \
         requires one index for each rank of the array being accessed.  \
         However, {num_provided} indices were provided \
         to access an array of rank {num_expected}."
    )]
    IncorrectNumberOfIndices {
        num_provided: usize,
        num_expected: usize,
    },

    #[error("MethodTable pointer for {0} was NULL.")]
    UnexpectedNullMethodTable(String),

    #[error("Could not find method table for '{0}'")]
    NoSuchMethodTableFound(String),

    #[error(
        "Could not find static field {field} within \
         method table of {class}."
    )]
    NoSuchStaticField { class: String, field: String },

    #[error(
        "The SymbolicOperation::Field(name) operation \
         accesses a field of a class or struct.  \
         However, it was applied to object '{0}' of type '{1}'."
    )]
    FieldAccessRequiresClassOrStruct(String, DSLType),

    #[error(
        "The SymbolicOperation::Downcast operation \
         casts an object to a subclass, \
         and may only be applied to Class instances.  \
         However, it was applied to an object of type {0}."
    )]
    DowncastRequiresClassInstance(DSLType),

    #[error(
        "Downcast requires the static type to be known, \
         but could not find a loaded method table for base class."
    )]
    DowncastRequiresKnownBaseClass,

    #[error("The method table of an array should contain the element type.")]
    ArrayMissingElementType,

    #[error(
        "The SymbolicOperation::IndexAccess(indices) operation \
         accesses an element of an array.  \
         However, it was applied to an object of type {0}."
    )]
    IndexAccessRequiresArray(DSLType),

    #[error("ReadValue requires pointer argument, but received {0}")]
    InvalidOperandForReadValue(DSLType),

    #[error(
        "Cannot apply operator '{op}' with operand types '{lhs}' and '{rhs}'"
    )]
    InvalidOperandsForBinaryOp {
        op: &'static str,
        lhs: DSLType,
        rhs: DSLType,
    },
}

macro_rules! infer_binary_op {
    ($func_name:ident,
     $(
         (
             $lhs_ty:ident,
             $rhs_ty:ident $(,)?
         ) => $result_ty:ident
     ),* $(,)?
    ) => {
        fn $func_name(
            &self,
            expr_kind: &ExprKind,
            lhs: SymbolicValue,
            rhs: SymbolicValue,
        ) -> Result<DSLType, TypeInferenceError> {
            let lhs_type = self.expect_cache(lhs);
            let rhs_type = self.expect_cache(rhs);
            match (lhs_type, rhs_type) {
                (DSLType::Unknown, _) | (_, DSLType::Unknown) => {
                    Ok(DSLType::Unknown)
                }

                $(
                    (
                        DSLType::Prim(RuntimePrimType::$lhs_ty),
                        DSLType::Prim(RuntimePrimType::$rhs_ty),
                    ) => Ok(DSLType::Prim(
                        RuntimePrimType::$result_ty
                    )),
                )*

                (other_lhs, other_rhs) => {
                    Err(TypeInferenceError::InvalidOperandsForBinaryOp {
                        op: expr_kind.op_name(),
                        lhs: other_lhs.clone(),
                        rhs: other_rhs.clone(),
                    })
                }
            }
        }
    };
}

impl<'a> TypeInference<'a> {
    pub fn new(reader: Option<CachedReader<'a>>) -> Self {
        let opt_reader = reader;
        Self {
            opt_reader,
            cache: Default::default(),
        }
    }

    fn reader(&self) -> Result<CachedReader<'a>, TypeInferenceError> {
        self.opt_reader.ok_or(TypeInferenceError::NoRemoteProcess)
    }

    fn expect_cache(&self, value: SymbolicValue) -> &DSLType {
        match value {
            SymbolicValue::Const(prim) => prim.static_runtime_type_ref(),
            SymbolicValue::Result(op_index) => {
                self.cache.get(&op_index).unwrap_or_else(|| {
                    panic!(
                        "Internal error: \
                         No cached value for operation at {op_index}. \
                         Topologic sort should ensure that \
                         all input expressions have their type inferred."
                    )
                })
            }
        }
    }

    pub fn infer_type(
        &'a self,
        graph: &SymbolicGraph,
        value: SymbolicValue,
    ) -> Result<&'a DSLType, TypeInferenceError> {
        let op_index = match value {
            SymbolicValue::Const(prim) => {
                return Ok(prim.static_runtime_type_ref());
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
                    expr.iter_input_nodes().for_each(|input_index| {
                        if !seen.contains(&input_index) {
                            to_visit.push(input_index);
                            seen.insert(input_index);
                        }
                    });
                }
            }

            reverse_topologic_order.sort();
            reverse_topologic_order
        };

        for index_to_infer in to_infer.into_iter() {
            let expr_kind = &graph[index_to_infer].kind;
            let inferred_type = match expr_kind {
                ExprKind::None => DSLType::Unknown,

                ExprKind::Function { params, output } => {
                    let params = Some(
                        params
                            .iter()
                            .map(|param| self.expect_cache(*param))
                            .cloned()
                            .collect(),
                    );
                    let output = Box::new(self.expect_cache(*output).clone());
                    FunctionType { params, output }.into()
                }
                ExprKind::FunctionArg(ty) => ty.clone(),
                ExprKind::FunctionCall { func, .. } => match self
                    .expect_cache(*func)
                {
                    DSLType::Function(FunctionType { output, .. }) => {
                        Ok(output.as_ref().clone())
                    }
                    DSLType::Unknown => Ok(DSLType::Unknown),
                    _ => Err(TypeInferenceError::AttemptedCallOnNonFunction),
                }?,
                ExprKind::Range { .. } => {
                    let item = DSLType::Prim(RuntimePrimType::NativeUInt);
                    IteratorType {
                        item: Box::new(item),
                    }
                    .into()
                }
                ExprKind::Map { map, .. } => {
                    let map = self.expect_cache(*map);
                    match map {
                        DSLType::Function(FunctionType { output, .. }) => {
                            Ok(IteratorType {
                                item: output.clone(),
                            }
                            .into())
                        }
                        _ => Err(TypeInferenceError::AttemptedMapOnNonFunction),
                    }?
                }
                ExprKind::Filter { iterator, .. } => {
                    let iter = self.expect_cache(*iterator);
                    iter.clone()
                }
                ExprKind::First { iterator } => {
                    let iter = self.expect_cache(*iterator);
                    match iter {
                        DSLType::Unknown => DSLType::Unknown,

                        DSLType::Iterator(IteratorType { item }) => {
                            item.as_ref().clone()
                        }

                        other => {
                            Err(TypeInferenceError::FirstRequiresIterator(
                                other.clone(),
                            ))?
                        }
                    }
                }
                ExprKind::Find { iterator, .. } => {
                    let iter = self.expect_cache(*iterator);
                    match iter {
                        DSLType::Unknown => DSLType::Unknown,

                        DSLType::Iterator(IteratorType { item }) => {
                            item.as_ref().clone()
                        }

                        other => {
                            Err(TypeInferenceError::FindRequiresIterator(
                                other.clone(),
                            ))?
                        }
                    }
                }
                ExprKind::FindMap { condition, .. } => {
                    let cond = self.expect_cache(*condition);
                    match cond {
                        DSLType::Unknown => DSLType::Unknown,

                        DSLType::Function(FunctionType { output, .. }) => {
                            output.as_ref().clone()
                        }

                        other => Err(
                            TypeInferenceError::FindMapRequiresMappingFunction(
                                other.clone(),
                            ),
                        )?,
                    }
                }
                ExprKind::Chain(iter_a, iter_b) => {
                    let iter_a_type = self.expect_cache(*iter_a);
                    let iter_b_type = self.expect_cache(*iter_b);
                    if iter_a_type == iter_b_type {
                        Ok(iter_a_type.clone())
                    } else if matches!(iter_a_type, DSLType::Unknown) {
                        Ok(iter_b_type.clone())
                    } else if matches!(iter_b_type, DSLType::Unknown) {
                        Ok(iter_a_type.clone())
                    } else {
                        Err(TypeInferenceError::ChainRequiresSameIteratorType(
                            iter_a_type.clone(),
                            iter_b_type.clone(),
                        ))
                    }?
                }
                ExprKind::Collect { iterator } => {
                    let iter = self.expect_cache(*iterator);
                    match iter {
                        DSLType::Unknown => DSLType::Unknown,

                        DSLType::Iterator(IteratorType { item }) => {
                            match item.as_ref() {
                                DSLType::Unknown => DSLType::Unknown,
                                other => other.vector_type()?,
                            }
                        }

                        other => {
                            Err(TypeInferenceError::CollectRequiresIterator(
                                other.clone(),
                            ))?
                        }
                    }
                }
                ExprKind::Reduce {
                    initial, reduction, ..
                }
                | ExprKind::SimpleReduce {
                    initial, reduction, ..
                } => {
                    let initial = self.expect_cache(*initial).clone();
                    let reduction = self.expect_cache(*reduction).clone();

                    let (reduction_param, reduction_output) = match &reduction {
                        DSLType::Unknown => {
                            Ok((DSLType::Unknown, DSLType::Unknown))
                        }

                        DSLType::Function(FunctionType { params, output }) => {
                            let first_param = if let Some(params) = params {
                                if params.len() == 2 {
                                    Ok(params[0].clone())
                                } else {
                                    Err(TypeInferenceError::ReductionMustTakeTwoArguments(
                                        reduction.clone(),
                                    ))
                                }
                            } else {
                                Ok(DSLType::Unknown)
                            }?;

                            Ok((first_param, output.as_ref().clone()))
                        }

                        other => {
                            Err(TypeInferenceError::ReductionMustBeAFunction(
                                other.clone(),
                            ))
                        }
                    }?;

                    if initial != DSLType::Unknown {
                        initial
                    } else if reduction_param != DSLType::Unknown {
                        reduction_param
                    } else if reduction_output != DSLType::Unknown {
                        reduction_output
                    } else {
                        DSLType::Unknown
                    }
                }
                ExprKind::NativeFunction(func) => func.signature()?,
                ExprKind::Tuple(elements) => {
                    let elements = elements
                        .iter()
                        .map(|element| self.expect_cache(*element))
                        .cloned()
                        .collect();
                    TupleType(elements).into()
                }
                ExprKind::StaticField(static_field) => {
                    let reader = self.reader()?;
                    static_field.runtime_type(reader)?
                }
                ExprKind::FieldAccess { obj, field } => {
                    let obj_type = self.expect_cache(*obj);

                    match obj_type {
                        DSLType::Unknown => DSLType::Unknown,
                        other => {
                            let method_table_ptr = other
                                .method_table_for_field_access(|| {
                                    format!("{}", graph.print(*obj))
                                })?;
                            let field_type =
                                self.reader()?.field_by_name_to_runtime_type(
                                    method_table_ptr,
                                    field.as_str(),
                                )?;
                            field_type.into()
                        }
                    }
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
                    let array_type = self.expect_cache(*array);
                    let num_indices = indices.len();

                    match array_type {
                        DSLType::Unknown => Ok(DSLType::Unknown),

                        DSLType::DotNet(DotNetType::Array { .. })
                            if num_indices != 1 =>
                        {
                            Err(TypeInferenceError::IncorrectNumberOfIndices {
                                num_provided: num_indices,
                                num_expected: 1,
                            })
                        }
                        DSLType::DotNet(DotNetType::MultiDimArray {
                            rank,
                            ..
                        }) if num_indices != *rank => {
                            return Err(
                                TypeInferenceError::IncorrectNumberOfIndices {
                                    num_provided: num_indices,
                                    num_expected: *rank,
                                },
                            );
                        }
                        DSLType::DotNet(
                            DotNetType::MultiDimArray {
                                method_table: None,
                                ..
                            }
                            | DotNetType::Array {
                                method_table: None, ..
                            },
                        ) => {
                            return Err(
                                TypeInferenceError::UnexpectedNullMethodTable(
                                    format!("{}", graph.print(*array)),
                                ),
                            );
                        }
                        DSLType::DotNet(
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
                            let opt_element_type = method_table
                                .array_element_type()
                                .ok_or(
                                    TypeInferenceError::ArrayMissingElementType,
                                )?
                                .as_method_table()
                                .map(|ptr| reader.runtime_type(ptr))
                                .transpose()?;

                            Ok(opt_element_type
                                .unwrap_or(RuntimeType::Unknown)
                                .into())
                        }

                        other => {
                            Err(TypeInferenceError::IndexAccessRequiresArray(
                                other.clone(),
                            ))
                        }
                    }?
                }
                ExprKind::ArrayExtent { .. }
                | ExprKind::NumArrayElements { .. } => {
                    DSLType::Prim(RuntimePrimType::NativeUInt)
                }
                ExprKind::PointerCast { ty, .. } => ty.clone(),
                ExprKind::IsSome(_) => DSLType::Prim(RuntimePrimType::Bool),

                ExprKind::IfElse {
                    if_branch,
                    else_branch,
                    ..
                } => {
                    let if_branch = self.expect_cache(*if_branch);
                    let else_branch = self.expect_cache(*else_branch);

                    match (if_branch, else_branch) {
                        (ty, DSLType::Unknown) | (DSLType::Unknown, ty) => {
                            ty.clone()
                        }
                        (ty, _) => ty.clone(),
                    }
                }

                ExprKind::And { .. }
                | ExprKind::Or { .. }
                | ExprKind::Not { .. }
                | ExprKind::Equal { .. }
                | ExprKind::NotEqual { .. }
                | ExprKind::LessThan { .. }
                | ExprKind::GreaterThan { .. }
                | ExprKind::LessThanOrEqual { .. }
                | ExprKind::GreaterThanOrEqual { .. } => {
                    DSLType::Prim(RuntimePrimType::Bool)
                }

                ExprKind::Add { lhs, rhs } => {
                    self.infer_add(expr_kind, *lhs, *rhs)?
                }
                ExprKind::Sub { lhs, rhs } => {
                    self.infer_sub(expr_kind, *lhs, *rhs)?
                }
                ExprKind::Mul { lhs, rhs } => {
                    self.infer_mul(expr_kind, *lhs, *rhs)?
                }
                ExprKind::Div { lhs, rhs } => {
                    self.infer_div(expr_kind, *lhs, *rhs)?
                }
                ExprKind::Mod { lhs, rhs } => {
                    self.infer_mod(expr_kind, *lhs, *rhs)?
                }

                ExprKind::PrimCast { prim_type, .. } => (*prim_type).into(),
                ExprKind::IsSubclassOf { .. } => RuntimePrimType::Bool.into(),

                ExprKind::ReadPrim { ptr, prim_type } => {
                    let ptr_type = self.expect_cache(*ptr);
                    match ptr_type {
                        DSLType::Unknown => Ok(()),
                        DSLType::Prim(RuntimePrimType::Ptr) => Ok(()),
                        other => {
                            Err(TypeInferenceError::InvalidOperandForReadValue(
                                other.clone(),
                            ))
                        }
                    }?;
                    (*prim_type).into()
                }
                ExprKind::ReadBytes { .. } => DSLType::ByteArray,
                ExprKind::CastBytes { prim_type, .. } => (*prim_type).into(),
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

    infer_binary_op! {
        infer_add,
        (Ptr,NativeUInt) => Ptr,
        (NativeUInt,Ptr) => Ptr,
        (NativeUInt,NativeUInt) => NativeUInt,
        (I32,I32) => I32,
        (F32,F32) => F32,
        (F64,F64) => F64,
    }
    infer_binary_op! {
        infer_sub,
        (Ptr,NativeUInt) => Ptr,
        (Ptr,Ptr) => NativeUInt,
        (NativeUInt,NativeUInt) => NativeUInt,
        (I32,I32) => I32,
        (F32,F32) => F32,
        (F64,F64) => F64,
    }
    infer_binary_op! {
        infer_mul,
        (NativeUInt,NativeUInt) => NativeUInt,
        (I32,I32) => I32,
        (F32,F32) => F32,
        (F64,F64) => F64,
    }
    infer_binary_op! {
        infer_div,
        (NativeUInt,NativeUInt) => NativeUInt,
        (I32,I32) => I32,
        (I32,NativeUInt) => I32,
        (F32,F32) => F32,
        (F32,NativeUInt) => F32,
        (F64,F64) => F64,
    }
    infer_binary_op! {
        infer_mod,
        (NativeUInt,NativeUInt) => NativeUInt,
    }
}

impl std::fmt::Debug for TypeInferenceError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
