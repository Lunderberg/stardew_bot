use std::collections::HashSet;

use elsa::FrozenMap;
use thiserror::Error;

use crate::{
    runtime_type::{DotNetType, FunctionType, RuntimePrimType, TupleType},
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
                    expr.visit_input_values(|input_value| {
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
            let expect_cache = |value: SymbolicValue| -> &RuntimeType {
                match value {
                    SymbolicValue::Int(_) => {
                        &RuntimeType::Prim(RuntimePrimType::NativeUInt)
                    }
                    SymbolicValue::Ptr(_) => {
                        &RuntimeType::Prim(RuntimePrimType::Ptr)
                    }
                    SymbolicValue::Result(op_index) => {
                        self.cache.get(&op_index).expect(
                            "Internal error: \
                             Topologic sort should ensure that \
                             all input expressions have their type inferred.",
                        )
                    }
                }
            };

            let inferred_type = match graph[index_to_infer].as_ref() {
                ExprKind::Function { params, output } => {
                    let params = params
                        .iter()
                        .map(|param| expect_cache(*param))
                        .cloned()
                        .collect();
                    let output = Box::new(expect_cache(*output).clone());
                    FunctionType { params, output }.into()
                }
                ExprKind::FunctionArg(ty) => ty.clone(),
                ExprKind::Tuple(elements) => {
                    let elements = elements
                        .iter()
                        .map(|element| expect_cache(*element))
                        .cloned()
                        .collect();
                    TupleType(elements).into()
                }
                ExprKind::StaticField(static_field) => {
                    let reader = self.reader()?;
                    static_field.runtime_type(reader)?
                }
                ExprKind::FieldAccess { obj, field } => {
                    let obj_type = expect_cache(*obj);

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
                    let array_type = expect_cache(*array);
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
                ExprKind::Add { lhs, rhs } => {
                    let lhs_type = expect_cache(*lhs);
                    let rhs_type = expect_cache(*rhs);
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
                            Ok(RuntimePrimType::Ptr)
                        }

                        (
                            RuntimeType::Prim(RuntimePrimType::NativeUInt),
                            RuntimeType::Prim(RuntimePrimType::NativeUInt),
                        ) => Ok(RuntimePrimType::NativeUInt),

                        (other_lhs, other_rhs) => {
                            Err(Error::InvalidOperandsForAddition {
                                lhs: other_lhs.clone(),
                                rhs: other_rhs.clone(),
                            })
                        }
                    }?
                    .into()
                }
                ExprKind::Mul { lhs, rhs } => {
                    let lhs_type = expect_cache(*lhs);
                    let rhs_type = expect_cache(*rhs);
                    match (lhs_type, rhs_type) {
                        (
                            RuntimeType::Prim(RuntimePrimType::NativeUInt),
                            RuntimeType::Prim(RuntimePrimType::NativeUInt),
                        ) => Ok(RuntimePrimType::NativeUInt),
                        (other_lhs, other_rhs) => {
                            Err(Error::InvalidOperandsForMultiplication {
                                lhs: other_lhs.clone(),
                                rhs: other_rhs.clone(),
                            })
                        }
                    }?
                    .into()
                }
                ExprKind::PrimCast { prim_type, .. } => (*prim_type).into(),
                ExprKind::PhysicalDowncast { obj, .. } => {
                    let obj_type = expect_cache(*obj);
                    match obj_type {
                        RuntimeType::Prim(RuntimePrimType::Ptr) => Ok(()),
                        other => Err(Error::InvalidOperandForPhysicalDowncast(
                            other.clone(),
                        )),
                    }?;
                    RuntimePrimType::Ptr.into()
                }
                ExprKind::ReadValue { ptr, prim_type } => {
                    let ptr_type = expect_cache(*ptr);
                    match ptr_type {
                        RuntimeType::Prim(RuntimePrimType::Ptr) => Ok(()),
                        other => Err(Error::InvalidOperandForReadValue(
                            other.clone(),
                        )),
                    }?;
                    (*prim_type).into()
                }
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
