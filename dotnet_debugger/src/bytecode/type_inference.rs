use std::collections::HashSet;

use elsa::FrozenMap;

use crate::{runtime_type::RuntimePrimType, CachedReader, Error, RuntimeType};

use super::{OpIndex, SymbolicExpr, SymbolicGraph, SymbolicValue};

pub struct TypeInference<'a> {
    reader: CachedReader<'a>,
    cache: FrozenMap<OpIndex, Box<RuntimeType>>,
}

impl<'a> TypeInference<'a> {
    pub fn new(reader: CachedReader<'a>) -> Self {
        Self {
            reader,
            cache: Default::default(),
        }
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

            let inferred_type = match &graph[index_to_infer] {
                SymbolicExpr::StaticField(static_field) => {
                    static_field.runtime_type(self.reader)?
                }
                SymbolicExpr::FieldAccess { obj, field } => {
                    let obj_type = expect_cache(*obj);

                    let method_table_ptr = obj_type
                        .method_table_for_field_access(|| {
                            format!("{}", graph.print(*obj))
                        })?;
                    let field_type =
                        self.reader.field_by_name_to_runtime_type(
                            method_table_ptr,
                            field.as_str(),
                        )?;
                    field_type
                }
                SymbolicExpr::SymbolicDowncast { ty, .. } => {
                    let method_table = ty.method_table(self.reader)?;
                    RuntimeType::Class {
                        method_table: Some(method_table),
                    }
                }
                SymbolicExpr::IndexAccess {
                    obj: array,
                    indices,
                } => {
                    let array_type = expect_cache(*array);
                    let num_indices = indices.len();

                    match array_type {
                        RuntimeType::Array { .. } if num_indices != 1 => {
                            Err(Error::IncorrectNumberOfIndices {
                                num_provided: num_indices,
                                num_expected: 1,
                            })
                        }
                        RuntimeType::MultiDimArray { rank, .. }
                            if num_indices != *rank =>
                        {
                            return Err(Error::IncorrectNumberOfIndices {
                                num_provided: num_indices,
                                num_expected: *rank,
                            });
                        }
                        RuntimeType::MultiDimArray {
                            method_table: None,
                            ..
                        }
                        | RuntimeType::Array {
                            method_table: None, ..
                        } => {
                            return Err(Error::UnexpectedNullMethodTable(
                                format!("{}", graph.print(*array)),
                            ));
                        }
                        RuntimeType::MultiDimArray {
                            method_table: Some(ptr),
                            ..
                        }
                        | RuntimeType::Array {
                            method_table: Some(ptr),
                            ..
                        } => {
                            let method_table =
                                self.reader.method_table(*ptr)?;
                            method_table
                                .array_element_type()
                                .ok_or(Error::ArrayMissingElementType)
                                .and_then(|ptr| self.reader.runtime_type(ptr))
                        }

                        other => {
                            Err(Error::IndexAccessRequiresArray(other.clone()))
                        }
                    }?
                }
                SymbolicExpr::ArrayExtent { .. }
                | SymbolicExpr::NumArrayElements { .. } => {
                    RuntimeType::Prim(RuntimePrimType::NativeUInt)
                }
                SymbolicExpr::PointerCast { ty, .. } => ty.clone(),
                SymbolicExpr::Add { lhs, rhs } => {
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
                SymbolicExpr::Mul { lhs, rhs } => {
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
                SymbolicExpr::PrimCast { prim_type, .. } => (*prim_type).into(),
                SymbolicExpr::PhysicalDowncast { obj, .. } => {
                    let obj_type = expect_cache(*obj);
                    match obj_type {
                        RuntimeType::Prim(RuntimePrimType::Ptr) => Ok(()),
                        other => Err(Error::InvalidOperandForPhysicalDowncast(
                            other.clone(),
                        )),
                    }?;
                    RuntimePrimType::Ptr.into()
                }
                SymbolicExpr::ReadValue { ptr, prim_type } => {
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
