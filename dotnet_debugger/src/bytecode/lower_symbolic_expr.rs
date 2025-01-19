use memory_reader::Pointer;

use crate::{
    runtime_type::RuntimePrimType, Error, RuntimeArray, RuntimeMultiDimArray,
    RuntimeType,
};

use super::{
    graph_rewrite::Analysis, GraphRewrite, SymbolicExpr, SymbolicGraph,
    SymbolicValue,
};

pub struct LowerSymbolicExpr<'a>(pub &'a Analysis<'a>);

impl<'a> GraphRewrite for LowerSymbolicExpr<'a> {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &SymbolicExpr,
    ) -> Result<Option<SymbolicValue>, crate::Error> {
        macro_rules! read_value_if_required {
            ($ptr:expr, $runtime_type:expr) => {{
                let ptr = if let Some(prim_type) = $runtime_type.storage_type()
                {
                    // The majority of fields should be read out after
                    // their location has been determined.
                    graph.read_value($ptr, prim_type)
                } else {
                    // The exception are ValueType fields.  These
                    // require additional FieldAccess operations to
                    // locate the primitive types within the composite
                    // ValueType, and must be kept as a pointer until
                    // then.
                    $ptr
                };
                let ptr = if !matches!($runtime_type, RuntimeType::Prim(_)) {
                    graph.pointer_cast(ptr, $runtime_type)
                } else {
                    ptr
                };
                ptr
            }};
        }

        let reader = self.0.reader();

        let opt_value = match expr {
            SymbolicExpr::StaticField(static_field) => {
                let runtime_type = static_field.runtime_type(reader)?;
                let ptr = static_field.location(reader)?;

                let ptr = SymbolicValue::Ptr(ptr);
                let expr = read_value_if_required!(ptr, runtime_type);

                Some(expr)
            }
            SymbolicExpr::FieldAccess { obj, field } => {
                let obj = *obj;
                let obj_type = self.0.infer_type(graph, obj)?;

                let method_table_ptr =
                    obj_type.method_table_for_field_access(|| {
                        format!("{}", graph.print(&obj))
                    })?;
                let (parent_of_field, field_description) = reader
                    .find_field_by_name(method_table_ptr, field.as_str())?;
                let field_type = reader.field_to_runtime_type(
                    parent_of_field,
                    &field_description,
                )?;

                // The `field_description.offset()` is relative to the
                // location of the first data member, regardless of
                // whether the object is a Class or ValueType
                // instance.  However, Class instances have an
                // additional pointer to their method table, prior to
                // the first data member.
                let ptr = graph.prim_cast(obj, RuntimePrimType::Ptr);
                let ptr = if matches!(obj_type, RuntimeType::Class { .. }) {
                    graph.add(ptr, Pointer::SIZE)
                } else {
                    ptr
                };

                let ptr = graph.add(ptr, field_description.offset());
                let value = read_value_if_required!(ptr, field_type);

                Some(value)
            }
            SymbolicExpr::IndexAccess { obj, indices } => {
                let array = *obj;
                let array_type = self.0.infer_type(graph, array)?;

                let (element_type, component_size) = match array_type {
                    RuntimeType::Array { method_table, .. }
                    | RuntimeType::MultiDimArray { method_table, .. } => {
                        let method_table = method_table.ok_or_else(|| {
                            Error::UnexpectedNullMethodTable(format!(
                                "{}",
                                graph.print(obj)
                            ))
                        })?;
                        let method_table = reader.method_table(method_table)?;
                        let component_size = method_table
                            .component_size()
                            .ok_or(Error::ArrayMissingComponentSize)?;
                        let element_type = method_table
                            .array_element_type()
                            .ok_or(Error::ArrayMissingElementType)
                            .and_then(|ptr| reader.runtime_type(ptr))?;

                        Ok((element_type, component_size))
                    }
                    other => {
                        Err(Error::IndexAccessRequiresArray(other.clone()))
                    }
                }?;

                let (header_size_bytes, shape) = match array_type {
                    RuntimeType::Array { .. } => {
                        let header_size_bytes = RuntimeArray::HEADER_SIZE;
                        let num_elements_ptr = graph.add(array, Pointer::SIZE);
                        let num_elements = graph
                            .read_value(num_elements_ptr, RuntimePrimType::U64);
                        let num_elements = graph.prim_cast(
                            num_elements,
                            RuntimePrimType::NativeUInt,
                        );
                        let shape = vec![num_elements];
                        Ok((header_size_bytes, shape))
                    }
                    RuntimeType::MultiDimArray { rank, .. } => {
                        let rank = *rank;

                        let shape_start =
                            graph.add(array, RuntimeArray::HEADER_SIZE);
                        let shape = (0..rank)
                            .map(|i| {
                                let extent_ptr = graph.add(
                                    shape_start,
                                    i * RuntimePrimType::U32.size_bytes(),
                                );
                                let extent = graph.read_value(
                                    extent_ptr,
                                    RuntimePrimType::U32,
                                );
                                graph.prim_cast(
                                    extent,
                                    RuntimePrimType::NativeUInt,
                                )
                            })
                            .collect();
                        let header_size_bytes =
                            RuntimeMultiDimArray::header_size(rank);

                        Ok((header_size_bytes, shape))
                    }
                    other => {
                        Err(Error::IndexAccessRequiresArray(other.clone()))
                    }
                }?;

                if shape.len() != indices.len() {
                    return Err(Error::IncorrectNumberOfIndices {
                        num_provided: indices.len(),
                        num_expected: shape.len(),
                    });
                }

                let ptr = {
                    let ptr = graph.prim_cast(array, RuntimePrimType::Ptr);
                    let first_element = graph.add(ptr, header_size_bytes);
                    let byte_offset = {
                        let strides = {
                            let mut strides = Vec::new();
                            let mut cum_prod: SymbolicValue =
                                component_size.into();
                            for dim in shape.into_iter().rev() {
                                strides.push(cum_prod);
                                cum_prod = graph.mul(cum_prod, dim);
                            }
                            strides.reverse();
                            strides
                        };

                        let mut total_offset: SymbolicValue = 0.into();
                        for (stride, index) in
                            strides.into_iter().zip(indices.iter().cloned())
                        {
                            let axis_offset = graph.mul(stride, index);
                            total_offset = graph.add(total_offset, axis_offset);
                        }
                        total_offset
                    };
                    graph.add(first_element, byte_offset)
                };

                let value = read_value_if_required!(ptr, element_type);

                Some(value)
            }
            SymbolicExpr::SymbolicDowncast { obj, ty } => {
                let obj = *obj;
                let obj_type = self.0.infer_type(graph, obj)?;

                let static_method_table_ptr =
                    obj_type.method_table_for_downcast()?;
                let target_method_table_ptr = ty.method_table(reader)?;
                let is_valid_downcast = reader
                    .method_table(static_method_table_ptr)?
                    .is_interface()
                    || reader.is_base_of(
                        target_method_table_ptr,
                        static_method_table_ptr,
                    )?
                    || reader.is_base_of(
                        static_method_table_ptr,
                        target_method_table_ptr,
                    )?;
                if !is_valid_downcast {
                    // Types are in separate hierachies.  This
                    // downcast is illegal.
                    return Err(Error::DowncastRequiresRelatedClasses(
                        format!("{obj_type}"),
                        format!("{ty}"),
                    ));
                }

                let target_method_table_ptr = ty.method_table(reader)?;
                let ptr = graph.prim_cast(obj, RuntimePrimType::Ptr);
                let ptr = graph.physical_downcast(ptr, target_method_table_ptr);
                let expr = graph.pointer_cast(
                    ptr,
                    RuntimeType::Class {
                        method_table: Some(target_method_table_ptr),
                    },
                );

                Some(expr)
            }
            SymbolicExpr::NumArrayElements { array } => {
                let array = *array;
                let array_type = self.0.infer_type(graph, array)?;
                let expr = match array_type {
                    RuntimeType::Array { .. }
                    | RuntimeType::MultiDimArray { .. } => {
                        let ptr = graph.prim_cast(array, RuntimePrimType::Ptr);
                        let num_elements_ptr = graph.add(ptr, Pointer::SIZE);
                        let expr = graph
                            .read_value(num_elements_ptr, RuntimePrimType::U64);
                        let expr =
                            graph.prim_cast(expr, RuntimePrimType::NativeUInt);
                        Ok(expr)
                    }

                    other => {
                        Err(Error::ArrayLengthRequiresArray(other.clone()))
                    }
                }?;

                Some(expr)
            }
            SymbolicExpr::ArrayExtent { array, dim } => {
                let array = *array;
                let dim = *dim;
                let array_type = self.0.infer_type(graph, array)?;

                let expr = match array_type {
                    RuntimeType::MultiDimArray { .. } => {
                        // TODO: Assert that `dim < rank`.  Will
                        // require implementing support for runtime
                        // assertions.
                        let dim_offset =
                            graph.mul(dim, RuntimePrimType::U32.size_bytes());
                        let offset =
                            graph.add(dim_offset, RuntimeArray::HEADER_SIZE);

                        let array_ptr =
                            graph.prim_cast(array, RuntimePrimType::Ptr);
                        let extent_ptr = graph.add(array_ptr, offset);
                        let extent =
                            graph.read_value(extent_ptr, RuntimePrimType::U32);
                        let extent = graph
                            .prim_cast(extent, RuntimePrimType::NativeUInt);
                        Ok(extent)
                    }
                    other => {
                        Err(Error::ArrayExtentRequiresMultiDimensionalArray(
                            other.clone(),
                        ))
                    }
                }?;

                Some(expr)
            }

            _ => None,
        };

        Ok(opt_value)
    }
}
