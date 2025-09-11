use dll_unpacker::RelativeVirtualAddress;
use env_var_flag::env_var_flag;
use memory_reader::Pointer;

use dotnet_debugger::{
    DotNetType, RuntimeArray, RuntimeMultiDimArray, RuntimePrimType,
    RuntimeType, TypeHandlePtrExt as _,
};

use crate::{
    graph_rewrite::Analysis, DSLType, Error, ExprKind, GraphRewrite,
    SymbolicGraph, SymbolicValue,
};

static USE_PHYSICAL_DOWNCAST: std::sync::LazyLock<bool> =
    std::sync::LazyLock::new(|| env_var_flag("USE_PHYSICAL_DOWNCAST"));

pub struct LowerSymbolicExpr<'a>(pub &'a Analysis<'a>);

impl<'a> GraphRewrite for LowerSymbolicExpr<'a> {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        name: Option<&str>,
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
                    graph.pointer_cast(ptr, $runtime_type.into())
                } else {
                    ptr
                };
                ptr
            }};
        }

        let opt_value = match expr {
            ExprKind::StaticField(static_field) => {
                let reader = self.0.reader()?;

                let (base_method_table_ptr, field_desc) =
                    static_field.method_table_and_field(reader)?;

                if !field_desc.is_static() {
                    return Err(
                        Error::ExpectedStaticFieldButFoundInstanceField {
                            class: format!("{}", static_field.class),
                            field: static_field.field_name.clone(),
                        },
                    );
                }

                let runtime_type = reader.field_to_runtime_type(
                    base_method_table_ptr,
                    &field_desc,
                )?;

                let method_table =
                    reader.method_table(base_method_table_ptr)?;
                let module = reader.runtime_module(method_table.module())?;

                let ptr: SymbolicValue =
                    match (field_desc.is_rva(), &runtime_type) {
                        (true, _) => {
                            let layout = module.metadata_layout(&reader)?;
                            let rva = RelativeVirtualAddress::new(
                                field_desc.offset(),
                            );
                            let ptr: Pointer =
                                layout.virtual_address_to_raw(rva)?;
                            ptr.into()
                        }
                        (_, RuntimeType::Prim(_)) => {
                            let base =
                                module.base_ptr_of_non_gc_statics(reader)?;
                            graph.add(base, field_desc.offset())
                        }
                        (
                            _,
                            RuntimeType::DotNet(DotNetType::ValueType {
                                ..
                            }),
                        ) => {
                            // Static value types are not stored inline, but are
                            // instead stored as if they were classes.  Need to read
                            // the pointer, dereference, then advance past a method
                            // table pointer.
                            let base: Pointer =
                                module.base_ptr_of_gc_statics(reader)?;
                            let ptr_loc = graph.add(base, field_desc.offset());
                            let ptr =
                                graph.read_value(ptr_loc, RuntimePrimType::Ptr);
                            graph.add(ptr, Pointer::SIZE)
                        }
                        (_, RuntimeType::DotNet(_)) => {
                            let base = module.base_ptr_of_gc_statics(reader)?;
                            graph.add(base, field_desc.offset())
                        }

                        (_, RuntimeType::Unknown) => unreachable!(
                            "Static .NET field can only be inferred \
                             as a primitive or a .NET type."
                        ),
                    };

                let expr = read_value_if_required!(ptr, runtime_type);

                Some(expr)
            }
            ExprKind::FieldAccess { obj, field } => {
                let obj = *obj;
                let obj_type = self.0.infer_type(graph, obj)?;

                if matches!(obj_type, DSLType::Unknown) {
                    return Ok(None);
                }

                let method_table_ptr =
                    obj_type.method_table_for_field_access(|| {
                        format!(
                            "{}, \
                             of type {obj_type}, \
                             for access of field {field}",
                            graph.print(obj),
                        )
                    })?;
                let reader = self.0.reader()?;
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
                let ptr = if matches!(
                    obj_type,
                    DSLType::DotNet(DotNetType::Class { .. })
                ) {
                    graph.add(ptr, Pointer::SIZE)
                } else {
                    ptr
                };

                let ptr = graph.add(ptr, field_description.offset());
                if let Some(name) = name {
                    graph.name(ptr, format!("member_ptr_{field}_of_{name}"))?;
                } else {
                    graph.name(ptr, format!("member_ptr_{field}"))?;
                }

                let value = read_value_if_required!(ptr, field_type);

                Some(value)
            }
            ExprKind::IndexAccess { obj, indices } => {
                let array = *obj;
                let array_type = self.0.infer_type(graph, array)?;

                let (element_type, component_size) = match array_type {
                    DSLType::DotNet(
                        DotNetType::Array { method_table, .. }
                        | DotNetType::MultiDimArray { method_table, .. },
                    ) => {
                        let method_table = method_table.ok_or_else(|| {
                            Error::UnexpectedNullMethodTable(format!(
                                "{}",
                                graph.print(*obj)
                            ))
                        })?;
                        let reader = self.0.reader()?;
                        let method_table = reader.method_table(method_table)?;
                        let component_size = method_table
                            .component_size()
                            .ok_or(Error::ArrayMissingComponentSize)?;
                        let opt_element_type = method_table
                            .array_element_type()
                            .ok_or(Error::ArrayMissingElementType)?
                            .as_method_table()
                            .map(|ptr| reader.runtime_type(ptr))
                            .transpose()?;

                        let Some(element_type) = opt_element_type else {
                            return Ok(None);
                        };

                        (element_type, component_size)
                    }
                    _ => {
                        return Ok(None);
                    }
                };

                let (header_size_bytes, shape) = match array_type {
                    DSLType::DotNet(DotNetType::Array { .. }) => {
                        let array_ptr =
                            graph.prim_cast(array, RuntimePrimType::Ptr);

                        let header_size_bytes = RuntimeArray::HEADER_SIZE;
                        let num_elements_ptr =
                            graph.add(array_ptr, Pointer::SIZE);
                        let num_elements = graph
                            .read_value(num_elements_ptr, RuntimePrimType::U64);
                        let num_elements = graph.prim_cast(
                            num_elements,
                            RuntimePrimType::NativeUInt,
                        );
                        let shape = vec![num_elements];
                        (header_size_bytes, shape)
                    }
                    DSLType::DotNet(DotNetType::MultiDimArray {
                        rank, ..
                    }) => {
                        let rank = *rank;

                        let array_ptr =
                            graph.prim_cast(array, RuntimePrimType::Ptr);
                        let shape_start =
                            graph.add(array_ptr, RuntimeArray::HEADER_SIZE);
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

                        (header_size_bytes, shape)
                    }
                    _ => {
                        return Ok(None);
                    }
                };

                if shape.len() != indices.len() {
                    return Err(Error::IncorrectNumberOfIndices {
                        num_provided: indices.len(),
                        num_expected: shape.len(),
                    });
                }

                let ptr = {
                    let array_ptr =
                        graph.prim_cast(array, RuntimePrimType::Ptr);
                    let first_element = graph.add(array_ptr, header_size_bytes);
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

                        let mut total_offset: SymbolicValue = 0usize.into();
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
            ExprKind::SymbolicDowncast { obj, ty } => {
                let obj = *obj;
                let obj_type = self.0.infer_type(graph, obj)?;

                if matches!(obj_type, DSLType::Unknown) {
                    return Ok(None);
                }

                let static_method_table_ptr =
                    obj_type.method_table_for_downcast()?;
                let reader = self.0.reader()?;
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

                let ptr = if *USE_PHYSICAL_DOWNCAST {
                    graph.physical_downcast(ptr, target_method_table_ptr)
                } else {
                    let method_table_ptr =
                        graph.read_value(ptr, RuntimePrimType::Ptr);
                    let is_target_type = graph.is_subclass_of(
                        method_table_ptr,
                        target_method_table_ptr,
                    );
                    let none = graph.none();
                    graph.if_else(is_target_type, ptr, none)
                };

                let expr = graph.pointer_cast(
                    ptr,
                    DotNetType::Class {
                        method_table: Some(target_method_table_ptr),
                    }
                    .into(),
                );

                Some(expr)
            }
            ExprKind::NumArrayElements { array } => {
                let array = *array;
                let array_type = self.0.infer_type(graph, array)?;
                let expr = match array_type {
                    DSLType::DotNet(
                        DotNetType::Array { .. }
                        | DotNetType::MultiDimArray { .. },
                    ) => {
                        let ptr = graph.prim_cast(array, RuntimePrimType::Ptr);
                        let num_elements_ptr = graph.add(ptr, Pointer::SIZE);
                        let expr = graph
                            .read_value(num_elements_ptr, RuntimePrimType::U64);

                        graph.prim_cast(expr, RuntimePrimType::NativeUInt)
                    }

                    _ => {
                        return Ok(None);
                    }
                };

                Some(expr)
            }
            ExprKind::ArrayExtent { array, dim } => {
                let array = *array;
                let dim = *dim;
                let array_type = self.0.infer_type(graph, array)?;

                let expr = match array_type {
                    DSLType::DotNet(DotNetType::MultiDimArray { .. }) => {
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
