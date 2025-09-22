use dll_unpacker::{RelativeVirtualAddress, SignatureType};
use dotnet_debugger::{
    DotNetType, MethodTable, RuntimeArray, RuntimeMultiDimArray,
    RuntimePrimType, RuntimeType, SignatureTypeExt as _,
};
use dsl_analysis::{Analysis, StaticFieldExt as _};
use dsl_ir::{
    DSLType, ExprKind, RuntimePrimValue, SymbolicGraph, SymbolicValue,
    TypedPointer,
};
use dsl_ir::{Pointer, SymbolicType};
use dsl_rewrite_utils::GraphRewrite;

use crate::Error;

pub struct LowerSymbolicExpr<'a: 'b, 'b>(pub &'b Analysis<'a>);

impl<'a: 'b, 'b> LowerSymbolicExpr<'a, 'b> {
    fn read_value_if_required(
        graph: &mut SymbolicGraph,
        ptr: SymbolicValue,
        runtime_type: RuntimeType,
    ) -> SymbolicValue {
        let ptr = if let Some(prim_type) = runtime_type.storage_type() {
            // The majority of fields should be read out after
            // their location has been determined.
            graph.read_value(ptr, prim_type)
        } else {
            // The exception are ValueType fields.  These
            // require additional FieldAccess operations to
            // locate the primitive types within the composite
            // ValueType, and must be kept as a pointer until
            // then.
            ptr
        };
        let ptr = if matches!(runtime_type, RuntimeType::Prim(_)) {
            ptr
        } else {
            graph.pointer_cast(ptr, runtime_type.into())
        };
        ptr
    }

    fn access_field_value(
        &self,
        graph: &mut SymbolicGraph,
        ptr: SymbolicValue,
        sig: &SignatureType<'_>,
    ) -> Result<SymbolicValue, Error> {
        let field_dsl_type: DSLType = self
            .0
            .reader()?
            .signature_type_to_runtime_type(&sig, &[])?
            .into();
        field_dsl_type.validate().unwrap();

        let value = match sig {
            SignatureType::Prim(prim) => graph.read_value(ptr, (*prim).into()),
            SignatureType::GenericInst {
                is_value_type: true,
                ..
            }
            | SignatureType::ValueType { .. } => {
                match field_dsl_type {
                    DSLType::Prim(prim) => {
                        // A System.Enum gets automatically
                        // unwrapped into its backing type.
                        graph.read_value(ptr, prim.into())
                    }
                    other => {
                        // Everything else gets exposed as a
                        // pointer to the ValueType, with
                        // further ExprKind::FieldAccess
                        // operations used to access
                        // individual fields of the ValueType.
                        graph.pointer_cast(ptr, other)
                    }
                }
            }
            _ => {
                let member = graph.read_value(ptr, RuntimePrimType::Ptr);
                graph.pointer_cast(member, field_dsl_type.into())
            }
        };
        Ok(value)
    }
}

impl GraphRewrite for LowerSymbolicExpr<'_, '_> {
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, crate::Error> {
        let opt_value = match expr {
            ExprKind::StaticField(static_field) => {
                self.0.infer_expr_sig(graph, expr).unwrap();

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

                let expr =
                    Self::read_value_if_required(graph, ptr, runtime_type);

                Some(expr)
            }

            ExprKind::TypeToMethodTable { ty } => {
                let sig = self.0.symbolic_to_signature(ty)?;
                self.0
                    .reader()?
                    .signature_to_method_table(&sig)?
                    .map(|ptr| ptr.as_untyped_ptr().into())
                    .filter(|_| {
                        let delay = env_var_flag::env_var_flag("SIMULATE_FRESH") && {
                            let name = format!("{ty}");
                            ["Stardew Valley.dll::TypeDef[156][]",
                             "Stardew Valley.dll::TypeDef[46]<Stardew Valley.dll::TypeDef[137], Stardew Valley.dll::TypeDef[90]<Stardew Valley.dll::TypeDef[137]>>",
                             "Stardew Valley.dll::TypeDef[60]<Stardew Valley.dll::TypeDef[137], Stardew Valley.dll::TypeDef[90]<Stardew Valley.dll::TypeDef[137]>>",
                             "Stardew Valley.dll::TypeDef[639][]",
                             "System.Private.CoreLib.dll::TypeDef[2150]<Stardew Valley.dll::TypeDef[156]>",
                             "System.Private.CoreLib.dll::TypeDef[2150]<Stardew Valley.dll::TypeDef[639]>",
                             "System.Private.CoreLib.dll::TypeDef[2150]<Stardew Valley.dll::TypeDef[90]<Stardew Valley.dll::TypeDef[137]>>",
                            ].into_iter().any(|skip_name| name == skip_name)
                        };

                        !delay
                    })
            }

            ExprKind::FieldOffset {
                method_table_ptr: SymbolicValue::Const(value),
                field: field_name,
            } => {
                let ptr: TypedPointer<MethodTable> = match value {
                    RuntimePrimValue::Ptr(ptr) => Ok(ptr),
                    other => Err(Error::MethodTableShouldBePointer(*other)),
                }?
                .clone()
                .into();

                let offset: usize = self
                    .0
                    .reader()?
                    .find_field_by_name(ptr, field_name)?
                    .1
                    .offset();
                Some(offset.into())
            }

            ExprKind::ArrayStride {
                method_table_ptr: SymbolicValue::Const(value),
            } => {
                let ptr: Pointer = (*value).try_into()?;

                let reader = self.0.reader()?;
                let method_table = reader.method_table(ptr.into())?;
                let array_stride = method_table
                    .component_size()
                    .ok_or_else(|| Error::ArrayMissingComponentSize)?;
                Some(array_stride.into())
            }

            ExprKind::ObjectMethodTable { obj } => {
                let obj = *obj;
                let obj_type = self.0.infer_type(graph, obj)?;

                match obj_type {
                    // A ValueType doesn't contain a method table
                    // pointer, so its method table pointer may be
                    // determined at compile-time.
                    DSLType::DotNet(DotNetType::ValueType {
                        method_table: Some(ptr),
                        ..
                    }) => Some(ptr.as_untyped_ptr().into()),

                    // For types that hold a pointer to their method
                    // table, must read that pointer at runtime.
                    // Using the statically-known method table pointer would
                    DSLType::DotNet(
                        DotNetType::Class { .. }
                        | DotNetType::Array { .. }
                        | DotNetType::MultiDimArray { .. }
                        | DotNetType::String,
                    ) => {
                        let ptr = graph.prim_cast(obj, RuntimePrimType::Ptr);
                        let method_table =
                            graph.read_value(ptr, RuntimePrimType::Ptr);
                        Some(method_table)
                    }
                    _ => None,
                }
            }

            ExprKind::FieldAccess { obj, field } => {
                let obj = *obj;

                let Some((obj_sig_type, field_sig_type)) =
                    self.0.infer_object_field_sig(graph, obj, field)?
                else {
                    return Ok(None);
                };

                let obj_sym_type =
                    obj_sig_type.to_symbolic(&|_: usize| -> SymbolicType {
                        todo!("Error handling for unresolved generics")
                    });
                let opt_mt_name = {
                    let base =
                        std::iter::successors(Some(&obj_sym_type), |sym| {
                            match sym {
                                SymbolicType::GenericInst { base, .. } => {
                                    Some(base)
                                }
                                _ => None,
                            }
                        })
                        .last()
                        .expect("Iterator contains at minimum `obj_sym_type`");

                    match base {
                        SymbolicType::Named { name, .. } => {
                            Some(name.as_str().into())
                        }
                        SymbolicType::Metadata { module, index } => {
                            let reader = self.0.reader()?;
                            let module_ptr =
                                reader.runtime_module_by_name(&module)?;
                            let name = reader
                                .runtime_module(module_ptr)?
                                .metadata(reader)?
                                .get(*index)?
                                .name()?;
                            Some(name)
                        }
                        _ => None,
                    }
                    .map(|name| {
                        name.rsplit_once('.')
                            .map(|(_, last)| last)
                            .unwrap_or(&name)
                            .to_string()
                    })
                };
                let mt = {
                    let direct = graph.type_to_method_table(obj_sym_type);
                    let init_func = graph.function_def(vec![], direct);
                    graph.lazy_static(init_func)
                };

                if let Some(mt_name) = &opt_mt_name {
                    graph.name(mt, mt_name)?;
                }

                // The `field_description.offset()` is relative to the
                // location of the first data member, regardless of
                // whether the object is a Class or ValueType
                // instance.  However, Class instances have an
                // additional pointer to their method table, prior to
                // the first data member.
                let obj_ptr = graph.prim_cast(obj, RuntimePrimType::Ptr);
                let ptr = if obj_sig_type.is_value_type() {
                    obj_ptr
                } else {
                    graph.add(obj_ptr, Pointer::SIZE)
                };

                let offset = {
                    let direct = graph.field_offset(mt, field);
                    let init_func = graph.function_def(vec![], direct);
                    graph.lazy_static(init_func)
                };
                if let Some(mt_name) = opt_mt_name {
                    graph.name(offset, format!("{mt_name}_{field}_offset"))?;
                }

                let ptr = graph.add(ptr, offset);
                if let Some(name) = name {
                    graph.name(ptr, format!("member_ptr_{field}_of_{name}"))?;
                } else {
                    graph.name(ptr, format!("member_ptr_{field}"))?;
                }

                let ptr = {
                    let condition = graph.is_some(obj_ptr);
                    let else_branch = graph.none();
                    graph.if_else(condition, ptr, else_branch)
                };

                let value =
                    self.access_field_value(graph, ptr, &field_sig_type)?;

                graph.name(
                    value,
                    if let Some(name) = name {
                        format!("member_{field}_of_{name}")
                    } else {
                        format!("member_{field}")
                    },
                )?;
                Some(value)
            }
            ExprKind::IndexAccess { obj, indices } => 'ty: {
                let array = *obj;
                let Some(array_type) = self.0.infer_sig(graph, array)? else {
                    break 'ty None;
                };

                let element_type = match array_type {
                    SignatureType::SizeArray(element_type)
                    | SignatureType::MultiDimArray { element_type, .. } => {
                        element_type.as_ref()
                    }
                    _ => {
                        break 'ty None;
                    }
                };

                let (header_size_bytes, shape) = match array_type {
                    SignatureType::SizeArray { .. } => {
                        // MethodTable* (ptr, 8 bytes)
                        // NumElements  (u64, 8 bytes)
                        // First Element
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
                    SignatureType::MultiDimArray { rank, .. } => {
                        // MethodTable* (ptr, 8 bytes)
                        // NumElements  (u64, 8 bytes)
                        // shape        ([u32; RANK], 4*RANK bytes)
                        // lower bounds ([u32; RANK], 4*RANK bytes)
                        // First Element
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
                        break 'ty None;
                    }
                };

                if shape.len() != indices.len() {
                    return Err(Error::IncorrectNumberOfIndices {
                        num_provided: indices.len(),
                        num_expected: shape.len(),
                    });
                }

                let array_stride = {
                    let array_sym_type =
                        array_type.to_symbolic(&|_: usize| -> SymbolicType {
                            todo!("Error handling for unresolved generics")
                        });
                    let method_table =
                        graph.type_to_method_table(array_sym_type);

                    let direct = graph.array_stride(method_table);
                    let init_func = graph.function_def(vec![], direct);
                    graph.lazy_static(init_func)
                };

                let ptr = {
                    let array_ptr =
                        graph.prim_cast(array, RuntimePrimType::Ptr);
                    let first_element = graph.add(array_ptr, header_size_bytes);
                    let byte_offset = {
                        let strides = {
                            let mut strides = Vec::new();
                            let mut cum_prod: SymbolicValue = array_stride;
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

                let value =
                    self.access_field_value(graph, ptr, element_type)?;

                Some(value)
            }

            ExprKind::SymbolicDowncast { obj, ty } => {
                let obj = *obj;

                let sub_signature_type = self.0.symbolic_to_signature(ty)?;
                let sub_runtime_type = self
                    .0
                    .reader()?
                    .signature_type_to_runtime_type(&sub_signature_type, &[])?;

                let obj_method_table = graph.object_method_table(obj);
                let cls_method_table = {
                    let direct = graph.type_to_method_table(ty.clone());
                    let init_func = graph.function_def(vec![], direct);
                    graph.lazy_static(init_func)
                };
                let condition =
                    graph.is_subclass_of(obj_method_table, cls_method_table);
                let failure = graph.none();
                let filtered = graph.if_else(condition, obj, failure);

                let downcast =
                    graph.pointer_cast(filtered, sub_runtime_type.into());
                Some(downcast)
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
