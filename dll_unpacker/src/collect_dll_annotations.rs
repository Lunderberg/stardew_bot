use crate::dll_unpacker::{
    Assembly, AssemblyRef, ClassLayout, ClrRuntimeHeader, Constant,
    CustomAttribute, DeclSecurity, Event, EventMap, Field, FieldLayout,
    FieldMarshal, FieldRVA, GenericParam, GenericParamConstraint, ImplMap,
    InterfaceImpl, ManifestResource, MemberRef, Metadata, MetadataRow,
    MetadataTable, MetadataTableHeader, MethodDef, MethodImpl, MethodSemantics,
    MethodSpec, Module, ModuleRef, NestedClass, Param, Property, PropertyMap,
    RawCLRMetadata, StandAloneSig, StreamHeader, TypeDef, TypeRef, TypeSpec,
};
use crate::{Annotation, Annotator, DLLUnpacker, Error, ExportedType};

impl<'a> DLLUnpacker<'a> {
    pub fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        self.dos_header()?.collect_annotations(annotator)?;
        self.pe_header()?.collect_annotations(annotator)?;
        self.optional_header()?.collect_annotations(annotator)?;
        self.clr_runtime_header()?.collect_annotations(annotator)?;

        for section in self.iter_section_header()? {
            annotator
                .group(section.section_range()?)
                .name(format!("{} PE section", section.name()?.value()));
            section.collect_annotations(annotator)?;
        }

        self.raw_metadata()?.collect_annotations(annotator)?;
        let layout = self.metadata_layout()?;
        layout.metadata(self.bytes).collect_annotations(annotator)?;

        Ok(())
    }
}

impl<'a> ClrRuntimeHeader<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.group(self.bytes).name("CLR Runtime Header");

        annotator.value(self.header_size()?).name("header_size");
        annotator
            .value(self.major_runtime_version()?)
            .name("major_runtime_version");
        annotator
            .value(self.minor_runtime_version()?)
            .name("minor_runtime_version");
        annotator
            .value(self.metadata_range()?)
            .name("metadata_range");
        annotator.value(self.flags()?).name("flags");
        annotator
            .value(self.entry_point_token()?)
            .name("entry_point_token");
        annotator.value(self.resources()?).name("resources");
        annotator
            .opt_value(self.strong_name_signature()?)
            .name("strong_name_signature");
        annotator
            .opt_value(self.code_manager_table()?)
            .name("code_manager_table");
        annotator
            .opt_value(self.vtable_fixups()?)
            .name("vtable_fixups");
        annotator
            .opt_value(self.export_address_table_jumps()?)
            .name("export_address_table_jumps");
        annotator
            .opt_value(self.managed_native_header()?)
            .name("managed_native_header");

        Ok(())
    }
}

impl<'a> RawCLRMetadata<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.group(self.bytes).name("CLR Metadata");

        annotator
            .range(self.metadata_signature()?.loc())
            .name("Metadata signature");
        annotator.value(self.major_version()?).name("major_version");
        annotator.value(self.minor_version()?).name("minor_version");
        annotator.value(self.reserved()?).name("reserved");
        annotator
            .value(self.version_str_len()?)
            .name("version_str_len");
        annotator.value(self.version_str()?).name("version_str");
        annotator.value(self.flags()?).name("flags");
        annotator.value(self.num_streams()?).name("num_streams");

        self.iter_stream_header()?.try_for_each(|stream_header| {
            stream_header?.collect_annotations(annotator)
        })?;

        self.metadata_tables_header()?
            .collect_annotations(annotator)?;

        Ok(())
    }
}

impl<'a> StreamHeader<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .group(self.offset.loc().start..self.name.loc().end)
            .name(format!("CLR {} Stream Header", self.name.value()));

        annotator.value(self.offset).name("Stream Offset");
        annotator.value(self.size).name("Stream Size");
        annotator.value(self.name).name("Stream Name");

        annotator
            .group(self.bytes)
            .name(format!("{} Stream", self.name.value()));

        Ok(())
    }
}

impl<'a> MetadataTableHeader<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.reserved_0()?).name("reserved_0");
        annotator.value(self.major_version()?).name("major_version");
        annotator.value(self.minor_version()?).name("minor_version");
        annotator.value(self.heap_sizes()?).name("heap_sizes");
        annotator.value(self.reserved_1()?).name("reserved_1");
        annotator
            .value(self.valid_table_bitfield()?)
            .name("valid_table_bitfield");
        annotator
            .value(self.sorted_table_bitfield()?)
            .name("sorted_table_bitfield");

        for res in self.iter_num_rows()? {
            let (value, kind) = res?;
            annotator.value(value).name(format!("Num {kind:?} rows"));
        }

        Ok(())
    }
}

impl<'a> Metadata<'a> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        self.iter_untyped_rows()
            .for_each(|(table_kind, i_row, ptr_range)| {
                annotator
                    .range(ptr_range)
                    .name(format!("{}[{}]", table_kind, i_row));
            });

        self.module_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.type_ref_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        let field_table = self.field_table();
        let method_table = self.method_def_table();
        let param_table = self.param_table();
        self.type_def_table().iter_rows().try_for_each(|row| {
            row.collect_annotations(
                annotator,
                &field_table,
                &method_table,
                &param_table,
            )
        })?;

        self.interface_impl_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.member_ref_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.constant_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.custom_attribute_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.field_marshal_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.decl_security_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.class_layout_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.field_layout_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.stand_alone_sig_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.event_map_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.event_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.property_map_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.property_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.method_semantics_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.method_impl_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.module_ref_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.type_spec_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.impl_map_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.field_rva_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.assembly_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.assembly_ref_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.exported_type_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.manifest_resource_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.nested_class_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.generic_param_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.method_spec_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        self.generic_param_constraint_table()
            .iter_rows()
            .try_for_each(|row| row.collect_annotations(annotator))?;

        Ok(())
    }
}

impl<'a> MetadataRow<'a, Module> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.generation_unpacked()?)
            .name("generation");

        let name = self.name()?;
        annotator
            .value(self.name_index()?)
            .name("Name")
            .append_value(name);

        annotator.value(self.module_id_index()?).name("Module id");
        annotator
            .value(self.module_id_unpacked()?)
            .name(format!("GUID, '{name}' Module"));
        annotator.opt_value(self.enc_id_index()?).name("enc_id");
        annotator
            .opt_value(self.enc_base_id_index()?)
            .name("enc_base_id");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, TypeRef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.resolution_scope_index()?)
            .name("resolution_scope")
            .append_value(self.resolution_scope()?.name()?);

        annotator
            .value(self.name_index()?)
            .name("Type name")
            .append_value(self.name()?);
        annotator
            .value(self.namespace_index()?)
            .name("Type namespace")
            .append_value(self.namespace()?);

        Ok(())
    }
}

impl<'a> MetadataRow<'a, TypeDef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        field_table: &MetadataTable<Field>,
        method_def_table: &MetadataTable<MethodDef>,
        param_table: &MetadataTable<Param>,
    ) -> Result<(), Error> {
        annotator.value(self.flags_unpacked()?).name("flags");
        let type_name = self.name()?;
        annotator
            .value(self.name_index()?)
            .name("Type name")
            .append_value(type_name);

        let type_namespace = self.namespace()?;
        annotator
            .value(self.namespace_index()?)
            .name("Type namespace")
            .append_value(type_namespace);

        annotator
            .opt_value(self.extends_index()?)
            .name("extends")
            .append_value(if let Some(base_class) = self.extends()? {
                base_class.name()?
            } else {
                "(none)"
            });
        let field_indices = self.field_indices();
        annotator.value(field_indices).name("Fields");

        annotator
            .group(field_table.address_range(field_indices)?)
            .name(format!("Class '{type_name}'"));

        self.iter_fields()?.try_for_each(|field| {
            field.collect_annotations(annotator, type_name, type_namespace)
        })?;

        let method_indices = self.method_indices();
        annotator.value(method_indices).name("method_indices");

        annotator
            .group(method_def_table.address_range(method_indices)?)
            .name(format!("Class '{type_name}'"));

        self.iter_methods()?.try_for_each(|method| {
            method.collect_annotations(
                annotator,
                type_name,
                type_namespace,
                param_table,
            )
        })?;

        if self.num_methods() > 0 {
            let start = self.iter_methods()?.find_map(|method| {
                method
                    .iter_params()
                    .unwrap()
                    .next()
                    .map(|param| param.ptr_range().start)
            });
            if let Some(start) = start {
                let end = self
                    .iter_methods()?
                    .rev()
                    .find_map(|method| {
                        method
                            .iter_params()
                            .unwrap()
                            .last()
                            .map(|param| param.ptr_range().end)
                    })
                    .unwrap();
                annotator
                    .group(start..end)
                    .name(format!("Class '{type_name}'"));
            }
        }

        Ok(())
    }
}

impl<'a> MetadataRow<'a, Field> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        type_name: &str,
        _type_namespace: &str,
    ) -> Result<(), Error> {
        annotator.value(self.flags_unpacked()?).name("flags");

        let name = self.name()?;
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);
        annotator
            .value(self.raw_signature_index()?)
            .name("signature")
            .append_value(self.signature()?);

        annotator
            .range(self.raw_signature()?.into())
            .name(format!("'{type_name}.{name}' signature"));

        Ok(())
    }
}

impl<'a> MetadataRow<'a, MethodDef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        type_name: &str,
        type_namespace: &str,
        param_table: &MetadataTable<Param>,
    ) -> Result<(), Error> {
        let name = self.name()?;

        let rva_ann = annotator.opt_value(self.rva_unpacked()?).name("rva");

        if let Some(cil_method) = self.cil_method()? {
            let method_range = cil_method.method_range()?;
            rva_ann.append_value(method_range.start);
            annotator
                .range(method_range)
                .name(format!("Method '{name}' Def"));
            cil_method.collect_annotations(annotator)?;
        }

        annotator
            .value(self.impl_flags_unpacked()?)
            .name("impl_flags");
        annotator.value(self.flags_unpacked()?).name("flags");

        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);

        annotator
            .value(self.signature_index()?)
            .name("signature_index");
        annotator
            .range(self.signature()?.into())
            .name(format!("'{type_name}.{name}' signature"));

        let param_indices = self.param_indices();
        annotator.value(param_indices).name("param_indices");

        annotator
            .range(param_indices.loc())
            .name("Param names")
            .value(
                self.iter_params()?
                    .map(|param| param.name())
                    .collect::<Result<String, Error>>()?,
            );

        self.iter_params()?.try_for_each(|param| {
            param.collect_annotations(
                annotator,
                type_name,
                type_namespace,
                name,
            )
        })?;

        annotator
            .group(param_table.address_range(param_indices)?)
            .name(format!("Method '{name}'"));

        Ok(())
    }
}

impl<'a> MetadataRow<'a, Param> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
        _type_name: &str,
        _type_namespace: &str,
        _method_name: &str,
    ) -> Result<(), Error> {
        annotator.value(self.flags_unpacked()?).name("flags");
        annotator.value(self.sequence_unpacked()?).name("sequence");

        let name = self.name()?;
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);

        Ok(())
    }
}

impl<'a> MetadataRow<'a, InterfaceImpl> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.class_index()?)
            .name("Class")
            .append_value(self.class()?.name()?);
        annotator
            .value(self.interface_index()?)
            .name("Interface")
            .append_value(self.interface()?.name()?);

        Ok(())
    }
}

impl<'a> MetadataRow<'a, MemberRef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.class_index()?)
            .name("class_index")
            .append_value(self.class()?.name()?);

        let name = self.name()?;
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);

        annotator
            .value(self.signature_index()?)
            .name("signature_index");

        annotator
            .range(self.signature()?.into())
            .name(format!("MemberRef {name}"));

        Ok(())
    }
}

impl<'a> MetadataRow<'a, Constant> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.type_value_unpacked()?)
            .name("Type value");
        annotator
            .value(self.parent_index()?)
            .name("Parent")
            .append_value(self.parent()?.name()?);

        annotator.value(self.value_index()?).name("Value");
        annotator.range(self.value()?.into()).name("Constant value");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, CustomAttribute> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.parent_index()?).name("Parent");
        annotator
            .value(self.attribute_type_index()?)
            .name("Type")
            .append_value(self.attribute_type()?.name()?);

        annotator.value(self.value_index()?).name("Value");
        annotator
            .range(self.value()?.into())
            .name("CustomAttribute value");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, FieldMarshal> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.parent_index()?)
            .name("Parent")
            .append_value(self.parent()?.name()?);
        annotator
            .value(self.native_type_index()?)
            .name("Native type");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, DeclSecurity> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.action_unpacked()?).name("Action");
        annotator
            .value(self.parent_index()?)
            .name("Parent")
            .append_value(self.parent()?.name()?);

        annotator
            .value(self.permission_set_index()?)
            .name("Permission set");
        annotator
            .range(self.permission_set()?.into())
            .name("DeclSecurity permission set");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, ClassLayout> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.alignment_unpacked()?)
            .name("Alignment");
        annotator.value(self.size_unpacked()?).name("Size (bytes)");

        annotator
            .value(self.class_index()?)
            .name("Class")
            .append_value(self.class()?.name()?);

        Ok(())
    }
}

impl<'a> MetadataRow<'a, FieldLayout> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        let class_name = self.field()?.find_owning_class()?.name()?;
        let field_name = self.field()?.name()?;

        annotator.value(self.offset_unpacked()?).name("Offset");
        annotator
            .value(self.field_index()?)
            .name("Field")
            .append_value(format!("{class_name}.{field_name}"));

        Ok(())
    }
}

impl<'a> MetadataRow<'a, StandAloneSig> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.signature_index()?).name("Signature");
        annotator
            .range(self.signature()?.into())
            .name("StandAloneSig signature");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, EventMap> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.class_index()?)
            .name("Class")
            .append_value(self.class()?.name()?);
        annotator.value(self.event_indices()).name("Events");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, Event> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.flags_unpacked()?).name("Flags");

        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(self.name()?);

        annotator
            .opt_value(self.event_type_index()?)
            .name("Event type")
            .append_value(if let Some(event) = self.event_type()? {
                event.name()?
            } else {
                "(none)"
            });

        Ok(())
    }
}

impl<'a> MetadataRow<'a, PropertyMap> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.class_index()?)
            .name("Class")
            .append_value(self.class()?.name()?);
        annotator.value(self.property_indices()).name("Properties");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, Property> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.flags_unpacked()?).name("Flags");

        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(self.name()?);

        annotator.value(self.signature_index()?).name("Signature");
        annotator
            .range(self.signature()?.into())
            .name("Property signature");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, MethodSemantics> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.semantics_unpacked()?)
            .name("Semantics");
        annotator
            .value(self.method_index()?)
            .name("Method")
            .append_value(self.method()?.name()?);
        annotator
            .value(self.association_index()?)
            .name("Association")
            .append_value(self.association()?.name()?);

        Ok(())
    }
}

impl<'a> MetadataRow<'a, MethodImpl> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.class_index()?)
            .name("Class")
            .append_value(self.class()?.name()?);
        annotator
            .value(self.method_body_index()?)
            .name("Method body")
            .append_value(self.method_body()?.name()?);
        annotator
            .value(self.method_declaration_index()?)
            .name("Method declaration")
            .append_value(self.method_declaration()?.name()?);

        Ok(())
    }
}

impl<'a> MetadataRow<'a, ModuleRef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(self.name()?);

        Ok(())
    }
}

impl<'a> MetadataRow<'a, TypeSpec> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.signature_index()?).name("Signature");
        annotator
            .range(self.signature()?.into())
            .name("TypeSpec signature");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, ImplMap> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.flags_unpacked()?)
            .name("Mapping flags");
        annotator
            .value(self.member_forwarded_index()?)
            .name("Member forwarded")
            .append_value(self.member_forwarded()?.name()?);

        annotator
            .value(self.import_name_index()?)
            .name("Import name")
            .append_value(self.import_name()?);

        annotator
            .value(self.import_scope_index()?)
            .name("Import scope")
            .append_value(self.import_scope()?.name()?);

        Ok(())
    }
}

impl<'a> MetadataRow<'a, FieldRVA> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.rva_unpacked()?)
            .name("RVA")
            .append_value(self.address()?);

        let name = self.field()?.name()?;

        annotator
            .value(self.field_index()?)
            .name("Field")
            .append_value(name);

        let address = self.address()?;
        annotator
            .range(address..address + 1)
            .name(format!("Field '{name}' default value"));

        Ok(())
    }
}

impl<'a> MetadataRow<'a, Assembly> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.hash_algorithm_id_unpacked()?)
            .name("Hash algorithm");
        annotator
            .value(self.major_version_unpacked()?)
            .name("Major version");
        annotator
            .value(self.minor_version_unpacked()?)
            .name("Minor version");
        annotator
            .value(self.build_number_unpacked()?)
            .name("Build number");
        annotator
            .value(self.revision_number_unpacked()?)
            .name("Revision number");

        annotator.value(self.flags_unpacked()?).name("Flags");

        let name = self.name()?;
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);

        annotator.value(self.public_key_index()?).name("Public key");
        annotator
            .range(self.public_key()?.into())
            .name(format!("Public key, '{name}' assembly"));

        annotator
            .value(self.culture_index()?)
            .name("Culture")
            .append_value(self.culture()?);

        Ok(())
    }
}

impl<'a> MetadataRow<'a, AssemblyRef> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.major_version_unpacked()?)
            .name("Major version");
        annotator
            .value(self.minor_version_unpacked()?)
            .name("Minor version");
        annotator
            .value(self.build_number_unpacked()?)
            .name("Build number");
        annotator
            .value(self.revision_number_unpacked()?)
            .name("Revision number");

        annotator.value(self.flags_unpacked()?).name("Flags");

        let name = self.name()?;
        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(name);

        annotator
            .value(self.culture_index()?)
            .name("culture")
            .append_value(self.culture()?);

        annotator
            .value(self.public_key_or_token_index()?)
            .name("Public key/token");
        annotator
            .range(self.public_key_or_token()?.into())
            .name(format!("Public key/token, '{name}' AssemblyRef"));

        annotator.value(self.hash_value_index()?).name("Hash value");
        annotator
            .range(self.hash_value()?.into())
            .name(format!("HashValue, '{name}' AssemblyRef"));

        Ok(())
    }
}

impl<'a> MetadataRow<'a, ExportedType> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.flags_unpacked()?).name("Flags");
        annotator
            .value(self.type_def_unpacked()?)
            .name("TypeDef id");

        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(self.name()?);

        annotator
            .value(self.namespace_index()?)
            .name("namespace")
            .append_value(self.namespace()?);

        annotator
            .value(self.implementation_index()?)
            .name("Implementation");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, ManifestResource> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator.value(self.offset_unpacked()?).name("Offset");
        annotator.value(self.flags_unpacked()?).name("Flags");

        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(self.name()?);

        annotator
            .opt_value(self.implementation_index()?)
            .name("Implementation");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, NestedClass> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.nested_class_index()?)
            .name("Nested class")
            .append_value(self.nested_class()?.name()?);
        annotator
            .value(self.enclosing_class_index()?)
            .name("Enclosing class")
            .append_value(self.enclosing_class()?.name()?);

        Ok(())
    }
}

impl<'a> MetadataRow<'a, GenericParam> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.number_unpacked()?)
            .name("Param number");
        annotator.value(self.flags_unpacked()?).name("Flags");
        annotator
            .value(self.owner_index()?)
            .name("Owner index")
            .append_value(self.owner()?.name()?);

        annotator
            .value(self.name_index()?)
            .name("name")
            .append_value(self.name()?);

        Ok(())
    }
}

impl<'a> MetadataRow<'a, MethodSpec> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.method_index()?)
            .name("Method")
            .append_value(self.method()?.name()?);

        annotator
            .value(self.instantiation_index()?)
            .name("Instantiation");
        annotator
            .range(self.instantiation()?.into())
            .name("MethodSpec instantiation");

        Ok(())
    }
}

impl<'a> MetadataRow<'a, GenericParamConstraint> {
    fn collect_annotations(
        &self,
        annotator: &mut impl Annotator,
    ) -> Result<(), Error> {
        annotator
            .value(self.generic_param_index()?)
            .name("Generic param")
            .append_value(self.generic_param()?.name()?);
        annotator
            .value(self.constraint_index()?)
            .name("Constraint")
            .append_value(self.constraint()?.name()?);

        Ok(())
    }
}
