use std::borrow::Borrow;
use std::cell::OnceCell;
use std::ops::Range;

use elsa::FrozenMap;

use dll_unpacker::{
    Field, MetadataCodedIndex, MetadataRow, MetadataTableIndex, TypeDefOrRef,
    TypeRef,
};
use memory_reader::{MemoryReader, Pointer};

use crate::extensions::*;
use crate::{
    Error, FieldDescription, FieldDescriptions, MethodTable, RuntimeModule,
    RuntimeObject, RuntimeType, RuntimeValue, TypedPointer,
};

#[derive(Default)]
pub struct PersistentState {
    method_tables: FrozenMap<TypedPointer<MethodTable>, Box<MethodTable>>,
    runtime_module_vtable: OnceCell<Pointer>,
    runtime_modules: FrozenMap<TypedPointer<RuntimeModule>, Box<RuntimeModule>>,
    field_descriptions:
        FrozenMap<TypedPointer<MethodTable>, Box<Option<FieldDescriptions>>>,
    runtime_module_by_name: FrozenMap<String, Box<TypedPointer<RuntimeModule>>>,
    method_table_by_metadata:
        FrozenMap<TypeInModule, Box<TypedPointer<MethodTable>>>,

    field_to_runtime_type: FrozenMap<
        (TypedPointer<MethodTable>, MetadataTableIndex<Field>),
        Box<RuntimeType>,
    >,
    field_to_type_name: FrozenMap<
        (TypedPointer<MethodTable>, MetadataTableIndex<Field>),
        Box<String>,
    >,
    field_to_name: FrozenMap<
        (TypedPointer<MethodTable>, MetadataTableIndex<Field>),
        Box<String>,
    >,

    method_table_to_name: FrozenMap<TypedPointer<MethodTable>, Box<String>>,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
struct TypeInModule {
    module: TypedPointer<RuntimeModule>,
    coded_index: MetadataCodedIndex<TypeDefOrRef>,
}

pub struct CachedReader<'a> {
    state: &'a PersistentState,
    reader: &'a MemoryReader,
}

impl PersistentState {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn cached_reader<'a>(
        &'a self,
        reader: &'a MemoryReader,
    ) -> CachedReader<'a> {
        CachedReader {
            state: self,
            reader,
        }
    }

    pub fn method_table(
        &self,
        ptr: TypedPointer<MethodTable>,
        reader: &MemoryReader,
    ) -> Result<&MethodTable, Error> {
        self.method_tables.try_insert(ptr, || ptr.read(reader))
    }

    pub fn runtime_module(
        &self,
        ptr: TypedPointer<RuntimeModule>,
        reader: &MemoryReader,
    ) -> Result<&RuntimeModule, Error> {
        self.runtime_modules.try_insert(ptr, || {
            let runtime_module = ptr.read(reader)?;

            self.runtime_module_vtable
                .or_try_init(|| runtime_module.vtable_location(reader))?;

            // Save the name of the module, to avoid needing to do a
            // memory search for it later.
            //
            // TODO: See if this could cause a weird order-dependent
            // error, if a module can be successfully located through
            // a known pointer, but cannot be located by the
            // heuristics used to search in memory.  If that is the
            // case, then this caching could paper over a need to
            // improve the heuristics.
            let dll_region = runtime_module.dll_region(reader)?;
            self.runtime_module_by_name.insert(
                dll_region.name().trim_end_matches(".dll").to_string(),
                Box::new(ptr),
            );
            Ok(runtime_module)
        })
    }

    pub fn runtime_module_by_name(
        &self,
        name: &str,
        reader: &MemoryReader,
    ) -> Result<TypedPointer<RuntimeModule>, Error> {
        // TODO: Have `try_insert` accept `impl ToOwned<Owned=T>` to
        // avoid needing to copy the string.
        self.runtime_module_by_name
            .try_insert(name.to_string(), || {
                let dll_region = reader
                    .regions
                    .iter()
                    .filter(|region| {
                        region.short_name().ends_with(".dll")
                            && region.short_name().trim_end_matches(".dll")
                                == name
                    })
                    .max_by_key(|region| region.size_bytes())
                    .ok_or_else(|| {
                        Error::RegionForDLLNotFoundFromName(name.to_string())
                    })?
                    .read()?;

                let metadata_layout =
                    dll_unpacker::unpack_metadata_layout(&dll_region)?;
                let metadata = metadata_layout.metadata(&dll_region);
                let module_vtable = self.runtime_module_vtable.get().copied();

                let module_ptr =
                    RuntimeModule::locate(metadata, module_vtable, &reader)?;

                Ok(module_ptr)
            })
            .copied()
    }

    pub fn field_to_runtime_type(
        &self,
        desc: &FieldDescription,
        reader: &MemoryReader,
    ) -> Result<RuntimeType, Error> {
        let locally_known_type = desc.runtime_type()?;
        if let Some(ty) = locally_known_type {
            // Quick return case, when the FieldDescription is
            // sufficient to know how to unpack the field.
            return Ok(ty);
        }

        // Otherwise, need to use the metadata to find the MethodTable
        // associated with the ValueType.

        let lookup_key = (desc.method_table(), desc.token());

        self.field_to_runtime_type
            .try_insert(lookup_key, || {
                let module_ptr =
                    self.method_table(desc.method_table(), reader)?.module();
                let field_metadata = self
                    .runtime_module(module_ptr, reader)?
                    .metadata(reader)?
                    .get(desc.token())?;
                let signature = field_metadata.signature()?;

                let coded_index =
                    signature.as_value_type()?.ok_or_else(|| {
                        Error::ExpectedValueTypeAsMetadataSignature {
                            field_name: field_metadata
                                .name()
                                .unwrap_or("(unknown field)")
                                .to_string(),
                            field_type: format!("{signature}"),
                        }
                    })?;

                let method_table = self.method_table_by_metadata(
                    module_ptr,
                    coded_index,
                    reader,
                )?;
                let size = self.method_table(method_table, reader)?.base_size();

                Ok(RuntimeType::ValueType { method_table, size })
            })
            .copied()
    }

    pub fn field_to_type_name(
        &self,
        desc: &FieldDescription,
        reader: &MemoryReader,
    ) -> Result<&str, Error> {
        let lookup_key = (desc.method_table(), desc.token());
        self.field_to_type_name
            .try_insert(lookup_key, || {
                let module_ptr =
                    self.method_table(desc.method_table(), reader)?.module();
                let field_metadata = self
                    .runtime_module(module_ptr, reader)?
                    .metadata(reader)?
                    .get(desc.token())?;

                let is_static = if field_metadata.is_static()? {
                    "static "
                } else {
                    ""
                };
                let signature = field_metadata.signature()?;
                let type_name = format!("{is_static}{signature}");

                Ok(type_name)
            })
            .map(|s| s.as_str())
    }

    pub fn field_to_name(
        &self,
        desc: &FieldDescription,
        reader: &MemoryReader,
    ) -> Result<&str, Error> {
        let lookup_key = (desc.method_table(), desc.token());
        self.field_to_name
            .try_insert(lookup_key, || {
                let module_ptr =
                    self.method_table(desc.method_table(), reader)?.module();
                let field_metadata = self
                    .runtime_module(module_ptr, reader)?
                    .metadata(reader)?
                    .get(desc.token())?;
                let name = field_metadata.name()?.to_string();

                Ok(name)
            })
            .map(|s| s.as_str())
    }

    pub fn field_descriptions(
        &self,
        ptr: TypedPointer<MethodTable>,
        reader: &MemoryReader,
    ) -> Result<Option<&FieldDescriptions>, Error> {
        self.field_descriptions
            .try_insert(ptr, || {
                let method_table = self.method_table(ptr, reader)?;
                method_table.get_field_descriptions(reader)
            })
            .map(|opt| opt.as_ref())
    }

    pub fn method_table_by_metadata(
        &self,
        module: TypedPointer<RuntimeModule>,
        coded_index: MetadataCodedIndex<TypeDefOrRef>,
        reader: &MemoryReader,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        let lookup_key = TypeInModule {
            module,
            coded_index,
        };
        self.method_table_by_metadata
            .try_insert(
                lookup_key,
                || -> Result<TypedPointer<MethodTable>, Error> {
                    let module = self.runtime_module(module, reader)?;
                    let type_metadata =
                        module.metadata(reader)?.get(coded_index)?;
                    match type_metadata {
                        dll_unpacker::MetadataTypeDefOrRef::TypeDef(row) => {
                            module.get_method_table(row.index(), reader)
                        }
                        dll_unpacker::MetadataTypeDefOrRef::TypeRef(row) => {
                            self.type_ref_lookup(row, reader)
                        }
                        dll_unpacker::MetadataTypeDefOrRef::TypeSpec(_) => {
                            todo!()
                        }
                    }
                },
            )
            .and_then(|ptr| {
                if ptr.is_null() {
                    let module = self.runtime_module(module, reader)?;
                    let metadata = module.metadata(reader)?.get(coded_index)?;
                    let name = metadata.name()?;
                    let namespace = metadata.namespace()?;
                    Err(Error::UnexpectedNullMethodTable(format!(
                        "{namespace}.{name}"
                    )))
                } else {
                    Ok(ptr)
                }
            })
            .copied()
    }

    pub fn method_table_to_name(
        &self,
        ptr: TypedPointer<MethodTable>,
        reader: &MemoryReader,
    ) -> Result<&str, Error> {
        self.method_table_to_name
            .try_insert(ptr, || {
                let method_table = self.method_table(ptr, reader)?;
                let module =
                    self.runtime_module(method_table.module(), reader)?;
                let metadata = module.metadata(reader)?;
                let name = metadata.get(method_table.token())?.name()?;
                Ok(name.to_string())
            })
            .map(|s| s.as_str())
    }

    fn type_ref_lookup(
        &self,
        type_ref: MetadataRow<TypeRef>,
        reader: &MemoryReader,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        self.pseudo_link_method_table(
            type_ref.target_dll_name()?,
            type_ref.name()?,
            type_ref.namespace()?,
            reader,
        )
    }

    fn pseudo_link_method_table(
        &self,
        assembly_name: &str,
        symbol_name: &str,
        symbol_namespace: &str,
        reader: &MemoryReader,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        let field_module_ptr =
            self.runtime_module_by_name(assembly_name, reader)?;
        let field_module = self.runtime_module(field_module_ptr, reader)?;
        let metadata = field_module.metadata(reader)?;

        let ref_type_def = metadata.type_def_table().iter_rows().find_ok(
            |row| -> Result<_, Error> {
                Ok(row.name()? == symbol_name
                    && row.namespace()? == symbol_namespace)
            },
        )?;

        if let Some(ref_type_def) = ref_type_def {
            // The TypeRef pointed to an assembly, and that assembly
            // contains the TypeDef we're looking for.
            return field_module.get_method_table(ref_type_def.index(), reader);
        }

        let ref_exported_type = metadata
            .exported_type_table()
            .iter_rows()
            .find_ok(|row| -> Result<_, Error> {
                Ok(row.name()? == symbol_name
                    && row.namespace()? == symbol_namespace)
            })?;

        if let Some(ref_exported_type) = ref_exported_type {
            // > I'm sorry, but your TypeDef is in another castle.
            //
            // The TypeRef/ExportedType pointed to an assembly, but
            // that assembly just re-exports a symbol, and points to
            // another
            return self.pseudo_link_method_table(
                ref_exported_type.target_dll_name()?,
                ref_exported_type.name()?,
                ref_exported_type.namespace()?,
                reader,
            );
        }

        use itertools::Itertools;
        let names = std::iter::empty()
            .chain(metadata.type_def_table().iter_rows().filter_map(|row| {
                let name = row.name().ok()?;
                let namespace = row.namespace().ok()?;
                Some(format!("  TypeDef {namespace} . {name}"))
            }))
            .chain(metadata.exported_type_table().iter_rows().filter_map(
                |row| {
                    let name = row.name().ok()?;
                    let namespace = row.namespace().ok()?;
                    Some(format!("  ExportedType {namespace} . {name}"))
                },
            ))
            .sorted()
            .join("\n");

        panic!(
            "Could not find {symbol_namespace} . {symbol_name} \
                in the assembly {assembly_name}.  \
                The assembly contains symbols:\n{names}"
        )
    }

    pub fn iter_fields<'a>(
        &'a self,
        method_table_ptr: TypedPointer<MethodTable>,
        reader: &'a MemoryReader,
    ) -> Result<impl Iterator<Item = FieldDescription<'a>> + 'a, Error> {
        let iter = std::iter::successors(
            Some(Ok(method_table_ptr)),
            |res_ptr| -> Option<Result<_, Error>> {
                res_ptr
                    .as_ref()
                    .ok()
                    .map(|ptr| -> Result<_, Error> {
                        let method_table = self.method_table(*ptr, reader)?;
                        let parent = method_table.parent_method_table();
                        if parent.is_null() {
                            Ok(None)
                        } else {
                            Ok(Some(parent))
                        }
                    })
                    .map(|res| res.transpose())
                    .flatten()
            },
        )
        .map(|res_ptr| {
            res_ptr.and_then(|ptr| self.field_descriptions(ptr, reader))
        })
        .collect::<Result<Vec<_>, Error>>()?
        .into_iter()
        .flatten()
        .flatten();

        Ok(iter)
    }
}

impl<'a> CachedReader<'a> {
    pub fn underlying_reader(&self) -> &'a MemoryReader {
        self.reader
    }

    pub fn object(
        &self,
        location: TypedPointer<RuntimeObject>,
    ) -> Result<RuntimeObject, Error> {
        location.read(self.reader)
    }

    pub fn value(
        &self,
        runtime_type: RuntimeType,
        location: Range<Pointer>,
    ) -> Result<RuntimeValue, Error> {
        let bytes = self
            .reader
            .read_bytes(location.start, location.end - location.start)?;
        let value = runtime_type.parse(&bytes)?;
        Ok(value)
    }

    pub fn method_table(
        &self,
        ptr: TypedPointer<MethodTable>,
    ) -> Result<&MethodTable, Error> {
        self.state.method_table(ptr, self.reader)
    }

    pub fn runtime_module(
        &self,
        ptr: TypedPointer<RuntimeModule>,
    ) -> Result<&RuntimeModule, Error> {
        self.state.runtime_module(ptr, self.reader)
    }

    pub fn runtime_module_by_name(
        &self,
        name: &str,
    ) -> Result<TypedPointer<RuntimeModule>, Error> {
        self.state.runtime_module_by_name(name, self.reader)
    }

    pub fn field_to_runtime_type(
        &self,
        desc: &FieldDescription,
    ) -> Result<RuntimeType, Error> {
        self.state.field_to_runtime_type(desc, self.reader)
    }

    pub fn field_to_type_name(
        &self,
        desc: &FieldDescription,
    ) -> Result<&str, Error> {
        self.state.field_to_type_name(desc, self.reader)
    }

    pub fn field_to_name(
        &self,
        desc: &FieldDescription,
    ) -> Result<&str, Error> {
        self.state.field_to_name(desc, self.reader)
    }

    pub fn field_descriptions(
        &self,
        ptr: TypedPointer<MethodTable>,
    ) -> Result<Option<&FieldDescriptions>, Error> {
        self.state.field_descriptions(ptr, self.reader)
    }

    pub fn method_table_by_metadata(
        &self,
        module: TypedPointer<RuntimeModule>,
        coded_index: MetadataCodedIndex<TypeDefOrRef>,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        self.state
            .method_table_by_metadata(module, coded_index, self.reader)
    }

    pub fn method_table_to_name(
        &self,
        ptr: TypedPointer<MethodTable>,
    ) -> Result<&str, Error> {
        self.state.method_table_to_name(ptr, self.reader)
    }

    pub fn iter_fields(
        &self,
        ptr: TypedPointer<MethodTable>,
    ) -> Result<impl Iterator<Item = FieldDescription<'a>> + 'a, Error> {
        self.state.iter_fields(ptr, self.reader)
    }
}

impl<'a> Borrow<MemoryReader> for CachedReader<'a> {
    fn borrow(&self) -> &MemoryReader {
        self.reader
    }
}

impl<'a, 'b> Borrow<MemoryReader> for &'b CachedReader<'a> {
    fn borrow(&self) -> &MemoryReader {
        self.reader
    }
}
