use std::borrow::Borrow;
use std::cell::OnceCell;
use std::ops::Range;

use dll_unpacker::MetadataCodedIndex;
use dll_unpacker::MetadataRow;
use dll_unpacker::TypeDefOrRef;
use dll_unpacker::TypeRef;
use elsa::FrozenMap;
use memory_reader::MemoryReader;
use memory_reader::Pointer;

use crate::extensions::*;
use crate::CorElementType;
use crate::FieldDescriptions;
use crate::RuntimeValue;
use crate::{Error, MethodTable, RuntimeObject};
use crate::{RuntimeModule, TypedPointer};

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
                    RuntimeModule::locate(&metadata, module_vtable, &reader)?;

                Ok(module_ptr)
            })
            .copied()
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
        runtime_type: CorElementType,
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

    pub fn class_name(&self, obj: &RuntimeObject) -> Result<String, Error> {
        let Self { state, reader } = *self;
        let method_table_ptr = obj.method_table();

        let method_table = state.method_table(method_table_ptr, reader)?;

        let module = state.runtime_module(method_table.module(), reader)?;

        let metadata = module.metadata(reader)?;

        let name = metadata.get(method_table.token())?.name()?;

        Ok(name.to_string())
    }

    pub fn iter_fields(
        &self,
        obj: &RuntimeObject,
    ) -> Result<
        impl Iterator<
                Item = (
                    &RuntimeModule,
                    crate::FieldDescription,
                    dll_unpacker::MetadataRow<dll_unpacker::Field>,
                ),
            > + '_,
        Error,
    > {
        let Self { state, reader } = *self;
        let method_table_ptr = obj.method_table();

        let method_table = state.method_table(method_table_ptr, reader)?;

        let module = state.runtime_module(method_table.module(), reader)?;
        let metadata = module.metadata(reader)?;

        let field_descriptions =
            state.field_descriptions(method_table_ptr, reader)?;

        let iter = field_descriptions.into_iter().flatten();

        let iter = iter.map(move |field| {
            // TODO: Add validation of the RuntimeModule against the
            // unpacked metadata.  That way, the panic that would
            // occur here from an out-of-bounds metadata token could
            // instead be caught earlier.
            let metadata_row = metadata.get(field.token()).unwrap();
            (module, field, metadata_row)
        });

        Ok(iter)
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
