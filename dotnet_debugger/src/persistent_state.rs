use std::borrow::Borrow;
use std::cell::OnceCell;
use std::ops::Range;

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
                    .find(|region| {
                        region.short_name().ends_with(".dll")
                            && region.short_name().trim_end_matches(".dll")
                                == name
                    })
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
