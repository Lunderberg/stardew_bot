use std::collections::HashMap;
use std::ops::Range;

use memory_reader::MemoryReader;
use memory_reader::Pointer;

use crate::extensions::*;
use crate::FieldDescriptions;
use crate::RuntimeType;
use crate::RuntimeValue;
use crate::{Error, MethodTable, RuntimeObject};
use crate::{ReadTypedPointer, RuntimeModule, TypedPointer};

#[derive(Default)]
pub struct PersistentState {
    method_tables: LookupCache<MethodTable>,
    runtime_modules: LookupCache<RuntimeModule>,

    // The field description can't use the LookupCache class, because
    // it requires additional information from the MethodTable.
    field_descriptions:
        HashMap<TypedPointer<MethodTable>, Option<FieldDescriptions>>,
}

pub struct CachedReader<'a> {
    state: &'a mut PersistentState,
    reader: &'a MemoryReader,
}

struct LookupCache<T> {
    cache: HashMap<TypedPointer<T>, T>,
}

impl PersistentState {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn cached_reader<'a>(
        &'a mut self,
        reader: &'a MemoryReader,
    ) -> CachedReader<'a> {
        CachedReader {
            state: self,
            reader,
        }
    }
}

impl<'a> CachedReader<'a> {
    pub fn object(
        &mut self,
        location: TypedPointer<RuntimeObject>,
    ) -> Result<RuntimeObject, Error> {
        location.read(self.reader)
    }

    pub fn value(
        &mut self,
        runtime_type: RuntimeType,
        location: Range<Pointer>,
    ) -> Result<RuntimeValue, Error> {
        let bytes = self
            .reader
            .read_bytes(location.start, location.end - location.start)?;
        let value = runtime_type.parse(&bytes)?;
        Ok(value)
    }

    pub fn class_name(&mut self, obj: &RuntimeObject) -> Result<String, Error> {
        let Self { state, reader } = self;
        let method_table_ptr = obj.method_table();

        let method_table = state
            .method_tables
            .cached_or_read(method_table_ptr, reader)?;

        let module = state
            .runtime_modules
            .cached_or_read(method_table.module(), reader)?;
        let metadata = module.metadata();

        let name = metadata.get(method_table.token())?.name()?;

        Ok(name.to_string())
    }

    pub fn iter_fields(
        &mut self,
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
        let Self { state, reader } = self;
        let method_table_ptr = obj.method_table();

        let method_table = state
            .method_tables
            .cached_or_read(method_table_ptr, reader)?;

        let module = state
            .runtime_modules
            .cached_or_read(method_table.module(), reader)?;
        let metadata = module.metadata();

        let field_descriptions = state
            .field_descriptions
            .entry(method_table_ptr)
            .or_try_insert(|_| method_table.get_field_descriptions(reader))?;

        let iter = field_descriptions.iter().flatten();

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

impl<T> LookupCache<T> {
    // pub fn cached_or_read<'a>(
    //     &'a mut self,
    //     ptr: TypedPointer<T>,
    //     reader: &MemoryReader,
    // ) -> Result<&'a mut T, Error>
    // where
    //     T: ReadTypedPointer,
    // {
    //     use std::collections::hash_map::Entry;
    //     match self.cache.entry(ptr) {
    //         Entry::Occupied(value) => Ok(value.into_mut()),
    //         Entry::Vacant(vacant) => Ok(vacant.insert(ptr.read(reader)?)),
    //     }
    // }

    pub fn cached_or_read<'a>(
        &'a mut self,
        ptr: TypedPointer<T>,
        reader: &MemoryReader,
    ) -> Result<&'a T, Error>
    where
        T: ReadTypedPointer,
    {
        use std::collections::hash_map::Entry;
        match self.cache.entry(ptr) {
            Entry::Occupied(value) => Ok(value.into_mut()),
            Entry::Vacant(vacant) => Ok(vacant.insert(ptr.read(reader)?)),
        }
    }
}

impl<T> std::ops::Deref for LookupCache<T> {
    type Target = HashMap<TypedPointer<T>, T>;

    fn deref(&self) -> &Self::Target {
        &self.cache
    }
}

impl<T> std::ops::DerefMut for LookupCache<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.cache
    }
}

impl<T> Default for LookupCache<T> {
    fn default() -> Self {
        Self {
            cache: Default::default(),
        }
    }
}
