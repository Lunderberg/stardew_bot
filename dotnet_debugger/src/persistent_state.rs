use std::collections::HashMap;

use memory_reader::{MemoryReader, Pointer};

use crate::{extensions::*, RuntimeModule};
use crate::{Error, MethodTable, RuntimeObject};

#[derive(Default)]
pub struct PersistentState {
    method_tables: HashMap<Pointer, MethodTable>,
    runtime_modules: HashMap<Pointer, RuntimeModule>,
}

pub struct CachedReader<'a> {
    state: &'a mut PersistentState,
    reader: &'a MemoryReader,
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
        location: Pointer,
    ) -> Result<RuntimeObject, Error> {
        RuntimeObject::read(location, self.reader)
    }

    pub fn class_name(&mut self, obj: &RuntimeObject) -> Result<String, Error> {
        let Self { state, reader } = self;
        let method_table_ptr = obj.method_table();

        let method_table = state
            .method_tables
            .entry(method_table_ptr)
            .or_try_insert(|ptr| MethodTable::read(*ptr, reader))?;

        let module = state
            .runtime_modules
            .entry(method_table.module())
            .or_try_insert(|ptr| RuntimeModule::read(*ptr, reader))?;
        let metadata = module.metadata()?;

        let name = metadata.get(method_table.token())?.name()?;

        Ok(name.to_string())
    }

    pub fn iter_fields(
        &mut self,
        obj: &RuntimeObject,
    ) -> Result<Vec<String>, Error> {
        let Self { state, reader } = self;
        let method_table_ptr = obj.method_table();

        let method_table = state
            .method_tables
            .entry(method_table_ptr)
            .or_try_insert(|ptr| MethodTable::read(*ptr, reader))?;

        let module = state
            .runtime_modules
            .entry(method_table.module())
            .or_try_insert(|ptr| RuntimeModule::read(*ptr, reader))?;
        let metadata = module.metadata()?;

        method_table
            .get_field_descriptions(reader)?
            .iter()
            .flatten()
            .map(|field| -> Result<String, Error> {
                Ok(metadata.get(field.token())?.name()?.to_string())
            })
            .collect()
    }
}
