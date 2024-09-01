use memory_reader::{MemoryReader, Pointer};

use crate::{Error, MethodTable, ReadTypedPointer, TypedPointer};

pub struct RuntimeObject {
    location: TypedPointer<RuntimeObject>,
    method_table: TypedPointer<MethodTable>,
}

impl RuntimeObject {
    pub fn new(
        location: TypedPointer<RuntimeObject>,
        method_table: TypedPointer<MethodTable>,
    ) -> Self {
        Self {
            location: location.into(),
            method_table: method_table.into(),
        }
    }

    pub fn read(
        location: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        let method_table: Pointer = reader.read_byte_array(location)?.into();
        Ok(Self::new(location.into(), method_table.into()))
    }

    pub fn location(&self) -> TypedPointer<RuntimeObject> {
        self.location
    }

    pub fn method_table(&self) -> TypedPointer<MethodTable> {
        self.method_table
    }
}

impl ReadTypedPointer for RuntimeObject {
    fn read_typed_ptr(
        ptr: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        RuntimeObject::read(ptr, reader)
    }
}
