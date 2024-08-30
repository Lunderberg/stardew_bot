use memory_reader::{MemoryReader, Pointer};

use crate::Error;

pub struct RuntimeObject {
    location: Pointer,
    method_table: Pointer,
}

impl RuntimeObject {
    pub fn new(location: Pointer, method_table: Pointer) -> Self {
        Self {
            location,
            method_table,
        }
    }

    pub fn read(
        location: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        let method_table = reader.read_byte_array(location)?.into();
        Ok(Self::new(location, method_table))
    }

    pub fn method_table(&self) -> Pointer {
        self.method_table
    }
}
