use std::ops::Range;

use memory_reader::{MemoryReader, Pointer};

use crate::{Error, MethodTable, ReadTypedPointer, RuntimeType, TypedPointer};

pub struct RuntimeArray {
    start: Pointer,
    stride: usize,
    num_elements: usize,
    element_type: RuntimeType,
}

impl RuntimeArray {
    const HEADER_SIZE: usize = Pointer::SIZE + 8;

    pub fn ptr_range(&self) -> Range<Pointer> {
        self.start..self.start + self.size_bytes()
    }

    pub fn size_bytes(&self) -> usize {
        let stride = self.element_type.size_bytes();
        Self::HEADER_SIZE + self.num_elements * stride
    }

    pub fn num_elements(&self) -> usize {
        self.num_elements
    }

    pub fn element_type(&self) -> RuntimeType {
        self.element_type
    }

    pub fn element_location(&self, index: usize) -> Range<Pointer> {
        let start = self.start + Self::HEADER_SIZE + index * self.stride;
        start..start + self.stride
    }
}

impl ReadTypedPointer for RuntimeArray {
    fn read_typed_ptr(
        ptr: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        const SHORT_READ: usize = 64;

        let bytes = reader.read_bytes(ptr..ptr + SHORT_READ)?;

        let method_table: TypedPointer<MethodTable> =
            bytes.subrange(0..Pointer::SIZE).unpack()?;
        let num_elements = bytes
            .subrange(Pointer::SIZE..Pointer::SIZE + 8)
            .unpack::<u64>()? as usize;

        let method_table = method_table.read(reader)?;
        assert!(method_table.is_array());
        assert!(method_table.component_size().is_some());

        let stride = method_table
            .component_size()
            .ok_or(Error::ArrayMissingComponentSize)?;

        let element_type_ptr = method_table
            .array_element_type()
            .ok_or(Error::ArrayMissingElementType)?;
        let element_type =
            element_type_ptr.read(reader)?.runtime_type(reader)?;

        Ok(Self {
            start: ptr,
            stride,
            element_type,
            num_elements,
        })
    }
}
