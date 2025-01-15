use std::ops::Range;

use memory_reader::{ByteRange, MemoryReader, Pointer};

use crate::{
    unpack_fields, Error, MethodTable, ReadTypedPointer, RuntimeType,
    TypedPointer,
};

pub struct RuntimeArray {
    start: Pointer,
    stride: usize,
    num_elements: usize,
    element_type: RuntimeType,
}

pub(crate) struct RuntimeArrayHeader<'a> {
    bytes: ByteRange<'a>,
}
impl<'a> RuntimeArrayHeader<'a> {
    pub(crate) const SIZE: usize = Pointer::SIZE + 8;

    pub(crate) fn new(bytes: ByteRange<'a>) -> Self {
        Self { bytes }
    }

    unpack_fields! {
        method_table: {TypedPointer<MethodTable>, 0..Pointer::SIZE},
        raw_num_elements: {u64, Pointer::SIZE..Pointer::SIZE+8},
    }

    pub(crate) fn num_elements(&self) -> usize {
        self.raw_num_elements() as usize
    }
}

impl RuntimeArray {
    pub const HEADER_SIZE: usize = Pointer::SIZE + 8;

    pub fn ptr_range(&self) -> Range<Pointer> {
        self.start..self.start + self.size_bytes()
    }

    pub fn size_bytes(&self) -> usize {
        Self::HEADER_SIZE + self.num_elements * self.stride
    }

    pub fn num_elements(&self) -> usize {
        self.num_elements
    }

    pub fn element_type(&self) -> &RuntimeType {
        &self.element_type
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

        let header = RuntimeArrayHeader::new(
            bytes.subrange(0..RuntimeArrayHeader::SIZE),
        );

        let method_table_ptr = header.method_table();
        let num_elements = header.num_elements();

        let method_table = method_table_ptr.read(reader)?;
        if !method_table.is_array() {
            return Err(Error::ArrayNotMarkedAsArray(
                method_table.runtime_type(reader)?,
            ));
        }
        if method_table.component_size().is_none() {
            return Err(Error::ArrayMissingComponentSize);
        }

        let stride = method_table
            .component_size()
            .ok_or(Error::ArrayMissingComponentSize)?;

        let element_type = method_table
            .array_element_type()
            .ok_or(Error::ArrayMissingElementType)?
            .read(reader)?
            .runtime_type(reader)?;

        Ok(Self {
            start: ptr,
            stride,
            element_type,
            num_elements,
        })
    }
}
