use std::ops::Range;

use memory_reader::{MemoryReader, Pointer};

use crate::{Error, ReadTypedPointer, RuntimeArrayHeader, RuntimeType};

pub struct RuntimeMultiDimArray {
    location: Pointer,
    stride: usize,
    num_elements: usize,
    element_type: RuntimeType,
    shape: Vec<usize>,
    _lower_bounds: Vec<usize>,
}

impl RuntimeMultiDimArray {
    pub fn header_size(rank: usize) -> usize {
        // The header used by normal arrays, followed by the shape
        // (`rank` values of type u32), followed by the lower bounds
        // (another `rank` values of type u32).
        RuntimeArrayHeader::SIZE + 2 * rank * std::mem::size_of::<u32>()
    }

    pub fn read(ptr: Pointer, reader: &MemoryReader) -> Result<Self, Error> {
        // Without the rank, we don't know the exact size of the
        // header for a multi-dimensional array.  The multi-dim array
        // header starts with the same fields as a regular array
        // header (16 bytes, method table pointer followed by `u64`
        // number of elements), and is followed by 2*N `u32` fields,
        // specifying the value and lower bounds of each dimension.
        //
        // The header is at least 24 bytes long (since N >= 1).
        // Reading 64 bytes avoids needing to perform a second read so
        // long as the dimensionality is 6 or less.
        const SHORT_READ: usize = 64;

        let bytes = reader.read_bytes(ptr..ptr + SHORT_READ)?;

        let header = RuntimeArrayHeader::new(
            bytes.subrange(0..RuntimeArrayHeader::SIZE),
        );
        let method_table = header.method_table().read(reader)?;
        let num_elements = header.num_elements();

        if !method_table.is_multi_dim_array() {
            return Err(Error::MultiDimArrayNotMarkedAsArray(
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

        // TODO: Provide the statically-known rank so that it doesn't
        // need to be re-read at this point.
        let rank = method_table.get_ee_class(reader)?.multi_dim_rank();

        let full_header_size = Self::header_size(rank);

        let bytes = if full_header_size <= SHORT_READ {
            bytes
        } else {
            reader.read_bytes(ptr..ptr + full_header_size)?
        };

        let shape = (0..rank)
            .map(|i| RuntimeArrayHeader::SIZE + i * 4)
            .map(|offset| bytes.subrange(offset..offset + 4).unpack().unwrap())
            .map(|value: u32| value as usize)
            .collect();

        let lower_bounds = (0..rank)
            .map(|i| RuntimeArrayHeader::SIZE + (i + rank) * 4)
            .map(|offset| bytes.subrange(offset..offset + 4).unpack().unwrap())
            .map(|value: u32| value as usize)
            .collect();

        Ok(Self {
            location: ptr,
            stride,
            num_elements,
            element_type,
            shape,
            _lower_bounds: lower_bounds,
        })
    }

    pub fn element_type(&self) -> RuntimeType {
        self.element_type.clone()
    }

    pub fn num_elements(&self) -> usize {
        self.num_elements
    }

    pub fn shape(&self) -> &[usize] {
        &self.shape
    }

    fn ptr_to_first_value(&self) -> Pointer {
        self.location
            + RuntimeArrayHeader::SIZE
            + 2 * self.shape.len() * std::mem::size_of::<u32>()
    }

    pub fn ptr_range(&self) -> Range<Pointer> {
        let end = self.ptr_to_first_value() + self.num_elements * self.stride;
        self.location..end
    }

    pub fn element_location(&self, index: usize) -> Range<Pointer> {
        let start = self.ptr_to_first_value() + index * self.stride;
        start..start + self.stride
    }
}

impl ReadTypedPointer for RuntimeMultiDimArray {
    fn read_typed_ptr(
        ptr: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        Self::read(ptr, reader)
    }
}
