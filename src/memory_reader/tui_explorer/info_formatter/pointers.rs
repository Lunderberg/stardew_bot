use std::fmt::Display;

use crate::{
    memory_reader::{MemoryRegion, Pointer},
    MemoryReader,
};

use super::FormatFromPointer;

pub struct FormatRegionPointedTo;
pub struct FormatPointerOffset;

impl FormatFromPointer for FormatRegionPointedTo {
    fn format(
        &self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display> {
        let data = region.bytes_at_pointer(location)?;
        let pointer: Pointer = data.value.into();
        reader
            .find_containing_region(pointer)
            .map(|pointed_region| pointed_region.short_name())
    }
}

impl FormatFromPointer for FormatPointerOffset {
    fn format(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display> {
        let pointer = region.bytes_at_pointer(location)?.value.into();
        region.contains(pointer).then(|| pointer - location)
    }
}
