use memory_reader::{MemoryReader, MemoryRegion, Pointer};

use crate::InfoFormatter;

pub struct FormatRegionPointedTo;
pub struct FormatPointerOffset;

impl InfoFormatter for FormatRegionPointedTo {
    fn name(&self) -> &'static str {
        "Points to"
    }

    fn format(
        &self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<String> {
        let data = region.bytes_at_pointer(location)?;
        let pointer: Pointer = data.value.into();
        reader
            .find_containing_region(pointer)
            .map(|pointed_region| pointed_region.short_name().to_string())
    }
}

impl InfoFormatter for FormatPointerOffset {
    fn name(&self) -> &'static str {
        "Offset"
    }

    fn format(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<String> {
        let pointer = region.bytes_at_pointer(location)?.value.into();
        region.contains(pointer).then(|| {
            if location < pointer {
                format!("+{}", pointer - location)
            } else {
                format!("-{}", location - pointer)
            }
        })
    }
}
