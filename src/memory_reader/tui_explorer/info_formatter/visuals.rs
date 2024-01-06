use std::fmt::Display;

use crate::{
    memory_reader::{MemoryRegion, Pointer},
    MemoryReader,
};

use super::FormatFromPointer;

pub struct FormatSpacer;
pub struct FormatLocation;

impl FormatFromPointer for FormatSpacer {
    fn format(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        _location: Pointer,
    ) -> Option<impl Display> {
        Some("")
    }
}

impl FormatFromPointer for FormatLocation {
    fn format(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display> {
        Some(location)
    }
}
