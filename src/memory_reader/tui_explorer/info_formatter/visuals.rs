use std::fmt::Display;

use crate::{
    memory_reader::{MemoryRegion, Pointer},
    MemoryReader,
};

use super::InfoFormatter;

pub struct FormatSpacer;
pub struct FormatLocation;

impl InfoFormatter for FormatSpacer {
    fn format(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        _location: Pointer,
    ) -> Option<impl Display> {
        Some("")
    }
}

impl InfoFormatter for FormatLocation {
    fn format(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display> {
        Some(location)
    }
}
