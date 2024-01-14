use crate::{
    memory_reader::{MemoryRegion, Pointer},
    MemoryReader,
};

use super::InfoFormatter;

pub struct FormatSpacer;
pub struct FormatLocation;

impl InfoFormatter for FormatSpacer {
    fn name(&self) -> &'static str {
        ""
    }

    fn format(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        _location: Pointer,
    ) -> Option<String> {
        Some(String::default())
    }
}

impl InfoFormatter for FormatLocation {
    fn name(&self) -> &'static str {
        "Location"
    }

    fn format(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        location: Pointer,
    ) -> Option<String> {
        Some(format!("{location}"))
    }
}
