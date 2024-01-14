use crate::{MemoryReader, MemoryRegion, Pointer};

pub trait InfoFormatter {
    fn name(&self) -> &'static str;

    fn format(
        &self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<String>;
}
