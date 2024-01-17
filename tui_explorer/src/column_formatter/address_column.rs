use memory_reader::extensions::*;
use memory_reader::{MemoryReader, MemoryRegion, MemoryValue, Pointer};

use crate::ColumnFormatter;

pub struct AddressColumn;

impl ColumnFormatter for AddressColumn {
    fn name(&self) -> &'static str {
        "Address"
    }

    fn cell_text(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        _pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        let range = region.as_range();
        let prefix = range.prefix();
        let width = range.suffix_hexadecimal_digits() as usize;
        format!("{:#0width$x}", row.location - prefix)
    }
}
