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
        _region: &MemoryRegion,
        _pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        format!("{}", row.location)
    }
}
