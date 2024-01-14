use crate::{
    memory_reader::{MemoryRegion, MemoryValue, Pointer},
    MemoryReader,
};

use super::ColumnFormatter;

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
