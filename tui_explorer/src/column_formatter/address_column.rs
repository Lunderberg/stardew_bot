use memory_reader::extensions::*;
use memory_reader::{MemoryRegion, MemoryValue};

use crate::extended_tui::WidgetGlobals;
use crate::ColumnFormatter;

pub struct AddressColumn;

impl ColumnFormatter for AddressColumn {
    fn name(&self) -> &'static str {
        "Address"
    }

    fn cell_text(
        &self,
        _globals: WidgetGlobals,
        region: &MemoryRegion,
        _selected_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
        printed_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        let range = region.as_range();
        let prefix = range.prefix();
        let width = range.suffix_hexadecimal_digits() as usize;
        format!("{:#0width$x}", printed_row.location - prefix)
    }
}
