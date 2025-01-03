use memory_reader::{MemoryRegion, MemoryValue};
use tui_utils::TuiGlobals;

use crate::ColumnFormatter;

pub struct PointsToColumn;

impl ColumnFormatter for PointsToColumn {
    fn name(&self) -> &'static str {
        "PointsTo"
    }

    fn cell_text(
        &self,
        globals: &TuiGlobals,
        _region: &MemoryRegion,
        _selected_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
        printed_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        globals
            .reader()
            .find_containing_region(printed_row.value.into())
            .map(|pointed_region| pointed_region.short_name())
            .unwrap_or_default()
            .to_string()
    }
}
