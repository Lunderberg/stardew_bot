use memory_reader::{MemoryRegion, MemoryValue, Pointer};

use crate::{extended_tui::WidgetGlobals, ColumnFormatter};

pub struct PointsToColumn;

impl ColumnFormatter for PointsToColumn {
    fn name(&self) -> &'static str {
        "PointsTo"
    }

    fn cell_text(
        &self,
        globals: WidgetGlobals,
        _region: &MemoryRegion,
        _pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        globals
            .reader
            .find_containing_region(row.value.into())
            .map(|pointed_region| pointed_region.short_name())
            .unwrap_or_default()
            .to_string()
    }
}
