use ratatui::text::Line;

use memory_reader::{MemoryRegion, MemoryValue};

use crate::TuiGlobals;

pub(crate) trait ColumnFormatter {
    fn name(&self) -> &'static str;

    fn cell_text(
        &self,
        globals: &TuiGlobals,
        region: &MemoryRegion,
        selected_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
        printed_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String;

    fn formatted_cell(
        &self,
        globals: &TuiGlobals,
        region: &MemoryRegion,
        selected_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
        printed_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> Line {
        self.cell_text(globals, region, selected_row, printed_row)
            .into()
    }
}
