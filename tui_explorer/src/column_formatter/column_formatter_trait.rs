use ratatui::text::Line;

use memory_reader::{MemoryRegion, MemoryValue, Pointer};

use crate::extended_tui::WidgetGlobals;

pub(crate) trait ColumnFormatter {
    fn name(&self) -> &'static str;

    fn cell_text(
        &self,
        globals: WidgetGlobals,
        region: &MemoryRegion,
        pointed_to: Pointer,
        printed_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String;

    fn formatted_cell(
        &self,
        globals: WidgetGlobals,
        region: &MemoryRegion,
        pointed_to: Pointer,
        printed_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> Line {
        self.cell_text(globals, region, pointed_to, printed_row)
            .into()
    }
}
