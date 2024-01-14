use ratatui::text::Line;

use crate::{
    memory_reader::{MemoryRegion, MemoryValue, Pointer},
    MemoryReader,
};

pub trait ColumnFormatter {
    fn name(&self) -> &'static str;

    fn cell_text(
        &self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String;

    fn formatted_cell(
        &self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> Line {
        self.cell_text(reader, region, pointed_to, row).into()
    }
}
