use itertools::Itertools as _;
use ratatui::{
    style::{Color, Style},
    text::{Line, Span},
};

use memory_reader::{MemoryReader, MemoryRegion, MemoryValue, Pointer};

use crate::ColumnFormatter;

pub struct HexColumn;
pub struct AsciiColumn;

fn formatted_cell(
    text: String,
    text_loc: Pointer,
    pointer_at_cursor: Option<Pointer>,
    chars_per_byte: usize,
) -> Line<'static> {
    if let Some(as_pointer) = pointer_at_cursor {
        if text_loc <= as_pointer
            && as_pointer < text_loc + MemoryRegion::POINTER_SIZE
        {
            let mut chars = text.chars();

            let raw: String = chars
                .by_ref()
                .take(chars_per_byte * (as_pointer - text_loc))
                .collect();
            let styled: String = chars.collect();

            return vec![
                Span::raw(raw),
                Span::styled(styled, Style::default().fg(Color::LightRed)),
            ]
            .into();
        }
    }

    text.into()
}

impl ColumnFormatter for HexColumn {
    fn name(&self) -> &'static str {
        "Hex"
    }

    fn cell_text(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        _pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        row.value.iter().map(|byte| format!("{byte:02x}")).join("")
    }

    fn formatted_cell(
        &self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> Line {
        let text = self.cell_text(reader, region, pointed_to, row);
        let pointer_at_cursor = region
            .bytes_at_pointer(pointed_to)
            .map(|bytes| bytes.value.into());

        formatted_cell(text, row.location, pointer_at_cursor, 2)
    }
}

impl ColumnFormatter for AsciiColumn {
    fn name(&self) -> &'static str {
        "ASCII"
    }

    fn cell_text(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        _pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        row.value
            .iter()
            .map(|&byte| {
                char::from_u32(byte.into())
                    .filter(|c| c.is_ascii() && !c.is_ascii_control())
                    .unwrap_or('☒')
            })
            .collect()
    }

    fn formatted_cell(
        &self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> Line {
        let text = self.cell_text(reader, region, pointed_to, row);
        let pointer_at_cursor = region
            .bytes_at_pointer(pointed_to)
            .map(|bytes| bytes.value.into());

        formatted_cell(text, row.location, pointer_at_cursor, 1)
    }
}
