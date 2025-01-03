use ratatui::{
    style::{Color, Style},
    text::{Line, Span},
};

use memory_reader::{MemoryRegion, MemoryValue, Pointer};
use tui_utils::TuiGlobals;

use crate::{Annotation, ColumnFormatter};

pub struct HexColumn;
pub struct AsciiColumn;

trait ByteFormatter {
    fn format_byte(byte: u8) -> impl Iterator<Item = char>;
}

impl ByteFormatter for HexColumn {
    fn format_byte(byte: u8) -> impl Iterator<Item = char> {
        [
            char::from_digit((byte / 16).into(), 16).unwrap(),
            char::from_digit((byte % 16).into(), 16).unwrap(),
        ]
        .into_iter()
    }
}

impl ByteFormatter for AsciiColumn {
    fn format_byte(byte: u8) -> impl Iterator<Item = char> {
        let c = char::from_u32(byte.into())
            .filter(|c| c.is_ascii() && !c.is_ascii_control())
            .unwrap_or('☒');
        std::iter::once(c)
    }
}

fn formatted_cell<Column: ByteFormatter>(
    row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    pointer_at_cursor: Pointer,
    annotations: &[Annotation],
) -> Line<'static> {
    let row_loc = row.location..row.location + MemoryRegion::POINTER_SIZE;

    let annotations =
        &annotations[annotations.partition_point(|ann| !ann.highlight_range)..];

    let annotation_slice_start = annotations
        .partition_point(|annotation| annotation.range.end < row_loc.start);
    let annotations = &annotations[annotation_slice_start..];
    let annotations = &annotations[..annotations
        .partition_point(|annotation| annotation.range.start < row_loc.end)];

    row.value
        .into_iter()
        .enumerate()
        .map(|(i_byte, byte)| -> Span {
            let byte_loc = row_loc.start + i_byte;
            let bg_color = annotations
                .iter()
                .enumerate()
                .find(|(_, ann)| ann.range.contains(&byte_loc))
                .map(|(i_ann, _)| {
                    if (i_ann + annotation_slice_start) % 2 == 0 {
                        Color::Rgb(150, 50, 30)
                    } else {
                        Color::Rgb(20, 20, 70)
                    }
                });

            let fg_color = if row_loc.contains(&pointer_at_cursor)
                && pointer_at_cursor - row_loc.start <= i_byte
            {
                Some(Color::LightRed)
            } else {
                None
            };

            let style = Style::default();
            let style = if let Some(fg) = fg_color {
                style.fg(fg)
            } else {
                style
            };
            let style = if let Some(bg) = bg_color {
                style.bg(bg)
            } else {
                style
            };

            Span::styled(Column::format_byte(byte).collect::<String>(), style)
        })
        .collect()
}

fn cell_text<Column: ByteFormatter>(
    row: [u8; MemoryRegion::POINTER_SIZE],
) -> String {
    row.into_iter()
        .flat_map(|byte| Column::format_byte(byte))
        .collect()
}

impl ColumnFormatter for HexColumn {
    fn name(&self) -> &'static str {
        "Hex"
    }

    fn cell_text(
        &self,
        _globals: &TuiGlobals,
        _region: &MemoryRegion,
        _selected_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
        printed_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        cell_text::<Self>(printed_row.value)
    }

    fn formatted_cell(
        &self,
        globals: &TuiGlobals,
        _region: &MemoryRegion,
        selected_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
        printed_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> Line {
        let pointer_at_cursor = selected_row.value.into();

        formatted_cell::<Self>(
            printed_row,
            pointer_at_cursor,
            &globals.get::<Vec<Annotation>>().unwrap(),
        )
    }
}

impl ColumnFormatter for AsciiColumn {
    fn name(&self) -> &'static str {
        "ASCII"
    }

    fn cell_text(
        &self,
        _globals: &TuiGlobals,
        _region: &MemoryRegion,
        _selected_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
        printed_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        cell_text::<Self>(printed_row.value)
    }

    fn formatted_cell(
        &self,
        globals: &TuiGlobals,
        _region: &MemoryRegion,
        selected_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
        printed_row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> Line {
        let pointer_at_cursor = selected_row.value.into();

        formatted_cell::<Self>(
            printed_row,
            pointer_at_cursor,
            &globals.get::<Vec<Annotation>>().unwrap(),
        )
    }
}
