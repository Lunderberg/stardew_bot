use ratatui::{
    style::{Color, Style},
    text::{Line, Span},
};

use memory_reader::{MemoryReader, MemoryRegion, MemoryValue, Pointer};

use crate::extensions::*;
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
    pointer_at_cursor: Option<Pointer>,
    annotations: &[Annotation],
) -> Line<'static> {
    let row_loc = row.location..row.location + MemoryRegion::POINTER_SIZE;

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
                        Color::Indexed(235)
                    } else {
                        Color::Indexed(240)
                    }
                });

            let fg_color = pointer_at_cursor
                .filter(|ptr| {
                    row_loc.contains(ptr) && *ptr - row_loc.start <= i_byte
                })
                .map(|_| Color::LightRed);

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
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        _pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        cell_text::<Self>(row.value)
    }

    fn formatted_cell(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        pointed_to: Pointer,
        annotations: &[Annotation],
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> Line {
        let pointer_at_cursor = region
            .bytes_at_pointer(pointed_to)
            .map(|bytes| bytes.value.into());

        formatted_cell::<Self>(row, pointer_at_cursor, annotations)
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
        cell_text::<Self>(row.value)
    }

    fn formatted_cell(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        pointed_to: Pointer,
        annotations: &[Annotation],
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> Line {
        let pointer_at_cursor = region
            .bytes_at_pointer(pointed_to)
            .map(|bytes| bytes.value.into());

        formatted_cell::<Self>(row, pointer_at_cursor, annotations)
    }
}
