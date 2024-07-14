use std::ops::Range;

use itertools::Itertools as _;
use ratatui::{
    style::{Color, Style},
    text::Line,
};

use memory_reader::{MemoryReader, MemoryRegion, MemoryValue, Pointer};

use crate::extensions::*;
use crate::{Annotation, ColumnFormatter};

pub struct HexColumn;
pub struct AsciiColumn;

fn formatted_cell(
    text: String,
    text_loc: Range<Pointer>,
    pointer_at_cursor: Option<Pointer>,
    annotations: &[Annotation],
) -> Line<'static> {
    let num_chars = text.chars().count();
    let chars_per_byte = num_chars / (text_loc.end - text_loc.start);
    assert!(chars_per_byte > 0);
    assert!(num_chars % chars_per_byte == 0);

    let mut line: Line = text.into();

    if let Some(as_pointer) = pointer_at_cursor {
        if text_loc.contains(&as_pointer) {
            let offset = chars_per_byte * (as_pointer - text_loc.start);
            let regex = format!(".{{{offset}}}(?<highlight>.*)");
            line =
                line.style_regex(regex, Style::default().fg(Color::LightRed));
        }
    }

    assert!(text_loc.end > text_loc.start);

    line = annotations
        .iter()
        .dedup_by(|a, b| a.range == b.range)
        .enumerate()
        .filter(|(_, ann)| {
            ann.range.start < text_loc.end && text_loc.start < ann.range.end
        })
        .fold(line, |line, (i, ann)| {
            let style = Style::default().bg(if i % 2 == 0 {
                Color::Indexed(250)
            } else {
                Color::Indexed(244)
            });
            // let style = Style::default().bg(Color::Green);

            let start_byte =
                text_loc.start.max(ann.range.start) - text_loc.start;
            let end_byte = text_loc.end.min(ann.range.end) - text_loc.start;
            let len_bytes = end_byte - start_byte;
            assert!(len_bytes > 0);
            let start_char = chars_per_byte * start_byte;
            let len_chars = chars_per_byte * len_bytes;

            assert!(len_chars > 0);

            let regex =
                format!("^.{{{start_char}}}(?<highlight>.{{{len_chars}}}).*");
            // let regex = format!("^.{{{start_char}}}(?<highlight>.{{4}}).*");
            // let regex = ".*";

            line.style_regex(regex, style)
        });

    line
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
        annotations: &[Annotation],
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> Line {
        let text = self.cell_text(reader, region, pointed_to, row);
        let pointer_at_cursor = region
            .bytes_at_pointer(pointed_to)
            .map(|bytes| bytes.value.into());

        formatted_cell(
            text,
            row.location..row.location + MemoryRegion::POINTER_SIZE,
            pointer_at_cursor,
            annotations,
        )
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
        annotations: &[Annotation],
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> Line {
        let text = self.cell_text(reader, region, pointed_to, row);
        let pointer_at_cursor = region
            .bytes_at_pointer(pointed_to)
            .map(|bytes| bytes.value.into());

        formatted_cell(
            text,
            row.location..row.location + MemoryRegion::POINTER_SIZE,
            pointer_at_cursor,
            annotations,
        )
    }
}
