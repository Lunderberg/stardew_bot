use itertools::Itertools as _;
use ratatui::{
    style::{Color, Style},
    text::{Line, Span},
};

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

pub struct AddressColumn;
pub struct HexColumn;
pub struct AsciiColumn;
pub struct PointsToColumn;

impl ColumnFormatter for AddressColumn {
    fn name(&self) -> &'static str {
        "Address"
    }

    fn cell_text(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        _pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        format!("{}", row.location)
    }
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

        // TODO: De-dup the repeated parts between this and AsciiColumn
        if let Some(value_at_pointer) = region.bytes_at_pointer(pointed_to) {
            let as_pointer: Pointer = value_at_pointer.value.into();
            if row.location <= as_pointer
                && as_pointer < row.location + MemoryRegion::POINTER_SIZE
            {
                let mut chars = text.chars();

                let raw: String = chars
                    .by_ref()
                    .take(2 * (as_pointer - row.location))
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

        if let Some(value_at_pointer) = region.bytes_at_pointer(pointed_to) {
            let as_pointer: Pointer = value_at_pointer.value.into();
            if row.location <= as_pointer
                && as_pointer < row.location + MemoryRegion::POINTER_SIZE
            {
                let mut chars = text.chars();

                let raw: String =
                    chars.by_ref().take(as_pointer - row.location).collect();
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
}

impl ColumnFormatter for PointsToColumn {
    fn name(&self) -> &'static str {
        "PointsTo"
    }

    fn cell_text(
        &self,
        reader: &MemoryReader,
        _region: &MemoryRegion,
        _pointed_to: Pointer,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        reader
            .find_containing_region(row.value.into())
            .map(|pointed_region| pointed_region.short_name())
            .unwrap_or_default()
    }
}
