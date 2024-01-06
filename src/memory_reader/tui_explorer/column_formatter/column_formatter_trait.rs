use itertools::Itertools as _;

use crate::{
    memory_reader::{MemoryRegion, MemoryValue},
    MemoryReader,
};

pub trait ColumnFormatter {
    fn name(&self) -> &'static str;

    fn format(
        &self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String;
}

pub struct AddressColumn;
pub struct HexColumn;
pub struct AsciiColumn;
pub struct PointsToColumn;

impl ColumnFormatter for AddressColumn {
    fn name(&self) -> &'static str {
        "Address"
    }

    fn format(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        format!("{}", row.location)
    }
}

impl ColumnFormatter for HexColumn {
    fn name(&self) -> &'static str {
        "Hex"
    }

    fn format(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        row.value.iter().map(|byte| format!("{byte:02x}")).join("")
    }
}

impl ColumnFormatter for AsciiColumn {
    fn name(&self) -> &'static str {
        "ASCII"
    }

    fn format(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
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
}

impl ColumnFormatter for PointsToColumn {
    fn name(&self) -> &'static str {
        "PointsTo"
    }

    fn format(
        &self,
        reader: &MemoryReader,
        _region: &MemoryRegion,
        row: &MemoryValue<[u8; MemoryRegion::POINTER_SIZE]>,
    ) -> String {
        reader
            .find_containing_region(row.value.into())
            .map(|pointed_region| pointed_region.short_name())
            .unwrap_or_default()
    }
}
