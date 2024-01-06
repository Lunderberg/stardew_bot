use std::fmt::Display;

use crate::{
    memory_reader::{MemoryRegion, Pointer},
    MemoryReader,
};

use super::FormatFromPointer;

pub struct FormatNullTerminatedString;
pub struct FormatStringPointerWithLength;
pub struct FormatStringPointerNullTerminated;

impl FormatFromPointer for FormatNullTerminatedString {
    fn format(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display> {
        region
            .iter_from_pointer(location)
            .take_while(|byte| byte.value > 0)
            .last()
            .map(|byte| &region[location..=byte.location])
            .and_then(|slice| std::str::from_utf8(slice).ok())
    }
}

impl FormatFromPointer for FormatStringPointerWithLength {
    fn format(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display> {
        let pointer = region.bytes_at_pointer(location).value.into();
        if !region.contains(pointer) {
            return None;
        }

        let len_location = location - std::mem::size_of::<usize>();
        if !region.contains(len_location) {
            return None;
        }
        let len =
            usize::from_ne_bytes(region.bytes_at_pointer(len_location).value);

        let last_char = pointer + len - 1;
        if !region.contains(last_char) {
            return None;
        }

        let str = region
            .iter_from_pointer(pointer)
            .map(|mem_byte| mem_byte.value)
            .take(len)
            .take_while(|&byte| byte > 0)
            .map(|byte| byte as char)
            .collect::<String>();

        Some(str)
    }
}

impl FormatFromPointer for FormatStringPointerNullTerminated {
    fn format(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display> {
        let pointer = region.bytes_at_pointer(location).value.into();
        if !region.contains(pointer) {
            return None;
        }

        region
            .iter_from_pointer(pointer)
            .map(|mem_byte| mem_byte.value)
            .take_while(|&byte| byte > 0)
            .map(|byte| byte as char)
            .map(|c| {
                Some(c)
                    .filter(|c| c.is_ascii())
                    .filter(|c| !c.is_ascii_control())
            })
            .collect::<Option<String>>()
            .filter(|str| str.len() > 2)
    }
}
