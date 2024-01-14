use memory_reader::{MemoryReader, MemoryRegion, Pointer};

use crate::InfoFormatter;

pub struct FormatNullTerminatedString;
pub struct FormatStringPointerWithLength;
pub struct FormatStringPointerNullTerminated;

impl InfoFormatter for FormatNullTerminatedString {
    fn name(&self) -> &'static str {
        "UTF-8"
    }

    fn format(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<String> {
        region
            .iter_from_pointer(location)
            .take_while(|byte| byte.value > 0)
            .last()
            .map(|byte| &region[location..=byte.location])
            .and_then(|slice| std::str::from_utf8(slice).ok())
            .map(|str| str.to_string())
    }
}

impl InfoFormatter for FormatStringPointerWithLength {
    fn name(&self) -> &'static str {
        "(char*,len)"
    }

    fn format(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<String> {
        let pointer = region.bytes_at_pointer(location)?.value.into();
        if !region.contains(pointer) {
            return None;
        }

        let len_location = location - MemoryRegion::POINTER_SIZE;
        let len =
            usize::from_ne_bytes(region.bytes_at_pointer(len_location)?.value);

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

impl InfoFormatter for FormatStringPointerNullTerminated {
    fn name(&self) -> &'static str {
        "char*"
    }

    fn format(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<String> {
        let pointer = region.bytes_at_pointer(location)?.value.into();
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
