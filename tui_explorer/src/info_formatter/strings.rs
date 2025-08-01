use itertools::Itertools as _;
use memory_reader::extensions::*;
use memory_reader::{MemoryReader, MemoryRegion, Pointer};

use crate::InfoFormatter;

pub struct FormatNullTerminatedString;
pub struct FormatStringPointerWithLength;
pub struct FormatStringPointerNullTerminated;
pub struct FormatUTF16String;

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
        let is_str_char = |byte: u8| -> bool {
            let c: char = byte.into();
            (c.is_ascii_graphic() || c.is_ascii_whitespace())
                && !c.is_ascii_control()
        };

        region
            .iter_from_pointer(location)
            .take_while_inclusive(|byte| {
                byte.value > 0 && is_str_char(byte.value)
            })
            .last()
            .filter(|byte| byte.location > location)
            .map(|byte| &region[location..byte.location])
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

        if len > 1024 * 1024 {
            return None;
        }

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

impl InfoFormatter for FormatUTF16String {
    fn name(&self) -> &'static str {
        "UTF-16"
    }

    fn format(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<String> {
        let u16_iter = region
            .iter_from_pointer(location)
            .map(|byte| byte.value)
            .iter_as::<[u8; 2]>()
            .map(u16::from_be_bytes);

        let out: String = char::decode_utf16(u16_iter)
            .map_while(|res| res.ok())
            .take_while(|c| c.is_ascii() && !c.is_ascii_control())
            .collect();
        if out.is_empty() {
            None
        } else {
            Some(out)
        }
    }
}
