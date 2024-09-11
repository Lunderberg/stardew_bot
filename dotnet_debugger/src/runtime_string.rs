use itertools::Itertools;
use memory_reader::{MemoryReader, Pointer};

use crate::{Error, ReadTypedPointer};

pub struct RuntimeString(
    // This field is used, but only in the implementation of Display.
    #[allow(dead_code)] String,
);

impl ReadTypedPointer for RuntimeString {
    fn read_typed_ptr(
        ptr: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        const SHORT_READ: usize = 32;

        // Advance past the System.String method table
        let ptr = ptr + Pointer::SIZE;

        let bytes = reader.read_bytes(ptr..ptr + SHORT_READ)?;
        let num_u16_code_units =
            i32::from_ne_bytes(bytes[0..4].try_into().unwrap());
        if num_u16_code_units < 0 {
            return Err(Error::NegativeStringLength(num_u16_code_units));
        }

        let num_str_bytes = (num_u16_code_units as usize) * 2;
        let num_total_bytes = num_str_bytes + 4;

        let bytes = if num_total_bytes <= SHORT_READ {
            bytes
        } else {
            reader.read_bytes(ptr..ptr + num_total_bytes)?
        };

        let iter_u16 = bytes
            .subrange(4..num_total_bytes)
            .bytes()
            .iter()
            .copied()
            .tuples()
            .map(|(a, b)| u16::from_ne_bytes([a, b]));

        let string =
            char::decode_utf16(iter_u16).collect::<Result<String, _>>()?;

        Ok(RuntimeString(string))
    }
}

impl Into<String> for RuntimeString {
    fn into(self) -> String {
        self.0
    }
}
