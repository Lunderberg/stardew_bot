use std::ops::Range;

use itertools::Itertools;
use memory_reader::{MemoryReader, OwnedBytes, Pointer, ReadTypedPointer};

use crate::Error;

pub struct RuntimeString(
    // This field is used, but only in the implementation of Display.
    #[allow(dead_code)] String,
);

impl RuntimeString {
    pub fn read_string<Reader, Error>(
        ptr: Pointer,
        mut read_bytes: Reader,
    ) -> Result<Self, Error>
    where
        Reader: FnMut(Range<Pointer>) -> Result<OwnedBytes, Error>,
        Error: From<crate::Error>,
    {
        const SHORT_READ: usize = 32;

        // Advance past the System.String method table
        let ptr = ptr
            .try_add(Pointer::SIZE)
            .map_err(|err| -> crate::Error { err.into() })?;

        let bytes = read_bytes(
            ptr..ptr
                .try_add(SHORT_READ)
                .map_err(|err| -> crate::Error { err.into() })?,
        )?;
        let num_u16_code_units =
            i32::from_ne_bytes(bytes[0..4].try_into().unwrap());
        if num_u16_code_units < 0 {
            return Err(
                crate::Error::NegativeStringLength(num_u16_code_units).into()
            );
        }

        let num_str_bytes = (num_u16_code_units as usize) * 2;
        let num_total_bytes = num_str_bytes + 4;

        let bytes = if num_total_bytes <= SHORT_READ {
            bytes
        } else {
            read_bytes(
                ptr..ptr
                    .try_add(num_total_bytes)
                    .map_err(|err| -> crate::Error { err.into() })?,
            )?
        };

        let iter_u16 = bytes
            .subrange(4..num_total_bytes)
            .bytes()
            .iter()
            .copied()
            .tuples()
            .map(|(a, b)| u16::from_ne_bytes([a, b]));

        let string = char::decode_utf16(iter_u16)
            .collect::<Result<String, _>>()
            .map_err(|err| -> crate::Error { err.into() })?;

        Ok(RuntimeString(string))
    }
}

impl ReadTypedPointer for RuntimeString {
    type Error = Error;

    fn read_typed_ptr(
        ptr: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        RuntimeString::read_string(ptr, |byte_range| -> Result<_, Error> {
            Ok(reader.read_bytes(byte_range)?)
        })
    }
}

impl From<RuntimeString> for String {
    fn from(val: RuntimeString) -> Self {
        val.0
    }
}
