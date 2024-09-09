use std::ops::Range;

use super::{NormalizeOffset, NormalizeRange};
use crate::{Error, Pointer, UnpackedValue};

#[derive(Clone, Copy)]
pub struct ByteRange<'a> {
    pub(crate) start: Pointer,
    pub(crate) bytes: &'a [u8],
}

pub trait UnpackBytes<'a>: Sized {
    type Error;
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error>;
}

pub trait UnpackOptBytes<'a>: Sized {
    type Error;
    fn unpack_opt(bytes: ByteRange<'a>) -> Result<Option<Self>, Self::Error>;
}

impl<'a, T> UnpackBytes<'a> for Option<T>
where
    T: UnpackOptBytes<'a>,
{
    type Error = <T as UnpackOptBytes<'a>>::Error;
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error> {
        <T as UnpackOptBytes<'a>>::unpack_opt(bytes)
    }
}

impl<'a> ByteRange<'a> {
    pub fn new(start: Pointer, bytes: &'a [u8]) -> Self {
        Self { start, bytes }
    }

    pub fn null() -> Self {
        Self {
            start: Pointer::null(),
            bytes: &[],
        }
    }

    pub fn ptr_range(&self) -> Range<Pointer> {
        self.start..self.start + self.bytes.len()
    }

    pub fn bytes(&self) -> &'a [u8] {
        self.bytes
    }

    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    pub fn unpack<T>(&self) -> Result<T, T::Error>
    where
        T: UnpackBytes<'a>,
    {
        T::unpack(*self)
    }

    pub fn get_u8(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<u8>, Error> {
        let byte_offset = loc.as_offset(self.start);
        let ptr = loc.as_ptr(self.start);
        let value = self.bytes[byte_offset];
        Ok(UnpackedValue::new(ptr..ptr + 1, value))
    }

    pub fn get_u16(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<u16>, Error> {
        let byte_offset = loc.as_offset(self.start);
        let byte_range = byte_offset..byte_offset + 2;
        self.subrange(byte_range).unpack()
    }

    pub fn get_u32(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<u32>, Error> {
        let byte_offset = loc.as_offset(self.start);
        let byte_range = byte_offset..byte_offset + 4;
        self.subrange(byte_range).unpack()
    }

    pub fn get_u64(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<u64>, Error> {
        let byte_offset = loc.as_offset(self.start);
        let byte_range = byte_offset..byte_offset + 8;
        let value = u64::from_le_bytes(
            self.bytes[byte_range.clone()].try_into().unwrap(),
        );
        Ok(UnpackedValue::new(self.address_range(byte_range), value))
    }

    pub fn get_null_terminated(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<&'a str>, Error> {
        let start = loc.as_offset(self.start);
        let size = self.bytes[start..]
            .iter()
            .enumerate()
            .take_while(|(_, byte)| **byte > 0)
            .map(|(i, _)| i + 1)
            .last()
            .unwrap_or(0);
        let value = std::str::from_utf8(&self.bytes[start..start + size])?;
        let loc = self.address_range(start..start + size);
        Ok(UnpackedValue::new(loc, value))
    }

    pub fn subrange(&self, range: impl NormalizeRange) -> Self {
        let range = range.as_offset(self.start..self.end());
        Self {
            start: self.start + range.start,
            bytes: &self.bytes[range],
        }
    }

    pub fn start(&self) -> Pointer {
        self.start
    }

    pub fn end(&self) -> Pointer {
        self.start + self.bytes.len()
    }

    pub fn address_range(&self, range: impl NormalizeRange) -> Range<Pointer> {
        range.as_ptr(self.start..self.end())
    }
}

impl<'a> Into<Range<Pointer>> for ByteRange<'a> {
    fn into(self) -> std::ops::Range<Pointer> {
        self.ptr_range()
    }
}

impl<'a> Into<&'a [u8]> for ByteRange<'a> {
    fn into(self) -> &'a [u8] {
        self.bytes
    }
}

impl<'a, T: NormalizeOffset> std::ops::Index<T> for ByteRange<'a> {
    type Output = u8;

    fn index(&self, index: T) -> &Self::Output {
        &self.bytes[index.as_offset(self.start)]
    }
}

impl<'a, T: NormalizeOffset> std::ops::Index<Range<T>> for ByteRange<'a> {
    type Output = [u8];

    fn index(&self, index: Range<T>) -> &Self::Output {
        let start = index.start.as_offset(self.start);
        let end = index.end.as_offset(self.start);
        &self.bytes[start..end]
    }
}

macro_rules! from_bytes_prim_uint {
    ($prim:ident) => {
        impl<'a> UnpackBytes<'a> for $prim {
            type Error = Error;
            fn unpack(bytes: ByteRange) -> Result<Self, Self::Error> {
                // Unwrapping instead of returning an error, because a
                // failure here means there's an inconsistency in the
                // unpacking.
                let value =
                    $prim::from_le_bytes(bytes.bytes.try_into().unwrap());
                Ok(value)
            }
        }
    };
}

from_bytes_prim_uint! {u8}
from_bytes_prim_uint! {u16}
from_bytes_prim_uint! {u32}
from_bytes_prim_uint! {u64}
from_bytes_prim_uint! {u128}

impl<'a> UnpackBytes<'a> for bool {
    type Error = Error;
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error> {
        let byte: u8 = bytes.unpack()?;
        Ok(byte > 0)
    }
}

impl<'a> UnpackBytes<'a> for Pointer {
    type Error = Error;
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error> {
        let arr: [u8; Pointer::SIZE] = bytes.bytes.try_into().unwrap();
        Ok(arr.into())
    }
}

impl<'a> UnpackBytes<'a> for usize {
    type Error = Error;
    fn unpack(bytes: ByteRange) -> Result<Self, Self::Error> {
        if bytes.len() == 2 {
            Ok(bytes.unpack::<u16>()? as usize)
        } else if bytes.len() == 4 {
            Ok(bytes.unpack::<u32>()? as usize)
        } else {
            panic!("Heap index is either u16 or u32");
        }
    }
}
