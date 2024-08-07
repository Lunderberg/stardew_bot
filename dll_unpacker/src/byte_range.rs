use memory_reader::Pointer;
use std::ops::{Range, RangeFrom, RangeTo};

use crate::{Error, UnpackedValue};

#[derive(Clone, Copy)]
pub struct ByteRange<'a> {
    pub(crate) start: Pointer,
    pub(crate) bytes: &'a [u8],
}

pub(crate) trait UnpackBytes<'a>: Sized {
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error>;
}

pub(crate) trait NormalizeOffset: Copy {
    fn as_offset(self, start: Pointer) -> usize;
    fn as_ptr(self, start: Pointer) -> Pointer;
}
impl NormalizeOffset for usize {
    fn as_offset(self, _start: Pointer) -> usize {
        self
    }

    fn as_ptr(self, start: Pointer) -> Pointer {
        start + self
    }
}
impl NormalizeOffset for Pointer {
    fn as_offset(self, start: Pointer) -> usize {
        self - start
    }

    fn as_ptr(self, _start: Pointer) -> Pointer {
        self
    }
}

pub(crate) trait NormalizeRange {
    fn as_offset(self, buf_range: Range<Pointer>) -> Range<usize>;
    fn as_ptr(self, buf_range: Range<Pointer>) -> Range<Pointer>;
}
impl<T> NormalizeRange for Range<T>
where
    T: NormalizeOffset,
{
    fn as_offset(self, buf_range: Range<Pointer>) -> Range<usize> {
        let start = self.start.as_offset(buf_range.start);
        let end = self.end.as_offset(buf_range.start);
        start..end
    }

    fn as_ptr(self, buf_range: Range<Pointer>) -> Range<Pointer> {
        let start = self.start.as_ptr(buf_range.start);
        let end = self.end.as_ptr(buf_range.start);
        start..end
    }
}

impl<T> NormalizeRange for RangeFrom<T>
where
    T: NormalizeOffset,
{
    fn as_offset(self, buf_range: Range<Pointer>) -> Range<usize> {
        let start = self.start.as_offset(buf_range.start);
        let end = buf_range.end - buf_range.start;
        start..end
    }

    fn as_ptr(self, buf_range: Range<Pointer>) -> Range<Pointer> {
        let start = self.start.as_ptr(buf_range.start);
        start..buf_range.end
    }
}

impl<T> NormalizeRange for RangeTo<T>
where
    T: NormalizeOffset,
{
    fn as_offset(self, buf_range: Range<Pointer>) -> Range<usize> {
        let end = self.end.as_offset(buf_range.start);
        0..end
    }

    fn as_ptr(self, buf_range: Range<Pointer>) -> Range<Pointer> {
        let end = self.end.as_ptr(buf_range.start);
        buf_range.start..end
    }
}

impl<'a> ByteRange<'a> {
    pub(crate) fn len(&self) -> usize {
        self.bytes.len()
    }

    pub(crate) fn unpack<T>(&self) -> Result<T, Error>
    where
        T: UnpackBytes<'a>,
    {
        T::unpack(*self)
    }

    pub(crate) fn get_u8(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<u8>, Error> {
        let byte_offset = loc.as_offset(self.start);
        let ptr = loc.as_ptr(self.start);
        let value = self.bytes[byte_offset];
        Ok(UnpackedValue::new(ptr..ptr + 1, value))
    }

    pub(crate) fn get_u16(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<u16>, Error> {
        let byte_offset = loc.as_offset(self.start);
        let byte_range = byte_offset..byte_offset + 2;
        self.subrange(byte_range).unpack()
    }

    pub(crate) fn get_u32(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<u32>, Error> {
        let byte_offset = loc.as_offset(self.start);
        let byte_range = byte_offset..byte_offset + 4;
        self.subrange(byte_range).unpack()
    }

    pub(crate) fn get_u64(
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

    pub(crate) fn get_null_terminated(
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

    pub(crate) fn subrange(&self, range: impl NormalizeRange) -> Self {
        let range = range.as_offset(self.start..self.end());
        Self {
            start: self.start + range.start,
            bytes: &self.bytes[range],
        }
    }

    pub(crate) fn end(&self) -> Pointer {
        self.start + self.bytes.len()
    }

    pub(crate) fn address_range(
        &self,
        range: impl NormalizeRange,
    ) -> Range<Pointer> {
        range.as_ptr(self.start..self.end())
    }
}

impl<'a> Into<Range<Pointer>> for ByteRange<'a> {
    fn into(self) -> std::ops::Range<Pointer> {
        self.start..self.start + self.bytes.len()
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
            fn unpack(bytes: ByteRange) -> Result<Self, Error> {
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
