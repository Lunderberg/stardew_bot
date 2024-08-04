use memory_reader::Pointer;
use std::ops::Range;

use crate::{
    dll_unpacker::{UnpackMetadataFromBytes, VirtualRange},
    Error, UnpackedValue,
};

#[derive(Clone, Copy)]
pub struct ByteRange<'a> {
    pub(crate) start: Pointer,
    pub(crate) bytes: &'a [u8],
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

impl<'a> ByteRange<'a> {
    pub(crate) fn len(&self) -> usize {
        self.bytes.len()
    }

    pub(crate) fn unpack<T>(&self) -> Result<T, Error>
    where
        T: UnpackMetadataFromBytes<'a>,
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

    pub(crate) fn get_virtual_range(
        &self,
        loc: impl NormalizeOffset,
    ) -> Result<UnpackedValue<VirtualRange>, Error> {
        let start = loc.as_offset(self.start);
        let rva = self.get_u32(start)?.value;
        let size = self.get_u32(start + 4)?.value;
        Ok(UnpackedValue::new(
            self.address_range(start..start + 8),
            VirtualRange { rva, size },
        ))
    }

    pub(crate) fn subrange(&self, range: Range<impl NormalizeOffset>) -> Self {
        let start = range.start.as_offset(self.start);
        let end = range.end.as_offset(self.start);
        Self {
            start: self.start + start,
            bytes: &self.bytes[start..end],
        }
    }

    pub(crate) fn end(&self) -> Pointer {
        self.start + self.bytes.len()
    }

    pub(crate) fn address_range(
        &self,
        range: Range<impl NormalizeOffset>,
    ) -> Range<Pointer> {
        let start = range.start.as_ptr(self.start);
        let end = range.end.as_ptr(self.start);
        start..end
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
