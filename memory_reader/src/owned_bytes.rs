use std::ops::{Deref, Range};

use super::{NormalizeOffset, NormalizeRange};
use crate::{ByteRange, Pointer};

#[derive(Clone)]
pub struct OwnedBytes {
    start: Pointer,
    bytes: Vec<u8>,
}

impl OwnedBytes {
    pub fn new(start: Pointer, bytes: Vec<u8>) -> Self {
        Self { start, bytes }
    }

    pub fn subrange<'a>(&'a self, range: impl NormalizeRange) -> ByteRange<'a> {
        let byte_range: ByteRange<'a> = self.into();
        byte_range.subrange(range)
    }

    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    pub fn start(&self) -> Pointer {
        self.start
    }

    pub fn end(&self) -> Pointer {
        self.start + self.bytes.len()
    }
}

impl IntoIterator for OwnedBytes {
    type Item = <Vec<u8> as IntoIterator>::Item;
    type IntoIter = <Vec<u8> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.bytes.into_iter()
    }
}

impl<'a> Into<ByteRange<'a>> for &'a OwnedBytes {
    fn into(self) -> ByteRange<'a> {
        ByteRange::new(self.start, &self.bytes)
    }
}

impl<'a> Into<Range<Pointer>> for &'a OwnedBytes {
    fn into(self) -> Range<Pointer> {
        self.start..self.start + self.bytes.len()
    }
}

impl Deref for OwnedBytes {
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.bytes
    }
}

impl<T: NormalizeOffset> std::ops::Index<T> for OwnedBytes {
    type Output = u8;

    fn index(&self, index: T) -> &Self::Output {
        &self.bytes[index.as_offset(self.start)]
    }
}

impl<T: NormalizeOffset> std::ops::Index<Range<T>> for OwnedBytes {
    type Output = [u8];

    fn index(&self, index: Range<T>) -> &Self::Output {
        let start = index.start.as_offset(self.start);
        let end = index.end.as_offset(self.start);
        &self.bytes[start..end]
    }
}
