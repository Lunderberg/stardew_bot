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

    pub fn ptr_range(&self) -> Range<Pointer> {
        self.start..self.start + self.bytes.len()
    }

    pub fn contains_range(&self, range: impl NormalizeRange) -> bool {
        let byte_range: ByteRange = self.into();
        byte_range.contains_range(range)
    }

    pub fn subrange<'a>(&'a self, range: impl NormalizeRange) -> ByteRange<'a> {
        let byte_range: ByteRange<'a> = self.into();
        byte_range.subrange(range)
    }

    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    pub fn start(&self) -> Pointer {
        self.start
    }

    pub fn end(&self) -> Pointer {
        self.start + self.bytes.len()
    }

    pub fn take(self) -> Vec<u8> {
        self.bytes
    }
}

impl IntoIterator for OwnedBytes {
    type Item = <Vec<u8> as IntoIterator>::Item;
    type IntoIter = <Vec<u8> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.bytes.into_iter()
    }
}

impl<'a> From<&'a OwnedBytes> for ByteRange<'a> {
    fn from(val: &'a OwnedBytes) -> Self {
        ByteRange::new(val.start, &val.bytes)
    }
}

impl From<&OwnedBytes> for Range<Pointer> {
    fn from(val: &OwnedBytes) -> Self {
        val.start..val.start + val.bytes.len()
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
