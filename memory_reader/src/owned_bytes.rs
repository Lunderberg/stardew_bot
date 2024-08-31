use std::ops::Range;

use crate::{ByteRange, NormalizeRange, Pointer};

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
