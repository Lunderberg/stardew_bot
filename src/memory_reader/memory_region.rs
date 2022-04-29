use super::{MemoryMapRegion, MemoryValue, Pointer};

use std::ops::Deref;

#[derive(Clone)]
pub struct MemoryRegion {
    start: Pointer,
    bytes: Vec<u8>,
    pub source: MemoryMapRegion,
}

impl MemoryRegion {
    pub fn new(
        start: Pointer,
        bytes: Vec<u8>,
        source: MemoryMapRegion,
    ) -> Self {
        Self {
            start,
            bytes,
            source,
        }
    }

    pub fn start(&self) -> Pointer {
        self.start
    }

    pub fn end(&self) -> Pointer {
        self.start + self.bytes.len()
    }

    pub fn bytes_at_offset<const N: usize>(
        &self,
        byte_offset: usize,
    ) -> MemoryValue<[u8; N]> {
        let mut out = [0; N];
        (0..N).zip(out.iter_mut()).for_each(|(i, out_byte)| {
            *out_byte = self[byte_offset + i];
        });
        MemoryValue::new(self.start + byte_offset, out)
    }

    pub fn size_bytes(&self) -> usize {
        self.bytes.len()
    }

    pub fn iter_bytes(&self) -> impl Iterator<Item = MemoryValue<u8>> + '_ {
        self.bytes
            .iter()
            .enumerate()
            .map(|(i, &val)| MemoryValue::new(self.start + i, val))
    }

    pub fn into_iter_bytes(self) -> impl Iterator<Item = MemoryValue<u8>> {
        let start = self.start;
        self.bytes
            .into_iter()
            .enumerate()
            .map(move |(i, val)| MemoryValue::new(start + i, val))
    }
}

impl Deref for MemoryRegion {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.bytes
    }
}
