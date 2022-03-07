use super::{MemoryValue, Pointer};

use std::ops::Deref;

pub struct MemoryRegion {
    start: Pointer,
    bytes: Vec<u8>,
}

impl MemoryRegion {
    pub fn new(start: Pointer, bytes: Vec<u8>) -> Self {
        Self { start, bytes }
    }

    pub fn iter_bytes(&self) -> impl Iterator<Item = MemoryValue<&u8>> + '_ {
        self.bytes
            .iter()
            .enumerate()
            .map(|(i, val)| MemoryValue::new(self.start + (i as u64), val))
    }

    pub fn into_iter_bytes(self) -> impl Iterator<Item = MemoryValue<u8>> {
        let start = self.start;
        self.bytes
            .into_iter()
            .enumerate()
            .map(move |(i, val)| MemoryValue::new(start + (i as u64), val))
    }
}

impl Deref for MemoryRegion {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        &self.bytes
    }
}
