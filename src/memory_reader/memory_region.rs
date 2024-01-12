use super::{CollectBytes as _, MemoryMapRegion, MemoryValue, Pointer};

use std::ops::{Index, Range, RangeInclusive};

#[derive(Clone)]
pub struct MemoryRegion {
    start: Pointer,
    bytes: Vec<u8>,
    source: MemoryMapRegion,
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

    pub const POINTER_SIZE: usize = std::mem::size_of::<usize>();

    pub fn start(&self) -> Pointer {
        self.start
    }

    pub fn end(&self) -> Pointer {
        self.start + self.bytes.len()
    }

    pub fn contains(&self, location: Pointer) -> bool {
        self.source.contains(location)
    }

    pub fn at_offset(&self, byte_offset: usize) -> Pointer {
        self.start + byte_offset
    }

    pub fn rfind_pattern(&self, pat: &[u8]) -> Option<Pointer> {
        self.bytes
            .windows(pat.len())
            .enumerate()
            .rev()
            .find(|(_, window)| {
                window.iter().zip(pat.iter()).all(|(a, b)| a == b)
            })
            .map(|(offset, _)| self.start + offset)
    }

    pub fn name(&self) -> String {
        self.source.short_name()
    }

    pub fn bytes_at_offset<const N: usize>(
        &self,
        byte_offset: usize,
    ) -> Option<MemoryValue<[u8; N]>> {
        (byte_offset + N <= self.bytes.len()).then(|| {
            let bytes = std::array::from_fn(|i| self[byte_offset + i]);
            MemoryValue::new(self.start + byte_offset, bytes)
        })
    }
    pub fn bytes_at_pointer<const N: usize>(
        &self,
        location: Pointer,
    ) -> Option<MemoryValue<[u8; N]>> {
        if location >= self.start {
            self.bytes_at_offset(location - self.start)
        } else {
            None
        }
    }

    pub fn size_bytes(&self) -> usize {
        self.bytes.len()
    }

    pub fn iter_from_pointer(
        &self,
        location: Pointer,
    ) -> impl Iterator<Item = MemoryValue<u8>> + '_ {
        self.bytes[location - self.start..]
            .iter()
            .enumerate()
            .map(move |(i, &val)| MemoryValue::new(location + i, val))
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

    pub(crate) fn data(&self) -> &[u8] {
        &self.bytes
    }

    pub fn stack_pointers(
        &self,
        libc_address_ranges: Vec<Range<Pointer>>,
    ) -> impl Iterator<Item = MemoryValue<Pointer>> + '_ {
        self.bytes
            .iter()
            .enumerate()
            .map(|(i, &val)| MemoryValue::new(self.start + i, val))
            .iter_byte_arr()
            .rev()
            .map(|bytes: MemoryValue<[u8; 8]>| -> MemoryValue<Pointer> {
                bytes.map(|b| b.into())
            })
            // The bottom stack frame has a return pointer into libc.so
            .skip_while(move |pointer| {
                libc_address_ranges.iter().all(|libc_address_range| {
                    !libc_address_range.contains(&pointer.value)
                })
            })
            .scan(Pointer::null(), |state: &mut Pointer, pointer| {
                // So the bottom stack frame pointer is the first NULL
                // after a pointer to libc.so, and every stack frame
                // after that can be found by searching for a pointer
                // to the previous.
                if *state == pointer.value {
                    *state = pointer.location;
                    Some(Some(pointer))
                } else {
                    Some(None)
                }
            })
            .filter_map(|x| x)
    }
}

impl Index<usize> for MemoryRegion {
    type Output = u8;

    fn index(&self, index: usize) -> &Self::Output {
        &self.bytes[index]
    }
}

impl Index<Range<usize>> for MemoryRegion {
    type Output = [u8];

    fn index(&self, index: Range<usize>) -> &Self::Output {
        &self.bytes[index]
    }
}

impl Index<RangeInclusive<usize>> for MemoryRegion {
    type Output = [u8];

    fn index(&self, index: RangeInclusive<usize>) -> &Self::Output {
        &self.bytes[index]
    }
}

impl Index<Pointer> for MemoryRegion {
    type Output = u8;

    fn index(&self, location: Pointer) -> &Self::Output {
        &self[location - self.start]
    }
}

impl Index<Range<Pointer>> for MemoryRegion {
    type Output = [u8];

    fn index(&self, index: Range<Pointer>) -> &Self::Output {
        let start = index.start - self.start;
        let end = index.end - self.start;
        &self[start..end]
    }
}

impl Index<RangeInclusive<Pointer>> for MemoryRegion {
    type Output = [u8];

    fn index(&self, index: RangeInclusive<Pointer>) -> &Self::Output {
        let start = *index.start() - self.start;
        let end = *index.end() - self.start;
        &self[start..=end]
    }
}
