use itertools::Itertools as _;

use crate::extensions::*;
use crate::MemoryReader;

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

    pub fn as_range(&self) -> Range<Pointer> {
        let end = self.start + self.bytes.len();
        self.start..end
    }

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

    pub fn name(&self) -> &str {
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
        location
            .checked_sub(self.start)
            .and_then(|diff| self.bytes_at_offset(diff))
    }

    pub fn size_bytes(&self) -> usize {
        self.bytes.len()
    }

    pub fn iter_from_pointer(
        &self,
        location: Pointer,
    ) -> impl Iterator<Item = MemoryValue<u8>> + '_ {
        location
            .checked_sub(self.start)
            .into_iter()
            .flat_map(|diff| &self.bytes[diff..])
            .enumerate()
            .map(move |(i, &val)| MemoryValue::new(location + i, val))
    }

    pub fn iter(
        &self,
    ) -> impl DoubleEndedIterator<Item = MemoryValue<u8>> + '_ {
        self.bytes
            .iter()
            .enumerate()
            .map(|(i, &val)| MemoryValue::new(self.start + i, val))
    }

    pub fn into_iter(self) -> impl DoubleEndedIterator<Item = MemoryValue<u8>> {
        let start = self.start;
        self.bytes
            .into_iter()
            .enumerate()
            .map(move |(i, val)| MemoryValue::new(start + i, val))
    }

    pub fn into_iter_bytes(self) -> impl Iterator<Item = MemoryValue<u8>> {
        self.into_iter()
    }

    pub fn iter_rows(
        &self,
    ) -> impl Iterator<Item = MemoryValue<[u8; Self::POINTER_SIZE]>> + '_ {
        (0..(self.bytes.len() - (Self::POINTER_SIZE - 1)))
            .step_by(Self::POINTER_SIZE)
            .map(|i| {
                MemoryValue::new(
                    self.start + i,
                    self[i..i + Self::POINTER_SIZE].try_into().unwrap(),
                )
            })
    }

    pub fn into_iter_as_pointers(
        self,
    ) -> impl Iterator<Item = MemoryValue<Pointer>> {
        let start = self.start;
        self.into_iter_as_pointers_from(start)
    }

    pub fn into_iter_as_pointers_from(
        self,
        after_address: Pointer,
    ) -> impl Iterator<Item = MemoryValue<Pointer>> {
        let start = self.start;
        let offset = after_address - start;

        let num_ptr = (self.bytes.len() - offset) / Self::POINTER_SIZE;

        // println!("Region start: {start}");
        // println!("Iterate from: {after_address}");
        // println!("Num ptr: {num_ptr}");

        let bytes = self.bytes;
        (0..num_ptr).map(move |i| {
            let pointer_start = offset + i * Self::POINTER_SIZE;

            let value: Pointer = bytes
                [pointer_start..pointer_start + Self::POINTER_SIZE]
                .try_into()
                .unwrap();
            MemoryValue::new(start + pointer_start, value)
        })
    }

    pub fn iter_as_pointers(
        &self,
    ) -> impl Iterator<Item = MemoryValue<Pointer>> + '_ {
        self.iter_as_pointers_from(
            self.start + (self.bytes.len() - Self::POINTER_SIZE),
        )
    }

    pub fn iter_as_pointers_from(
        &self,
        after_address: Pointer,
    ) -> impl Iterator<Item = MemoryValue<Pointer>> + '_ {
        let byte_offset: usize =
            after_address.checked_sub(self.start).unwrap_or(0);
        self.bytes[..byte_offset]
            .iter()
            .enumerate()
            .map(|(i, &val)| MemoryValue::new(self.start + i, val))
            .iter_byte_arr()
            .rev()
            .map(|bytes: MemoryValue<[u8; 8]>| -> MemoryValue<Pointer> {
                bytes.map(|b| b.into())
            })
    }

    pub fn data(&self) -> &[u8] {
        &self.bytes
    }

    pub fn find_pointer_to(
        &self,
        address_range: Range<Pointer>,
    ) -> Option<MemoryValue<Pointer>> {
        self.iter_as_pointers()
            .find(|pointer| address_range.contains(&pointer.value))
    }

    pub fn stack_pointers<'a>(
        &'a self,
        reader: &'a MemoryReader,
    ) -> impl Iterator<Item = MemoryValue<Pointer>> + 'a {
        let starting_symbols: Vec<_> = reader
            .regions
            .iter()
            .filter(|region| {
                let name = region.short_name();
                name.starts_with("libc-") && name.ends_with(".so")
            })
            .flat_map(|region| region.iter_symbols())
            .filter(|symbol| {
                symbol.name == "__libc_start_main" || symbol.name == "clone"
            })
            .map(|symbol| symbol.location)
            .collect();

        let bottom_frame: Option<MemoryValue<Pointer>> = self
            .iter()
            .iter_as::<MemoryValue<Pointer>>()
            .rev()
            .find(|pointer| {
                starting_symbols
                    .iter()
                    .any(|range| range.contains(&pointer.value))
            })
            .and_then(|return_pointer| {
                self.bytes_at_pointer(
                    return_pointer.location - Self::POINTER_SIZE,
                )
            })
            .map(|bytes| bytes.map(Into::into));

        std::iter::successors(bottom_frame, |prev| {
            self.iter_as_pointers_from(prev.location)
                .tuple_windows()
                .find(|(return_pointer, stack_pointer)| {
                    stack_pointer.value == prev.location
                        && reader.is_in_executable_region(return_pointer.value)
                })
                .map(|(_, stack_pointer)| stack_pointer)
        })
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
