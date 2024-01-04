use super::{MemoryReader, MemoryRegion, Pointer};

use std::{fmt::Display, marker::PhantomData};

pub trait FormatFromPointer {
    fn format(
        &self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display>;
}

pub struct FormatSpacer;
pub struct FormatLocation;
pub struct FormatByte;
pub struct FormatHexValue<T>(PhantomData<T>);
pub struct FormatDecValue<T>(PhantomData<T>);
pub struct FormatNullTerminatedString;
pub struct FormatRegionPointedTo;

impl FormatFromPointer for FormatSpacer {
    fn format(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        _location: Pointer,
    ) -> Option<impl Display> {
        Some("")
    }
}

impl FormatFromPointer for FormatLocation {
    fn format(
        &self,
        _reader: &MemoryReader,
        _region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display> {
        Some(location)
    }
}

impl FormatFromPointer for FormatByte {
    fn format(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display> {
        Some(region.bytes_at_pointer::<1>(location).value[0])
    }
}

impl<T> FormatHexValue<T> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}
impl<T> FormatDecValue<T> {
    pub fn new() -> Self {
        Self(PhantomData)
    }
}

macro_rules! primitive_formatter {
    ($prim:ty) => {

        impl FormatFromPointer for FormatDecValue<$prim>
        {
            fn format(
                &self,
                _reader: &MemoryReader,
                region: &MemoryRegion,
                location: Pointer,
            ) -> Option<impl Display> {
                const ALIGNMENT: usize = std::mem::align_of::<$prim>();
                location.is_aligned(ALIGNMENT).then(|| {
                    const NBYTES: usize = std::mem::size_of::<$prim>();

                    let data = region.bytes_at_pointer::<NBYTES>(location);
                    let data = <$prim>::from_ne_bytes(data.value);
                    format!("{data}")
                })
            }
        }

        impl FormatFromPointer for FormatHexValue<$prim>
        {
            fn format(
                &self,
                _reader: &MemoryReader,
                region: &MemoryRegion,
                location: Pointer,
            ) -> Option<impl Display> {
                const ALIGNMENT: usize = std::mem::align_of::<$prim>();
                location.is_aligned(ALIGNMENT).then(|| {
                    const NBYTES: usize = std::mem::size_of::<$prim>();

                    let data = region.bytes_at_pointer::<NBYTES>(location);
                    let data = <$prim>::from_ne_bytes(data.value);
                    format!("0x{data:02x}")
                })
            }
        }

    };

    ($first:ty $(, $rest:ty)+ $(,)? ) => {
        primitive_formatter!{$first}
        $( primitive_formatter!{$rest} )*
    }
}

primitive_formatter! {usize, isize}
primitive_formatter! {u8, u16, u32, u64, u128}
primitive_formatter! {i8, i16, i32, i64, i128}

impl FormatFromPointer for FormatNullTerminatedString {
    fn format(
        &self,
        _reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display> {
        region
            .iter_from_pointer(location)
            .take_while(|byte| byte.value > 0)
            .last()
            .map(|byte| &region[location..=byte.location])
            .and_then(|slice| std::str::from_utf8(slice).ok())
    }
}

impl FormatFromPointer for FormatRegionPointedTo {
    fn format(
        &self,
        reader: &MemoryReader,
        region: &MemoryRegion,
        location: Pointer,
    ) -> Option<impl Display> {
        let data = region.bytes_at_pointer(location);
        let value: usize = usize::from_ne_bytes(data.value);
        let pointer: Pointer = value.into();
        reader
            .find_containing_region(pointer)
            .map(|pointed_region| pointed_region.short_name())
    }
}
