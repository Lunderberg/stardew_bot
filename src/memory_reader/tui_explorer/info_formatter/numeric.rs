use std::{fmt::Display, marker::PhantomData};

use crate::{
    memory_reader::{MemoryRegion, Pointer},
    MemoryReader,
};

use super::FormatFromPointer;

pub struct FormatHexValue<T>(PhantomData<T>);
pub struct FormatDecValue<T>(PhantomData<T>);

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
