use std::marker::PhantomData;

use crate::{MemoryReader, MemoryRegion, Pointer};

use super::InfoFormatter;

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

        impl InfoFormatter for FormatDecValue<$prim>
        {
            fn name(&self) -> &'static str {
                "Dec Value"
            }

            fn format(
                &self,
                _reader: &MemoryReader,
                region: &MemoryRegion,
                location: Pointer,
            ) -> Option<String> {
                const ALIGNMENT: usize = std::mem::align_of::<$prim>();
                let location = location.as_aligned(ALIGNMENT)?;
                const NBYTES: usize = std::mem::size_of::<$prim>();

                let data = region.bytes_at_pointer::<NBYTES>(location)?;
                let data = <$prim>::from_ne_bytes(data.value);
                Some(format!("{data}"))
            }
        }

        impl InfoFormatter for FormatHexValue<$prim>
        {
            fn name(&self) -> &'static str {
                "Hex Value"
            }

            fn format(
                &self,
                _reader: &MemoryReader,
                region: &MemoryRegion,
                location: Pointer,
            ) -> Option<String> {
                const ALIGNMENT: usize = std::mem::align_of::<$prim>();
                let location = location.as_aligned(ALIGNMENT)?;

                const NBYTES: usize = std::mem::size_of::<$prim>();

                let data = region.bytes_at_pointer::<NBYTES>(location)?;
                let data = <$prim>::from_ne_bytes(data.value);
                Some(format!("0x{data:02x}"))
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
