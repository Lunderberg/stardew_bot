use std::ops::Range;

use memory_reader::Pointer;

use crate::{ByteRange, Error};

// TODO: Move all the type tags into a common namespace, so that I
// don't need to prefix the actual objects.  While objects with a
// static lifetime can serve as their own type tag, unpacked objects
// with a non-static lifetime cannot easily do so.
#[derive(Clone, Copy)]
pub struct UnpackedBlob<'a> {
    bytes: ByteRange<'a>,
    header_size: usize,
}

impl<'a> UnpackedBlob<'a> {
    /// Construct a new UnpackedBlob.  The `heap` parameter may have
    /// additional trailing data after the blob, which will be
    /// truncated during initialization.
    pub fn new(bytes: ByteRange<'a>) -> Result<Self, Error> {
        // Determine the length of the blob based on ECMA-335,
        // section II.24.2.4.

        let byte = bytes[0];

        let leading_ones = byte.leading_ones();
        let (header_size, size) = match leading_ones {
            0 => {
                let size: usize = (byte & 0x7f).into();
                (1, size)
            }
            1 => {
                let high: usize = (byte & 0x3f).into();
                let low: usize = bytes[1].into();
                (2, (high << 8) + low)
            }
            2 => {
                let high: usize = (byte & 0x1f).into();
                let mid1: usize = bytes[1].into();
                let mid2: usize = bytes[2].into();
                let low: usize = bytes[3].into();
                (4, (high << 24) + (mid1 << 16) + (mid2 << 8) + low)
            }
            _ => {
                return Err(Error::InvalidBlobHeader { leading_ones });
            }
        };

        let bytes = bytes.subrange(..header_size + size);

        Ok(Self { bytes, header_size })
    }

    pub fn ptr_range(&self) -> Range<Pointer> {
        self.bytes.into()
    }

    pub fn content(&self) -> ByteRange<'a> {
        self.bytes.subrange(self.header_size..)
    }
}

impl<'a> Into<Range<Pointer>> for UnpackedBlob<'a> {
    fn into(self) -> Range<Pointer> {
        self.bytes.into()
    }
}
