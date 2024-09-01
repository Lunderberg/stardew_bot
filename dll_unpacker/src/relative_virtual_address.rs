use std::ops::Range;

use crate::Error;

use memory_reader::{ByteRange, Pointer, UnpackBytes, UnpackOptBytes};

/// A virtual address, relative to the start of the file.  Can be
/// converted to an actual address using
/// `Unpacker.virtual_address_to_raw`.
#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct RelativeVirtualAddress(usize);

#[derive(Copy, Clone)]
pub struct VirtualRange {
    pub(crate) rva: RelativeVirtualAddress,
    pub(crate) size: usize,
}

#[derive(Copy, Clone)]
pub struct VirtualAddressRelocation {
    range: VirtualRange,
    location: Pointer,
}

impl RelativeVirtualAddress {
    pub fn new(value: usize) -> Self {
        Self(value)
    }
}

impl VirtualRange {
    pub fn new(rva: RelativeVirtualAddress, size: usize) -> Self {
        Self { rva, size }
    }

    fn start(&self) -> RelativeVirtualAddress {
        self.rva
    }

    fn end(&self) -> RelativeVirtualAddress {
        self.rva + self.size
    }

    fn contains(&self, rva: RelativeVirtualAddress) -> bool {
        self.start() <= rva && rva < self.end()
    }
}

impl VirtualAddressRelocation {
    pub fn new(range: VirtualRange, location: Pointer) -> Self {
        Self { range, location }
    }

    pub fn apply(&self, rva: RelativeVirtualAddress) -> Option<Pointer> {
        self.range.contains(rva).then(|| {
            let offset = rva - self.range.start();
            self.location + offset
        })
    }

    pub fn apply_range(&self, range: VirtualRange) -> Option<Range<Pointer>> {
        self.range.contains(range.start()).then(|| {
            let location_start =
                self.location + (range.start() - self.range.start());
            let location_end =
                self.location + (range.end() - self.range.start());

            location_start..location_end
        })
    }
}

impl<'a> UnpackOptBytes<'a> for RelativeVirtualAddress {
    type Error = Error;
    fn unpack_opt(bytes: ByteRange<'a>) -> Result<Option<Self>, Self::Error> {
        let addr: u32 = bytes.unpack()?;
        if addr == 0 {
            Ok(None)
        } else {
            Ok(Some(RelativeVirtualAddress(addr as usize)))
        }
    }
}

impl<'a> UnpackBytes<'a> for RelativeVirtualAddress {
    type Error = Error;
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error> {
        bytes.unpack().and_then(|opt_rva: Option<Self>| {
            assert!(opt_rva.is_some());
            opt_rva.ok_or(Error::UnexpectedNullVirtualAddress)
        })
    }
}

impl<'a> UnpackOptBytes<'a> for VirtualRange {
    type Error = Error;
    fn unpack_opt(bytes: ByteRange<'a>) -> Result<Option<Self>, Self::Error> {
        assert!(bytes.len() == 8);

        let opt_rva: Option<RelativeVirtualAddress> =
            bytes.subrange(0..4).unpack()?;
        let size: usize = bytes.subrange(4..8).unpack::<u32>()? as usize;
        Ok(opt_rva.map(|rva| VirtualRange { rva, size }))
    }
}

impl<'a> UnpackBytes<'a> for VirtualRange {
    type Error = Error;
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Self::Error> {
        assert!(bytes.len() == 8);
        let rva = bytes.subrange(0..4).unpack()?;
        let size = bytes.subrange(4..8).unpack()?;
        Ok(VirtualRange { rva, size })
    }
}

impl std::fmt::Display for VirtualRange {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RVA {}\n{} bytes", self.rva, self.size)
    }
}

impl std::fmt::Display for RelativeVirtualAddress {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:x}", self.0)
    }
}

impl std::ops::Add<usize> for RelativeVirtualAddress {
    type Output = RelativeVirtualAddress;

    fn add(self, rhs: usize) -> Self::Output {
        RelativeVirtualAddress(self.0 + rhs)
    }
}

impl std::ops::Sub<RelativeVirtualAddress> for RelativeVirtualAddress {
    type Output = usize;

    fn sub(self, rhs: RelativeVirtualAddress) -> Self::Output {
        self.0 - rhs.0
    }
}
