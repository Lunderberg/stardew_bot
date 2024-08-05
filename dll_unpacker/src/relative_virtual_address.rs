use crate::{ByteRange, Error, UnpackBytes};

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

impl RelativeVirtualAddress {
    pub fn new(value: usize) -> Self {
        Self(value)
    }
}

impl<'a> UnpackBytes<'a> for Option<RelativeVirtualAddress> {
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
        let addr: u32 = bytes.unpack()?;
        if addr == 0 {
            Ok(None)
        } else {
            Ok(Some(RelativeVirtualAddress(addr as usize)))
        }
    }
}

impl<'a> UnpackBytes<'a> for RelativeVirtualAddress {
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
        bytes.unpack().and_then(|opt_rva: Option<Self>| {
            assert!(opt_rva.is_some());
            opt_rva.ok_or(Error::UnexpectedNullVirtualAddress)
        })
    }
}

impl<'a> UnpackBytes<'a> for Option<VirtualRange> {
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
        assert!(bytes.len() == 8);

        let opt_rva: Option<RelativeVirtualAddress> =
            bytes.subrange(0..4).unpack()?;
        let size: usize = bytes.subrange(4..8).unpack::<u32>()? as usize;
        Ok(opt_rva.map(|rva| VirtualRange { rva, size }))
    }
}

impl<'a> UnpackBytes<'a> for VirtualRange {
    fn unpack(bytes: ByteRange<'a>) -> Result<Self, Error> {
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
