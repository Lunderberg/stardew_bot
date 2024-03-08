use std::fmt::{Debug, Display};
use std::io::Read;

use process_vm_io::ProcessVirtualMemoryIO;

use crate::extensions::IterConversion;
use crate::numeric_traits::{CheckedAdd, CheckedSub};
use crate::{Error, MemoryValue, Result};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer {
    pub(crate) address: usize,
}

impl Pointer {
    pub fn new(address: impl Into<Self>) -> Self {
        address.into()
    }

    pub fn null() -> Self {
        Self { address: 0 }
    }

    pub fn is_null(&self) -> bool {
        self.address == 0
    }

    pub fn is_aligned(&self, alignment: usize) -> bool {
        self.address % alignment == 0
    }

    pub fn as_aligned(self, alignment: usize) -> Option<Self> {
        self.is_aligned(alignment).then_some(self)
    }

    pub fn read_bytes(&self, pid: u32, num_bytes: usize) -> Result<Vec<u8>> {
        let mut process_io =
            unsafe { ProcessVirtualMemoryIO::new(pid, self.address as u64) }?;

        let mut buffer = vec![0u8; num_bytes];

        process_io.read_exact(&mut buffer).map_err(|err| {
            let err_string = format!("{}", err);
            if err_string.contains("Operation not permitted") {
                Error::MemoryReadInsufficientPermission
            } else if err_string.contains("Bad address") {
                Error::MemoryReadBadAddress
            } else {
                Error::MemoryReadOther { err }
            }
        })?;

        Ok(buffer)
    }

    pub fn checked_add<RHS>(
        self,
        rhs: RHS,
    ) -> Option<<Self as CheckedAdd<RHS>>::Output>
    where
        Self: CheckedAdd<RHS>,
    {
        <Self as CheckedAdd<RHS>>::checked_add(self, rhs)
    }

    pub fn checked_sub<RHS>(
        self,
        rhs: RHS,
    ) -> Option<<Self as CheckedSub<RHS>>::Output>
    where
        Self: CheckedSub<RHS>,
    {
        <Self as CheckedSub<RHS>>::checked_sub(self, rhs)
    }
}

impl CheckedAdd<usize> for Pointer {
    type Output = Pointer;

    fn checked_add(self, rhs: usize) -> Option<Self::Output> {
        self.address
            .checked_add(rhs)
            .map(|address| Self { address })
    }
}

impl std::ops::Add<usize> for Pointer {
    type Output = Pointer;

    fn add(self, rhs: usize) -> Self::Output {
        self.checked_add(rhs).unwrap()
    }
}

impl CheckedSub<usize> for Pointer {
    type Output = Pointer;

    fn checked_sub(self, rhs: usize) -> Option<Self::Output> {
        self.address
            .checked_sub(rhs)
            .map(|address| Self { address })
    }
}

impl std::ops::Sub<usize> for Pointer {
    type Output = Pointer;

    fn sub(self, rhs: usize) -> Self::Output {
        self.checked_sub(rhs).unwrap()
    }
}

impl CheckedSub<Pointer> for Pointer {
    type Output = usize;

    fn checked_sub(self, rhs: Pointer) -> Option<Self::Output> {
        self.address.checked_sub(rhs.address)
    }
}

impl std::ops::Sub for Pointer {
    type Output = usize;

    fn sub(self, rhs: Self) -> Self::Output {
        self.checked_sub(rhs).unwrap_or_else(|| {
            panic!(
                "Subtracting {rhs} from {self} causes overflow (diff = -{})",
                rhs - self
            )
        })
    }
}

impl Debug for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Pointer(0x{:016x})", self.address)
    }
}

impl Display for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "0x{:016x}", self.address)
    }
}

impl From<usize> for Pointer {
    fn from(address: usize) -> Self {
        Self { address }
    }
}

impl From<[u8; 8]> for Pointer {
    fn from(bytes: [u8; 8]) -> Self {
        let address = usize::from_ne_bytes(bytes);
        Self { address }
    }
}

impl IterConversion<usize> for Pointer {
    fn convert_next<Iter: Iterator<Item = usize>>(
        iter: &mut Iter,
    ) -> Option<Self> {
        iter.next().map(Into::into)
    }

    fn convert_next_back<Iter: DoubleEndedIterator<Item = usize>>(
        iter: &mut Iter,
    ) -> Option<Self> {
        iter.next_back().map(Into::into)
    }
}

impl IterConversion<MemoryValue<usize>> for MemoryValue<Pointer> {
    fn convert_next<Iter: Iterator<Item = MemoryValue<usize>>>(
        iter: &mut Iter,
    ) -> Option<Self> {
        iter.next().map(|value| value.map(Into::into))
    }

    fn convert_next_back<
        Iter: DoubleEndedIterator<Item = MemoryValue<usize>>,
    >(
        iter: &mut Iter,
    ) -> Option<Self> {
        iter.next_back().map(|value| value.map(Into::into))
    }
}

impl IterConversion<MemoryValue<u8>> for MemoryValue<Pointer> {
    fn convert_next<Iter: Iterator<Item = MemoryValue<u8>>>(
        iter: &mut Iter,
    ) -> Option<Self> {
        <MemoryValue<[u8; 8]> as IterConversion<MemoryValue<u8>>>::convert_next(
            iter,
        )
        .map(|value| value.map(Into::into))
    }

    fn convert_next_back<Iter: DoubleEndedIterator<Item = MemoryValue<u8>>>(
        iter: &mut Iter,
    ) -> Option<Self> {
        <MemoryValue<[u8; 8]> as IterConversion<MemoryValue<u8>>>::convert_next_back(
            iter,
        )
        .map(|value| value.map(Into::into))
    }
}
