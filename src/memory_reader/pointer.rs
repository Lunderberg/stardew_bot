use std::fmt::{Debug, Display};
use std::io::Read;

use process_vm_io::ProcessVirtualMemoryIO;

use crate::memory_reader::{Error, Result};

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Pointer {
    address: usize,
}

impl Pointer {
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
            unsafe { ProcessVirtualMemoryIO::new(pid, self.address as u64) }
                .unwrap();

        let mut buffer = vec![0u8; num_bytes];

        process_io.read_exact(&mut buffer).map_err(|err| {
            let err_string = format!("{}", err);
            if err_string.contains("Operation not permitted") {
                Error::MemoryReadPermissionError
            } else if err_string.contains("Bad address") {
                Error::MemoryReadBadAddress
            } else {
                Error::MemoryReadOtherError { err }
            }
        })?;

        Ok(buffer)
    }
}

impl std::ops::Add<usize> for Pointer {
    type Output = Pointer;

    fn add(self, rhs: usize) -> Self::Output {
        let address = self.address + rhs;
        Self { address }
    }
}

impl std::ops::Sub<usize> for Pointer {
    type Output = Pointer;

    fn sub(self, rhs: usize) -> Self::Output {
        let address = self.address - rhs;
        Self { address }
    }
}

impl std::ops::Sub for Pointer {
    type Output = usize;

    fn sub(self, rhs: Self) -> Self::Output {
        self.address - rhs.address
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
