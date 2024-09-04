use memory_reader::Pointer;
use thiserror::Error;

use crate::CorElementType;

#[derive(Error)]
pub enum Error {
    #[error("dll_unpacker::Error{{ {err} }}")]
    DLLUnpacker {
        #[from]
        err: dll_unpacker::Error,
    },

    #[error("memory_reader::Error{{ {err} }}")]
    MemoryReader {
        #[from]
        err: memory_reader::Error,
    },

    #[error("Could not find pointer to .NET Module '{0}'")]
    ModulePointerNotFound(String),

    #[error("Could not find pointer to table of method tables")]
    PointerToMethodTableTableNotFound,

    #[error("Could not find pointer to DomainLocalModule")]
    PointerToDomainLocalModuleNotFound,

    #[error("Could not find instance of object")]
    NoObjectInstanceFound,

    #[error("Could not locate the DLL, starting from the Module")]
    DLLPointerNotFoundFromModule,

    #[error("Memmap region for .NET Module {0} not found")]
    RegionForDLLNotFoundFromName(String),

    #[error("Could not read the CLR DLL at {0}")]
    RegionForDLLNotFoundFromPointer(Pointer),

    #[error("Value 0x{0:02x} does not correspond to any element type")]
    InvalidRuntimeType(u8),

    #[error(
        "Element type {0} is only used for signatures, \
         and should not appear as a field type."
    )]
    NoSuchRuntimeValue(CorElementType),

    #[error("Locating non-static field requires pointer to instance")]
    LocationOfInstanceFieldRequiresInstance,

    #[error(
        "Parsing {runtime_type} requires {expected} bytes, \
         but was only provided with {provided}."
    )]
    InsufficientBytesForValue {
        runtime_type: CorElementType,
        provided: usize,
        expected: usize,
    },

    #[error(
        "Parsing ValueType requires the containing MethodTable \
         in order to determine the type."
    )]
    ValueTypeRequiresContextualParsing,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
