use memory_reader::Pointer;
use thiserror::Error;

#[derive(Error)]
pub enum Error {
    #[error("Path not convertible to UTF-8")]
    InvalidUTF8InPath,
    #[error("std::io::Error{{ {err} }}")]
    Io {
        #[from]
        err: std::io::Error,
    },
    #[error("memory_reader::Error{{ {err} }}")]
    MemoryReader {
        #[from]
        err: memory_reader::Error,
    },
    #[error("dll_unpacker::Error{{ {err} }}")]
    DLLUnpacker {
        #[from]
        err: dll_unpacker::Error,
    },

    #[error("stardew_utils::Error{{ {err} }}")]
    StardewUtilError {
        #[from]
        err: stardew_utils::Error,
    },

    #[error("Invalid emacs-style key sequence: {0}")]
    InvalidKeyBinding(String),

    #[error("Could not find MemoryRegion containing pointer {0}")]
    PointerNotFound(Pointer),

    #[error("Could not find pointer to .NET Module")]
    ModulePointerNodeFound,

    #[error("Could not find pointer to table of method tables")]
    PointerToMethodTableTableNotFound,

    #[error("Could not find table of method tables")]
    MethodTableTableNotFound,

    #[error("Module class should only have pointers to canonical MethodTable")]
    MethodTableTableReferencedNonCanonicalMethodTable,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
