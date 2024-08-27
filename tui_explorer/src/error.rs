use memory_reader::Pointer;
use thiserror::Error;

use crate::KeySequence;

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

    #[error("dotnet_debugger::Error{{ {err} }}")]
    DotnetDebugger {
        #[from]
        err: dotnet_debugger::Error,
    },

    #[error("stardew_utils::Error{{ {err} }}")]
    StardewUtilError {
        #[from]
        err: stardew_utils::Error,
    },

    #[error("Invalid emacs-style key sequence: {0}")]
    InvalidKeyBinding(String),

    #[error("No binding for '{0}'")]
    UnknownKeySequence(KeySequence),

    #[error("Could not find MemoryRegion containing pointer {0}")]
    PointerNotFound(Pointer),

    #[error("Could not find table of method tables")]
    MethodTableTableNotFound,

    #[error("Module class should only have pointers to canonical MethodTable")]
    MethodTableTableReferencedNonCanonicalMethodTable,

    #[error("The named annotation was not found")]
    AnnotationNotFound,

    #[error("Could not find method table {0}")]
    MethodTableNotFound(&'static str),

    #[error(
        "Expected to find a unique instance of Game object, \
             but found {0} instances."
    )]
    UniqueGameObjectInstanceNotFound(usize),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
