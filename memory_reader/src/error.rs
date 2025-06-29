use thiserror::Error;

use crate::Pointer;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error)]
pub enum Error {
    #[error("/proc/{0}/maps not found")]
    MemoryMapNotFound(u32),

    #[error("Stack memory map not found")]
    StackNotFound,

    #[error("Path not convertible to UTF-8")]
    InvalidUTF8InPath,

    #[error("InvalidUTF8")]
    InvalidUTF8(#[from] std::str::Utf8Error),

    #[error("Could not find memory map {0}")]
    MissingMemoryMapSection(String),

    #[error(
        "No permissions to read memory.  \
         Consider temporarily disabling ptrace_scope protections \
         with 'echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope'"
    )]
    MemoryReadInsufficientPermission,

    #[error("Attempted memory read at nullptr of remote process")]
    MemoryReadNullPointer,

    #[error(
        "Bad read of {1} bytes \
         from address {0} in remote process"
    )]
    MemoryReadBadAddress(Pointer, usize),

    #[error("Region {name:?} from address {start} to {end} could not be read")]
    MemoryReadBadRegion {
        name: Option<String>,
        start: Pointer,
        end: Pointer,
    },

    #[error(
        "Expected a memory reader initialized for PID {expected}, \
         but found a memory reader initialied for PID {actual}."
    )]
    IncorrectPIDForMemoryReader { expected: u32, actual: u32 },

    #[error("Error {err} reading process memory.")]
    MemoryReadOther {
        #[source]
        err: std::io::Error,
    },

    #[error("nix::errno::Errno({err})")]
    NixError {
        #[from]
        err: nix::errno::Errno,
    },

    #[error(transparent)]
    Io {
        #[from]
        err: std::io::Error,
    },

    #[error(transparent)]
    InvalidElfFormat {
        #[from]
        err: elf::ParseError,
    },

    #[error("Pointer overflow, {0} + {1}")]
    PointerOverflow(Pointer, usize),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
