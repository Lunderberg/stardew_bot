use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error)]
pub enum Error {
    #[error("/proc/{0}/maps not found")]
    MemoryMapNotFound(u32),
    #[error("Stack memory map not found")]
    StackNotFound,
    #[error("Path not convertible to UTF-8")]
    InvalidUTF8InPath,
    #[error("Could not find memory map {0}")]
    MissingMemoryMapSection(String),
    #[error(
        "No permissions to read memory.  \
         Consider temporarily disabling ptrace_scope protections \
         with 'echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope'"
    )]
    MemoryReadInsufficientPermission,
    #[error("Bad address in remote process")]
    MemoryReadBadAddress,
    #[error("Error {err} reading process memory.")]
    MemoryReadOther {
        #[source]
        err: std::io::Error,
    },
    #[error("Error {err} in TuiExplorer.")]
    TuiIo {
        #[source]
        err: std::io::Error,
    },
    #[error("Error {err} reading process memory")]
    ProcessVM {
        #[source]
        err: process_vm_io::Error,
    },
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<process_vm_io::Error> for Error {
    fn from(err: process_vm_io::Error) -> Self {
        Error::ProcessVM { err }
    }
}
