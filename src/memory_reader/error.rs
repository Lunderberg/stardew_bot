use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error)]
pub enum Error {
    #[error("/proc/{0}/maps not found")]
    MemoryMapNotFound(u32),
    #[error("Stack memory map not found")]
    StackNotFound,
    #[error("Could not find memory map {0}")]
    MissingMemoryMapSection(String),
    #[error(
        "No permissions to read memory.  \
         Consider temporarily disabling ptrace_scope protections \
         with 'echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope'"
    )]
    MemoryReadPermissionError,
    #[error("Bad address in remote process")]
    MemoryReadBadAddress,
    #[error("Error {err} reading process memory.")]
    MemoryReadOtherError {
        #[source]
        err: std::io::Error,
    },
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
