use super::memory_reader::Error as MemoryReadError;

use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Error)]
pub enum Error {
    #[error("Stardew Valley process not running")]
    StardewNotRunning,
    #[error("")]
    MemoryReadError {
        #[from]
        err: MemoryReadError,
    },
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
