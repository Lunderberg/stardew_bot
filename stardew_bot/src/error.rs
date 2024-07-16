use thiserror::Error;

#[derive(Error)]
pub enum Error {
    #[error("{err}")]
    UtilError {
        #[from]
        err: stardew_utils::Error,
    },

    #[error("{err}")]
    MemoryReadError {
        #[from]
        err: memory_reader::Error,
    },

    #[error("{err}")]
    TuiError {
        #[from]
        err: tui_explorer::Error,
    },
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
