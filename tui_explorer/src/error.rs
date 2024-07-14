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

    #[error("Invalid emacs-style key sequence: {0}")]
    InvalidKeyBinding(String),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
