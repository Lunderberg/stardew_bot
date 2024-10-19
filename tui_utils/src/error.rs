use thiserror::Error;

#[derive(Error)]
pub enum Error {
    #[error("std::io::Error( {0} )")]
    Io(#[from] std::io::Error),

    #[error("dotnet_debugger::Error( {0} )")]
    DotnetDebugger(#[from] dotnet_debugger::Error),

    #[error("Invalid emacs-style key sequence: {0}")]
    InvalidKeyBinding(String),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
