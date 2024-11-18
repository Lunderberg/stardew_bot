use thiserror::Error;
use tui_utils::inputs::KeySequence;

use crate::X11Error;

#[derive(Error)]
pub enum Error {
    #[error("std::io::Error( {0} )")]
    Io(#[from] std::io::Error),

    #[error("stardew_utils::Error( {0} )")]
    UtilError(#[from] stardew_utils::Error),

    #[error("memory_reader::Error( {0} )")]
    MemoryReadError(#[from] memory_reader::Error),

    #[error("dotnet_debugger::Error( {0} )")]
    DotnetDebugger(#[from] dotnet_debugger::Error),

    #[error("X11Error( {0} )")]
    X11Error(#[from] X11Error),

    #[error("tui_utils::Error( {0} )")]
    TuiUtilError(#[from] tui_utils::Error),

    #[error("No binding for '{0}'")]
    UnknownKeySequence(KeySequence),

    #[error("Expected non-empty value from PhysicalAccessChain")]
    ExpectedNoneEmptyValue,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
