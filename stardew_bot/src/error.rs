use thiserror::Error;
use tui_utils::inputs::KeySequence;

use crate::{bot_logic::BotError, X11Error};

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

    #[error("BotError( {0} )")]
    BotError(#[from] BotError),

    #[error("No binding for '{0}'")]
    UnknownKeySequence(KeySequence),

    #[error("Expected non-empty output from VirtualMachine")]
    ExpectedNonEmptyValue,

    #[error("Unrecognized item index {0} used as resource clump")]
    UnrecognizedResourceClump(i32),

    #[error("Tree kind '{0}' did not correspond to any known tree.")]
    UnrecognizedTreeKind(String),

    #[error("Quality should be a value from 0 to 3, but found {0}")]
    InvalidQualityValue(i32),

    #[error("The bot has achieved all of its goals.")]
    NoRemainingGoals,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}

impl From<x11rb::x11_utils::X11Error> for Error {
    fn from(err: x11rb::x11_utils::X11Error) -> Self {
        let err: X11Error = err.into();
        err.into()
    }
}
