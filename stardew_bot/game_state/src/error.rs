use thiserror::Error;

use crate::ItemId;

#[derive(Error)]
pub enum Error {
    #[error("dotnet_debugger::Error( {0} )")]
    DotnetDebugger(#[from] dotnet_debugger::Error),

    #[error("dsl::Error( {0} )")]
    DSL(#[from] dsl::Error),

    #[error("dsl::ir::Error( {0} )")]
    DslIr(#[from] dsl::ir::Error),

    #[error("Expected non-empty output from VirtualMachine")]
    ExpectedNonEmptyValue,

    #[error("Could not locate crop data for seed '{0}'")]
    UnknownSeedKind(ItemId),

    #[error("Tree kind '{0}' did not correspond to any known tree.")]
    UnrecognizedTreeKind(String),

    #[error("Quality should be a value from 0 to 3, but found {0}")]
    InvalidQualityValue(i32),

    #[error("Unrecognized item index {0} used as resource clump")]
    UnrecognizedResourceClump(i32),

    #[error("Unrecognized item index {0} used as furniture type")]
    UnrecognizedFurnitureType(i32),

    #[error(
        "Expected to have player stat '{0}' defined, \
         but it was missing."
    )]
    MissingStat(String),

    #[error("Could not find room named '{0}'")]
    UnknownRoom(String),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
