use thiserror::Error;

use crate::TypeInferenceError;

#[derive(Error)]
pub enum Error {
    #[error("dotnet_debugger::Error( {0} )")]
    DotnetDebugger(#[from] dotnet_debugger::Error),

    #[error("dsl::analysis::TypeInferenceError( {0} )")]
    TypeInferenceError(#[from] TypeInferenceError),

    #[error("dsl::ir::error( {0} )")]
    DslIr(#[from] dsl_ir::Error),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
