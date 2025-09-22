use thiserror::Error;

use crate::{SignatureInferenceError, TypeInferenceError};

#[derive(Error)]
pub enum Error {
    #[error("dll_unpacker::Error( {0} )")]
    DllUnpacker(#[from] dll_unpacker::Error),

    #[error("dotnet_debugger::Error( {0} )")]
    DotnetDebugger(#[from] dotnet_debugger::Error),

    #[error("dsl::analysis::TypeInferenceError( {0} )")]
    TypeInferenceError(#[from] TypeInferenceError),

    #[error("dsl::analysis::SignatureInferenceError( {0} )")]
    SignatureInferenceError(#[from] SignatureInferenceError),

    #[error("dsl::ir::error( {0} )")]
    DslIr(#[from] dsl_ir::Error),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
