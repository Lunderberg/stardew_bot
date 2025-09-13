use thiserror::Error;

#[derive(Error)]
pub enum Error {
    #[error("dotnet_debugger::Error( {0} )")]
    DotnetDebugger(#[from] dotnet_debugger::Error),

    #[error("dsl::ir::Error( {0} )")]
    DslIr(#[from] dsl_ir::Error),

    #[error("dsl::analysis::Error( {0} )")]
    DslAnalysis(#[from] dsl_analysis::Error),

    #[error("dsl::analysis::TypeInferenceError( {0} )")]
    TypeInferenceError(#[from] dsl_analysis::TypeInferenceError),

    #[error("dsl::rewrite_utils::Error( {0} )")]
    DslRewriteUtils(#[from] dsl_rewrite_utils::Error),

    #[error(
        "Cannot downcast from '{0}' to '{1}', \
         because '{1}' is not a subclass of '{0}'"
    )]
    DowncastRequiresRelatedClasses(String, String),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
