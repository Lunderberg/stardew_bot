use dsl_ir::ExprKind;
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

    #[error("dsl::validation::Error( {0} )")]
    DslValidation(#[from] dsl_validation::Error),

    #[error("dsl::passes::Error( {0} )")]
    DslPasses(#[from] dsl_passes::Error),

    #[error("dsl::vm::Error( {0} )")]
    DslVm(#[from] dsl_vm::Error),

    #[error("Attempted use of an already consumed value.")]
    AttemptedUseOfConsumedValue,

    #[error(
        "Symbolic expression must be lowered \
         prior to generating VM instruction, \
         but encountered {0}."
    )]
    SymbolicExpressionRequiresLowering(ExprKind),

    #[error(
        "Short-circuit boolean operators must be lowered \
         prior to generating VM instruction, \
         but encountered {0}."
    )]
    BooleanOperatorRequiresLowering(ExprKind),

    #[error(
        "ReadPrim operators must be lowered \
         to ReadBytes and CastBytes operators\
         prior to generating VM instruction, \
         but encountered {0}."
    )]
    ReadPrimOperatorRequiresLowering(ExprKind),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
