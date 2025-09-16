use thiserror::Error;

use dsl_ir::{DSLType, Pointer};

#[derive(Error)]
pub enum Error {
    #[error("memory_reader::Error( {0} )")]
    MemoryReader(#[from] memory_reader::Error),

    #[error("dotnet_debugger::Error( {0} )")]
    DotnetDebugger(#[from] dotnet_debugger::Error),

    #[error("dsl::ir::Error( {0} )")]
    DslIr(#[from] dsl_ir::Error),

    #[error(
        "Attempted to convert StackValue to a scalar, \
         but the VMResult contained `None` at that location."
    )]
    AttemptedConversionOfMissingValue,

    #[error(
        "Expected to dsl::runtime::Values \
         to contain {expected} values, \
         but instead found {actual} values."
    )]
    IncorrectNumberOfValues { expected: usize, actual: usize },

    #[error(
        "Attempted to read output as {attempted}, \
         but the output was of type {actual}."
    )]
    IncorrectOutputType { attempted: DSLType, actual: DSLType },

    #[error("Local evaluation may not perform any reads.")]
    ReadOccurredDuringLocalEvaluation,

    #[error("Pointer arithmetic of ({0} + {1})")]
    InvalidPointerAddition(Pointer, usize),

    #[error("Cannot pop from empty value stack")]
    CannotPopFromEmptyValueStack,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
