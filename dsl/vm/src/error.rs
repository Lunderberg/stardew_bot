use thiserror::Error;

use dsl_ir::{DSLType, Pointer, RuntimePrimType, RuntimePrimValue};

use crate::InstructionIndex;

#[derive(Error)]
pub enum Error {
    #[error("memory_reader::Error( {0} )")]
    MemoryReader(#[from] memory_reader::Error),

    #[error("dotnet_debugger::Error( {0} )")]
    DotnetDebugger(#[from] dotnet_debugger::Error),

    #[error("dsl::ir::Error( {0} )")]
    DslIr(#[from] dsl_ir::Error),

    #[error("Virtual machine did not contain a function named '{0}'")]
    NoSuchFunction(String),

    #[error("Cannot define function {0} multiple times")]
    DuplicateFunctionName(String),

    #[error(
        "Offset must be applied to Ptr, \
         but was instead applied to {0}."
    )]
    OffsetAppliedToNonPointer(RuntimePrimType),

    #[error(
        "DynamicOffset requires the index \
         to have been previously computed, \
         but location {0} was empty."
    )]
    DynamicOffsetWasEmpty(usize),

    #[error(
        "DynamicScale requires the index \
         to have been previously computed, \
         but location {0} was empty."
    )]
    DynamicScaleWasEmpty(usize),

    #[error(
        "DynamicOffset requires the index \
         to be convertible to a usize, \
         but index instead contained {0}."
    )]
    DynamicOffsetNotConvertibleToIndex(RuntimePrimValue),

    #[error(
        "IsSubclassOf requires the argument to be a Pointer, \
         but instead received {0}."
    )]
    InvalidArgumentForSubclassCheck(RuntimePrimValue),

    #[error(
        "Downcast requires the register to contain a pointer, \
         but instead register contained {0}."
    )]
    DowncastAppliedToNonPointer(RuntimePrimValue),

    #[error(
        "Downcast requires the register to contain a pointer, \
         but instead register contained {0}."
    )]
    ReadAppliedToNonPointer(RuntimePrimValue),

    #[error("Local evaluation of VM may not perform any reads.")]
    ReadOccurredDuringLocalVMEvaluation,

    #[error(
        "ConditionalJump requires the register \
         to contain a boolean value.  \
         However, it contained an instance of type {0}."
    )]
    InvalidOperandForConditionalJump(RuntimePrimType),

    #[error(
        "Native function expecting {expected} arguments \
         was provided with {provided} arguments."
    )]
    InvalidNumberOfOperandsForNativeFunction {
        expected: usize,
        provided: usize,
    },

    #[error(
        "Operator {operator} in instruction {index} \
         does not support operands \
         of rust-native type {arg_type:?}"
    )]
    OperatorExpectsPrimitiveArgument {
        operator: &'static str,
        index: InstructionIndex,
        arg_type: DSLType,
    },

    #[error(
        "Operator {operator} in instruction {index} \
         expected a byte array, \
         but received argument of type {arg_type:?}"
    )]
    OperatorExpectsByteArray {
        operator: &'static str,
        index: InstructionIndex,
        arg_type: DSLType,
    },

    #[error(
        "Operator {operator} in instruction {index} \
         attention to access byte {byte_index} \
         in byte array of size {array_size}."
    )]
    OutOfBoundsByteIndex {
        operator: &'static str,
        index: InstructionIndex,
        byte_index: usize,
        array_size: usize,
    },

    #[error(
        "Operator {operator} in instruction {index} \
         attempted to use {value} as an integer number of bytes."
    )]
    ByteCountNotConvertibleToInt {
        operator: &'static str,
        index: InstructionIndex,
        value: RuntimePrimValue,
    },

    #[error(
        "Rust-native function expected argument of type {expected}, \
         but received argument of type {actual}."
    )]
    InvalidArgumentForNativeFunction { expected: DSLType, actual: DSLType },

    #[error("Cannot initialize vector with element type {0}")]
    IllegalVectorElementType(DSLType),

    #[error(
        "Vector was expected to be type {expected}, \
         but was instead {actual}."
    )]
    IncorrectVectorType { expected: DSLType, actual: DSLType },

    #[error(
        "Item of type {item_type} cannot be pushed \
         into Vec<{element_type}>."
    )]
    IncorrectVectorElementType {
        element_type: DSLType,
        item_type: DSLType,
    },

    #[error(
        "Expected vector into which to accumulate, \
         but received None."
    )]
    ExpectedVectorToAccumulateInto,

    #[error(
        "When pushing into vector, \
         pushed element must not be None.  \
         However, attempted to push None \
         into vector '{name}'."
    )]
    MissingElementTypeInVectorAccumulation { name: String },

    #[error(
        "Attempted to read output as {attempted}, \
         but the output was of type {actual}."
    )]
    IncorrectOutputType { attempted: DSLType, actual: DSLType },

    #[error(
        "Reached the last instruction \
         without encountering a Return."
    )]
    ReachedEndWithoutReturnInstruction,

    #[error("Pointer arithmetic of ({0} + {1})")]
    InvalidPointerAddition(Pointer, usize),

    #[error(
        "Cannot apply operator '{op}' with operand types '{lhs}' and '{rhs}'"
    )]
    InvalidOperandsForBinaryOp {
        op: &'static str,
        lhs: DSLType,
        rhs: DSLType,
    },

    #[error(
        "Expected to VMResult to contain {expected} values, \
         but instead found {actual} values."
    )]
    IncorrectNumberOfResults { expected: usize, actual: usize },

    #[error(
        "Attempted to convert StackValue to a scalar, \
         but the VMResult contained `None` at that location."
    )]
    AttemptedConversionOfMissingValue,

    #[error("Cannot apply operator '{op}' with operand type '{arg}'")]
    InvalidOperandForUnaryOp { op: &'static str, arg: DSLType },
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
