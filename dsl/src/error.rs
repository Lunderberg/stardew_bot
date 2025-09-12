use dotnet_debugger::RuntimePrimType;
use memory_reader::Pointer;
use thiserror::Error;

use crate::{TypeInferenceError, VMExecutionError};
use dsl_ir::{DSLType, ExprKind, OpIndex, SymbolicValue};

#[derive(Error)]
pub enum Error {
    #[error("memory_reader::Error{{ {err} }}")]
    MemoryReader {
        #[from]
        err: memory_reader::Error,
    },

    #[error("dll_unpacker::Error{{ {err} }}")]
    DllUnpacker {
        #[from]
        err: dll_unpacker::Error,
    },

    #[error("memory_reader::Error{{ {err} }}")]
    DotnetDebugger {
        #[from]
        err: dotnet_debugger::Error,
    },

    #[error("dsl::ir::Error{{ {err} }}")]
    IR {
        #[from]
        err: dsl_ir::Error,
    },

    #[error("dotnet_debugger::TypeInferenceError( {0} )")]
    TypeInferenceError(#[from] TypeInferenceError),

    #[error("dsl::VMExecutionError( {0} )")]
    VMExecutionError(#[from] VMExecutionError),

    #[error("MethodTable pointer for {0} was NULL.")]
    UnexpectedNullMethodTable(String),

    #[error("Could not find method table for '{0}'")]
    NoSuchMethodTableFound(String),

    #[error(
        "Could not find static field {field} within \
         method table of {class}."
    )]
    NoSuchStaticField { class: String, field: String },

    #[error(
        "Expected a static field, \
         but field '{field}' in class {class} \
         was an instance field."
    )]
    ExpectedStaticFieldButFoundInstanceField { class: String, field: String },

    #[error(
        "The SymbolicOperation::Field(name) operation \
         accesses a field of a class or struct.  \
         However, it was applied to object '{0}' of type '{1}'."
    )]
    FieldAccessRequiresClassOrStruct(String, DSLType),

    #[error("Unable to find inferred type for operation {0}")]
    InferredTypeNotFound(SymbolicValue),

    #[error("Invalid reference from expression {from} to {to}")]
    InvalidReference { from: OpIndex, to: OpIndex },

    #[error(
        "Parameters must have a unique owner, \
             but the parameter at {param} is used by \
             the functions at {first_owner} and {second_owner}."
    )]
    MultipleFunctionsOwnSameParam {
        param: OpIndex,
        first_owner: OpIndex,
        second_owner: OpIndex,
    },

    #[error(
        "The function parameter {param} was used, \
         but did not have a definition."
    )]
    UseOfUndefinedParam { param: OpIndex },

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

    #[error(
        "Invalid node name: '{0}'.  \
         Node names beginning with underscore \
         followed by a digit are reserved for internal use."
    )]
    AttemptedUseOfReservedName(String),

    #[error(
        "Type {0} found in a context \
         where only .NET types are expected."
    )]
    UnexpectedTypeFoundInDotNetContext(DSLType),

    #[error(
        "Attempted conversion of rust-native object \
         of type {0:?} into primitive."
    )]
    AttemptedConversionOfNativeObject(std::any::TypeId),

    #[error(
        "Attempted to convert StackValue to a scalar, \
         but the VMResult contained `None` at that location."
    )]
    AttemptedConversionOfMissingValue,

    #[error(
        "Expected to VMResult to contain {expected} values, \
         but instead found {actual} values."
    )]
    IncorrectNumberOfResults { expected: usize, actual: usize },

    #[error(
        "Analysis routine attempted to inspect remote process, \
         but cannot do so when running in local mode."
    )]
    AnalysisAttemptedToUseMissingRemoteProcess,

    #[error("Attempted use of an already consumed value.")]
    AttemptedUseOfConsumedValue,

    #[error(
        "Collection of iterators into nested vectors \
         is not currently supported."
    )]
    CollectionIntoNestedVectorNotSupported,

    #[error("Cannot apply operator '{op}' with operand type '{arg}'")]
    InvalidOperandForUnaryOp { op: &'static str, arg: DSLType },

    #[error(
        "Cannot apply operator '{op}' with operand types '{lhs}' and '{rhs}'"
    )]
    InvalidOperandsForBinaryOp {
        op: &'static str,
        lhs: DSLType,
        rhs: DSLType,
    },

    #[error(
        "Cannot perform equality check \
         for operands with types {lhs} and {rhs}."
    )]
    InvalidOperandsForEqualityCheck { lhs: DSLType, rhs: DSLType },

    #[error(
        "Cannot compare numeric values \
         of operands with types {lhs} and {rhs}."
    )]
    InvalidOperandsForNumericComparison { lhs: DSLType, rhs: DSLType },

    #[error("Downcast requires pointer argument, but received {0}")]
    InvalidOperandForPhysicalDowncast(DSLType),

    #[error("ReadValue requires pointer argument, but received {0}")]
    InvalidOperandForReadValue(DSLType),

    #[error(
        "Attempted to convert primitive of type {0} \
         into rust-native object."
    )]
    AttemptedConversionOfPrimitiveToNativeObject(RuntimePrimType),

    #[error(
        "Object of type {0:?} cannot be used \
         as a pointer to a .NET string."
    )]
    InvalidOperandForDotnetString(DSLType),

    #[error("Pointer arithmetic of ({0} + {1})")]
    InvalidPointerAddition(Pointer, usize),

    #[error(
        "The SymbolicOperation::IndexAccess(indices) operation \
         accesses an element of an array.  \
         However, it was applied to an object of type {0}."
    )]
    IndexAccessRequiresArray(DSLType),

    #[error(
        "Expressions of type {0} not convertible \
         to an array index"
    )]
    TypeNotConvertibleToIndex(DSLType),

    #[error(
        "The SymbolicOperation::NumArrayElements operation \
         returns the number of elements in an array.  \
         However, it was applied to an object of type {0}."
    )]
    ArrayLengthRequiresArray(DSLType),

    #[error(
        "The SymbolicOperation::ArrayExtent operation \
         returns the extent of a multi-dimensional array..  \
         However, it was applied to an object of type {0}."
    )]
    ArrayExtentRequiresMultiDimensionalArray(DSLType),

    #[error(
        "The SymbolicOperation::IndexAccess(indices) operation \
         requires one index for each rank of the array being accessed.  \
         However, {num_provided} indices were provided \
         to access an array of rank {num_expected}."
    )]
    IncorrectNumberOfIndices {
        num_provided: usize,
        num_expected: usize,
    },

    #[error(
        "The SymbolicOperation::Downcast operation \
         casts an object to a subclass, \
         and may only be applied to Class instances.  \
         However, it was applied to an object of type {0}."
    )]
    DowncastRequiresClassInstance(DSLType),

    #[error(
        "Downcast requires the static type to be known, \
         but could not find a loaded method table for base class."
    )]
    DowncastRequiresKnownBaseClass,

    #[error(
        "Cannot downcast from '{0}' to '{1}', \
         because '{1}' is not a subclass of '{0}'"
    )]
    DowncastRequiresRelatedClasses(String, String),

    #[error(
        "Field {field} was type {ty}, \
         but expected a primitive type."
    )]
    SymbolicExpressionMustProducePrimitive { field: String, ty: DSLType },

    #[error(
        "Currently, tuples are only supported \
         at the top level of an expression"
    )]
    TupleExpressionOnlySupportedAtTopLevel,

    #[error("The method table of an array should contain the element size.")]
    ArrayMissingComponentSize,

    #[error("The method table of an array should contain the element type.")]
    ArrayMissingElementType,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
