use memory_reader::Pointer;
use thiserror::Error;

use crate::{
    runtime_type::RuntimePrimType, symbolic_expr::SymbolicValue,
    CorElementType, OpIndex, RuntimePrimValue, RuntimeType, SymbolicExpr,
};

#[derive(Error)]
pub enum Error {
    #[error("dll_unpacker::Error{{ {err} }}")]
    DLLUnpacker {
        #[from]
        err: dll_unpacker::Error,
    },

    #[error("memory_reader::Error{{ {err} }}")]
    MemoryReader {
        #[from]
        err: memory_reader::Error,
    },

    #[error("std::fmt::Error{{ {err} }}")]
    FmtError {
        #[from]
        err: std::fmt::Error,
    },

    #[error("dotnet_debugger::ParseError( {0} )")]
    ParseError(#[from] crate::ParseError),

    #[error("Could not find pointer to .NET Module '{0}'")]
    ModulePointerNotFound(String),

    #[error(
        "Could not find pointer to any .NET Module \
         during initial search based on MethodDef locations."
    )]
    NoModulePointerFoundFromMethodDef,

    #[error("Could not find pointer to table of method tables")]
    PointerToMethodTableTableNotFound,

    #[error("Could not find pointer to instantiated generics")]
    PointerToInstantiatedGenericsNotFound,

    #[error("Could not find pointer to DomainLocalModule")]
    PointerToDomainLocalModuleNotFound,

    #[error("Could not infer Module layout from any DLL")]
    LayoutNotInferableFromAnyModule,

    #[error("Could not find instance of object")]
    NoObjectInstanceFound,

    #[error("Could not locate the DLL, starting from the Module")]
    DLLPointerNotFoundFromModule,

    #[error("Memmap region for .NET Module {0} not found")]
    RegionForDLLNotFoundFromName(String),

    #[error("Could not read the CLR DLL at {0}")]
    RegionForDLLNotFoundFromPointer(Pointer),

    #[error("Value 0x{0:02x} does not correspond to any element type")]
    InvalidRuntimeType(u8),

    #[error(
        "Element type {0} is only used for signatures, \
         and should not appear as a field type."
    )]
    NoSuchRuntimeValue(CorElementType),

    #[error("Locating non-static field requires pointer to instance")]
    LocationOfInstanceFieldRequiresInstance,

    #[error(
        "Parsing {runtime_type} requires {expected} bytes, \
         but was only provided with {provided}."
    )]
    InsufficientBytesForValue {
        runtime_type: RuntimeType,
        provided: usize,
        expected: usize,
    },

    #[error(
        "Parsing ValueType requires the containing MethodTable \
         in order to determine the type."
    )]
    ValueTypeRequiresContextualParsing,

    #[error("MethodTable pointer for {0} was NULL.")]
    UnexpectedNullMethodTable(String),

    #[error(
        "Attempted to read {0} as MethodTable, \
         but it was not aligned to the size of a pointer.  \
         Due to internal pointer fields, \
         all MethodTables must be aligned."
    )]
    MisalignedMethodTable(Pointer),

    #[error(
        "Field {field_name} has type {field_type} in the DLL's metadata, \
         but was expected to be a ValueType based on usage in the runtime."
    )]
    ExpectedValueTypeAsMetadataSignature {
        field_name: String,
        field_type: String,
    },

    #[error(
        "Length of System.String should be non-negative, \
         but found length of {0} UTF-16 code units."
    )]
    NegativeStringLength(i32),

    #[error("Invalid UTF-16 string: {err}")]
    InvalidUTF16 {
        #[from]
        err: std::char::DecodeUtf16Error,
    },

    #[error("Pointer to array instead found RuntimeType {0}")]
    ArrayNotMarkedAsArray(RuntimeType),

    #[error(
        "Pointer to multi-dimensional array \
         instead found RuntimeType {0}"
    )]
    MultiDimArrayNotMarkedAsArray(RuntimeType),

    #[error("The method table of an array should contain the element size.")]
    ArrayMissingComponentSize,

    #[error("The method table of an array should contain the element type.")]
    ArrayMissingElementType,

    #[error(
        "From flags in method table, expected PrimType, \
         but EEClass contained type {0}."
    )]
    ExpectedPrimType(CorElementType),

    #[error("Method table contained invalid type flag 0x{0:x}.")]
    InvalidTypeFlag(u32),

    #[error(
        "Generic type var had index {index}, \
         but only {num_vars} were found."
    )]
    InvalidGenericTypeVar { index: usize, num_vars: usize },

    #[error("Instantiated generic should have method table")]
    GenericInstShouldNotBeTypeDescription,

    #[error(
        "Could not find method table for uninstantiated generic used by {0}"
    )]
    GenericMethodTableNotFound(String),

    #[error("Could not find method table for generic {0} with args [{1}]")]
    InstantiatedGenericMethodTableNotFound(String, String),

    #[error("The System.Object method table should always be present")]
    MethodTableOfSystemObjectShouldBeLoaded,

    #[error("All static fields should have their method tables loaded")]
    MethodTableOfStaticFieldShouldBeLoaded,

    #[error("Could not find method table for '{0}'")]
    NoSuchMethodTableFound(String),

    #[error(
        "Could not find static field {field} within \
         method table of {class}."
    )]
    NoSuchStaticField { class: String, field: String },

    #[error(
        "Could not find instance field '{field_name}' within \
         method table of {class_name}"
    )]
    NoSuchInstanceField {
        class_name: String,
        field_name: String,
    },

    #[error(
        "The SymbolicOperation::Field(name) operation \
         accesses a field of a class or struct.  \
         However, it was applied to an object of type {0}."
    )]
    FieldAccessRequiresClassOrStruct(RuntimeType),

    #[error(
        "The SymbolicOperation::IndexAccess(indices) operation \
         accesses an element of an array.  \
         However, it was applied to an object of type {0}."
    )]
    IndexAccessRequiresArray(RuntimeType),

    #[error(
        "The SymbolicOperation::NumArrayElements operation \
         returns the number of elements in an array.  \
         However, it was applied to an object of type {0}."
    )]
    ArrayLengthRequiresArray(RuntimeType),

    #[error(
        "The SymbolicOperation::ArrayExtent operation \
         returns the extent of a multi-dimensional array..  \
         However, it was applied to an object of type {0}."
    )]
    ArrayExtentRequiresMultiDimensionalArray(RuntimeType),

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
    DowncastRequiresClassInstance(RuntimeType),

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
    SymbolicExpressionMustProducePrimitive { field: String, ty: RuntimeType },

    #[error(
        "Currently, tuples are only supported \
         at the top level of an expression"
    )]
    TupleExpressionOnlySupportedAtTopLevel,

    #[error("Expected {expected}, but found {actual}")]
    UnexpectedRuntimeValue {
        expected: &'static str,
        actual: &'static str,
    },

    #[error("Not yet implemented: {0}")]
    NotImplementedYet(String),

    #[error("dotnet_debugger::VMExecutionError( {0} )")]
    VMExecutionError(#[from] crate::VMExecutionError),

    #[error("Unable to find inferred type for operation {0}")]
    InferredTypeNotFound(SymbolicValue),

    #[error("Invalid reference from expression {from} to {to}")]
    InvalidReference { from: OpIndex, to: OpIndex },

    #[error("Value '{0}' of type {1} not convertible to an index")]
    ValueNotConvertibleToIndex(RuntimePrimValue, RuntimePrimType),

    #[error("Cannot access array without a known component size")]
    AttemptedAccessOfArrayTypeWithUnknownComponentSize,

    #[error("Cannot add values with types {lhs} and {rhs}")]
    InvalidOperandsForAddition { lhs: RuntimeType, rhs: RuntimeType },

    #[error("Cannot multiply values with types {lhs} and {rhs}")]
    InvalidOperandsForMultiplication { lhs: RuntimeType, rhs: RuntimeType },

    #[error(
        "Symbolic expression must be lowered \
         prior to generating VM instruction, \
         but encountered {0}."
    )]
    SymbolicExpressionRequiresLowering(SymbolicExpr),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
