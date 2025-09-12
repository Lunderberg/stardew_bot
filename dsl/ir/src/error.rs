use thiserror::Error;

use crate::{DSLType, ParseError};

#[derive(Error)]
pub enum Error {
    #[error("memory_reader::Error{{ {err} }}")]
    DotnetDebugger {
        #[from]
        err: dotnet_debugger::Error,
    },

    #[error("dsl::ir::ParseError( {0} )")]
    ParseError(#[from] ParseError),

    #[error(
        "Type {0} found in a context \
         where only .NET types are expected."
    )]
    UnexpectedTypeFoundInDotNetContext(DSLType),

    #[error("MethodTable pointer for {0} was NULL.")]
    UnexpectedNullMethodTable(String),

    #[error(
        "The SymbolicOperation::Field(name) operation \
         accesses a field of a class or struct.  \
         However, it was applied to object '{0}' of type '{1}'."
    )]
    FieldAccessRequiresClassOrStruct(String, DSLType),

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
        "Vector operations require a vector operand, \
         but were applied to type '{0}'."
    )]
    InvalidVectorType(DSLType),

    #[error(
        "Vector are only supported when they contain \
         primitive elements, or rust-native types that are not vectors.  \
         Cannot construct a vector of type '{0}'. "
    )]
    InvalidVectorElementType(DSLType),

    #[error(
        "Excepted a vector element-type of {expected}, \
         but instead found a vector element-type of {actual}."
    )]
    IncorrectVectorElementType { expected: DSLType, actual: DSLType },

    #[error(
        "Collection of iterators into nested vectors \
         is not currently supported."
    )]
    CollectionIntoNestedVectorNotSupported,

    #[error(
        "When pushing into vector, \
         pushed element must not be None.  \
         However, attempted to push None \
         into vector '{name}'."
    )]
    MissingElementInVectorAccumulation { name: String },

    #[error(
        "Native function expecting {expected} arguments \
         was provided with {provided} arguments."
    )]
    InvalidNumberOfOperandsForNativeFunction {
        expected: usize,
        provided: usize,
    },

    #[error(
        "Rust-native function expected argument of type {expected}, \
         but received argument of type {actual}."
    )]
    InvalidArgumentForNativeFunction { expected: DSLType, actual: DSLType },

    #[error(
        "Interop with native function \
         with signature '{sig}' is not supported, \
         because {reason}."
    )]
    UnsupportedNativeFunction { sig: String, reason: String },

    #[error(
        "Invalid node name: '{0}'.  \
         Node names beginning with underscore \
         followed by a digit are reserved for internal use."
    )]
    AttemptedUseOfReservedName(String),

    #[error(
        "The mark_extern_func() function \
         may only be called on a function definition"
    )]
    AttemptedToMarkNonFunctionAsExternFunc,

    #[error("Functions marked as external must have an explicit name")]
    ExternalFunctionMustBeNamed,

    #[error("Invalid conversion from {0:?} to primitive.")]
    IllegalConversionToPrimitiveValue(DSLType),

    #[error("Invalid conversion from {0:?} to rust-native object.")]
    IllegalConversionToNativeObject(DSLType),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
