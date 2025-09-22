use dotnet_debugger::MethodTable;
use dsl_ir::{DSLType, RuntimePrimValue, TypedPointer};
use thiserror::Error;

#[derive(Error)]
pub enum Error {
    #[error("dll_unpacker::Error( {0} )")]
    DllUnpacker(#[from] dll_unpacker::Error),

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
        "Expressions of type {0} not convertible \
         to an array index"
    )]
    TypeNotConvertibleToIndex(DSLType),

    #[error(
        "Expected a static field, \
         but field '{field}' in class {class} \
         was an instance field."
    )]
    ExpectedStaticFieldButFoundInstanceField { class: String, field: String },

    #[error("MethodTable pointer for {0} was NULL.")]
    UnexpectedNullMethodTable(String),

    #[error("The method table of an array should contain the element size.")]
    ArrayMissingComponentSize,

    #[error("The method table of an array should contain the element type.")]
    ArrayMissingElementType,

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
        "Cannot downcast from '{0}' to '{1}', \
         because '{1}' is not a subclass of '{0}'"
    )]
    DowncastRequiresRelatedClasses(String, String),

    #[error(
        "The SymbolicOperation::ArrayExtent operation \
         returns the extent of a multi-dimensional array..  \
         However, it was applied to an object of type {0}."
    )]
    ArrayExtentRequiresMultiDimensionalArray(DSLType),

    #[error(
        "Expected method table to be pointer, \
         but instead found {0}, \
         which is of type {}",
        .0.runtime_type(),
    )]
    MethodTableShouldBePointer(RuntimePrimValue),

    #[error("Method table {0} has no fields")]
    MethodTableHasNoFields(TypedPointer<MethodTable>),

    #[error("Method table {ptr} does not have field {field}")]
    FieldNotFound {
        ptr: TypedPointer<MethodTable>,
        field: String,
    },

    #[error(
        "ExprKind::LazyStatic must have initialization function, \
         but instead had '{0}'."
    )]
    LazyStaticInitializationMustBeFunction(&'static str),

    #[error(
        "ExprKind::LazyStatic must have nullary initialization function, \
         but instead had a function with {0} parameters."
    )]
    LazyStaticInitializationMayNotHaveParams(usize),
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
