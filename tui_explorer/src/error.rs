use memory_reader::Pointer;
use thiserror::Error;
use tui_utils::inputs::KeySequence;

#[derive(Error)]
pub enum Error {
    #[error("Path not convertible to UTF-8")]
    InvalidUTF8InPath,

    #[error("std::io::Error( {0} )")]
    Io(#[from] std::io::Error),

    #[error("tui_utils::Error( {0} )")]
    TuiUtilError(#[from] tui_utils::Error),

    #[error("memory_reader::Error{{ {err} }}")]
    MemoryReader {
        #[from]
        err: memory_reader::Error,
    },

    #[error("dll_unpacker::Error{{ {err} }}")]
    DLLUnpacker {
        #[from]
        err: dll_unpacker::Error,
    },

    #[error("dotnet_debugger::Error( {0} )")]
    DotnetDebugger(#[from] dotnet_debugger::Error),

    #[error("dsl::Error( {0} )")]
    DSL(#[from] dsl::Error),

    #[error("dsl::ir::Error( {0} )")]
    DslIr(#[from] dsl::ir::Error),

    #[error("dsl::vm::Error( {0} )")]
    DslVm(#[from] dsl::vm::Error),

    #[error("dsl::compile::Error( {0} )")]
    DslCompile(#[from] dsl::compile::Error),

    #[error("dsl::optimize::Error( {0} )")]
    DslOptimize(#[from] dsl::optimize::Error),

    #[error("stardew_utils::Error{{ {err} }}")]
    StardewUtilError {
        #[from]
        err: stardew_utils::Error,
    },

    #[error("UserConfig error: {err}")]
    UserConfigError {
        #[from]
        err: crate::user_config::Error,
    },

    #[error("No binding for '{0}'")]
    UnknownKeySequence(KeySequence),

    #[error("Could not find MemoryRegion containing pointer {0}")]
    PointerNotFound(Pointer),

    #[error("Could not find table of method tables")]
    MethodTableTableNotFound,

    #[error("Module class should only have pointers to canonical MethodTable")]
    MethodTableTableReferencedNonCanonicalMethodTable,

    #[error("The named annotation was not found")]
    AnnotationNotFound,

    #[error("The named symbol was not found")]
    SymbolNotFound,

    #[error("Could not find module named {0}")]
    RuntimeModuleNotFound(String),

    #[error("Could not find method table {0}")]
    MethodTableNotFound(&'static str),

    #[error("Method table of value type not found")]
    MissingMethodTableOfValueType,

    #[error(
        "Expected to find a unique instance of Game object, \
             but found {0} instances."
    )]
    UniqueGameObjectInstanceNotFound(usize),

    #[error("Not implemented yet: {0}")]
    NotImplementedYet(String),

    #[error("Cannot expand NULL field")]
    CannotExpandNullField,

    #[error(
        "Prefetch should contain all bytes required for unpacked, \
         but did not include {0}."
    )]
    BytesNotFoundInPrefetch(Pointer),

    #[error("Attempted to access MetadataDisplay at invalid index")]
    InvalidMetadataDisplayIndex,

    #[error("Found RuntimeType::Unknown in context requiring known type")]
    UnexpectedUnknownType,
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
