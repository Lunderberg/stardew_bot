pub use dotnet_debugger::{RuntimePrimType, RuntimePrimValue};
pub use memory_reader::Pointer;

pub use dsl_analysis as analysis;
pub use dsl_compile as compile;
pub use dsl_graph_comparison as graph_comparison;
pub use dsl_ir as ir;
pub use dsl_lowering as lowering;
pub use dsl_optimize as optimize;
pub use dsl_rewrite_utils as rewrite_utils;
pub use dsl_validation as validation;
pub use dsl_vm as vm;

pub use compile::{SymbolicGraphCompile, SymbolicGraphCompiler};
pub use graph_comparison::GraphComparisonExt;
pub use ir::{
    DSLType, ExposedNativeFunction, RustNativeObject, StackValue,
    SymbolicGraph, SymbolicType, SymbolicValue,
};
pub use rewrite_utils::{
    GraphRewrite, SymbolicGraphCSE, SymbolicGraphDCE, SymbolicGraphRewrite,
};
pub use vm::VirtualMachine;

mod error;
pub use error::Error;

pub use dsl_proc_macros::*;
