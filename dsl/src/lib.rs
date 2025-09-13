pub use dotnet_debugger::{RuntimePrimType, RuntimePrimValue};
pub use memory_reader::Pointer;

pub use dsl_analysis as analysis;
pub use dsl_ir as ir;
pub use dsl_lowering as lowering;
pub use dsl_optimize as optimize;
pub use dsl_rewrite_utils as rewrite_utils;
pub use dsl_validation as validation;
pub use dsl_vm as vm;

pub use dsl_graph_comparison::GraphComparisonExt;
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

mod expr;
pub use expr::*;

pub use dsl_proc_macros::*;

mod identify_static_field;
pub use identify_static_field::IdentifyStaticField;

mod expr_to_virtual_machine;
