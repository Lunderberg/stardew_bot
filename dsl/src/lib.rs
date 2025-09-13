pub use dotnet_debugger::{RuntimePrimType, RuntimePrimValue};
pub use memory_reader::Pointer;

pub use dsl_analysis as analysis;
pub use dsl_ir as ir;
pub use dsl_lowering as lowering;
pub use dsl_rewrite_utils as rewrite_utils;
pub use dsl_validation as validation;
pub use dsl_vm as vm;

pub use dsl_graph_comparison::GraphComparisonExt;
pub use ir::{
    DSLType, ExposedNativeFunction, RustNativeObject, StackValue,
    SymbolicGraph, SymbolicType, SymbolicValue,
};
pub use rewrite_utils::{GraphRewrite, SymbolicGraphCSE, SymbolicGraphDCE};
pub use vm::VirtualMachine;

mod error;
pub use error::Error;

mod expr;
pub use expr::*;

pub use dsl_proc_macros::*;

mod identify_static_field;
pub use identify_static_field::IdentifyStaticField;

mod constant_fold;
pub use constant_fold::ConstantFold;

mod remove_unused_downcast;
pub use remove_unused_downcast::RemoveUnusedDowncast;

mod remove_unused_primcast;
pub use remove_unused_primcast::RemoveUnusedPrimcast;

mod merge_parallel_reads;
pub use merge_parallel_reads::MergeParallelReads;

mod remove_unused_pointer_cast;
pub use remove_unused_pointer_cast::RemoveUnusedPointerCast;

mod expr_to_virtual_machine;
