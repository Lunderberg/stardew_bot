pub use dotnet_debugger::{RuntimePrimType, RuntimePrimValue};
pub use memory_reader::Pointer;

pub use dsl_analysis as analysis;
pub use dsl_ir as ir;
pub use dsl_vm as vm;

pub use ir::{
    DSLType, ExposedNativeFunction, RustNativeObject, StackValue,
    SymbolicGraph, SymbolicType, SymbolicValue,
};
pub use vm::VirtualMachine;

mod error;
pub use error::Error;

mod expr;
pub use expr::*;

pub use dsl_proc_macros::*;

mod graph_rewrite;
pub use graph_rewrite::*;

mod identify_static_field;
pub use identify_static_field::IdentifyStaticField;

mod constant_fold;
pub use constant_fold::ConstantFold;

mod remove_unused_downcast;
pub use remove_unused_downcast::RemoveUnusedDowncast;

mod remove_unused_primcast;
pub use remove_unused_primcast::RemoveUnusedPrimcast;

mod recursive_rewrite;
pub use recursive_rewrite::RecursiveRewrite;

mod single_rewrite;
pub use single_rewrite::SingleRewrite;

mod sequential_rewrite;
pub use sequential_rewrite::SequentialRewrite;

mod lower_symbolic_expr;
pub use lower_symbolic_expr::LowerSymbolicExpr;

mod separate_read_and_parse_bytes;
pub use separate_read_and_parse_bytes::SeparateReadAndParseBytes;

mod merge_parallel_reads;
pub use merge_parallel_reads::MergeParallelReads;

mod remove_unused_pointer_cast;
pub use remove_unused_pointer_cast::RemoveUnusedPointerCast;

mod inline_function_calls;
pub use inline_function_calls::InlineFunctionCalls;

mod merge_range_reduce_to_simple_reduce;
pub use merge_range_reduce_to_simple_reduce::MergeRangeReduceToSimpleReduce;

mod inline_iterator_map;
pub use inline_iterator_map::InlineIteratorMap;

mod inline_iterator_filter;
pub use inline_iterator_filter::InlineIteratorFilter;

mod split_iterator_chain_reduce;
pub use split_iterator_chain_reduce::SplitIteratorChainReduce;

mod convert_collect_to_reduce;
pub use convert_collect_to_reduce::ConvertCollectToReduce;

mod convert_find_to_filter_first;
pub use convert_find_to_filter_first::ConvertFindToFilterFirst;

mod convert_first_to_reduce;
pub use convert_first_to_reduce::ConvertFirstToReduce;

mod convert_find_map_to_filter_find;
pub use convert_find_map_to_filter_find::ConvertFindMapToFilterFind;

mod convert_boolean_operator_to_conditional;
pub use convert_boolean_operator_to_conditional::ConvertBooleanOperatorToConditional;

mod infer_function_parameter_types;
pub use infer_function_parameter_types::InferFunctionParameterTypes;

mod legalize_operand_types;
pub use legalize_operand_types::LegalizeOperandTypes;

mod expr_to_virtual_machine;
