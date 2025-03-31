mod expr;
pub use expr::{ExprKind, SymbolicGraph, SymbolicType, SymbolicValue};

mod graph_rewrite;
pub use graph_rewrite::GraphRewrite;

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

mod sequential_rewrite;
pub use sequential_rewrite::SequentialRewrite;

mod lower_symbolic_expr;
pub use lower_symbolic_expr::LowerSymbolicExpr;

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

mod convert_collect_to_reduce;
pub use convert_collect_to_reduce::ConvertCollectToReduce;

mod type_inference;
pub use type_inference::{TypeInference, TypeInferenceError};

mod parser;
pub(crate) use parser::*;

mod printer;

mod op_index;
pub use op_index::OpIndex;

mod expr_to_virtual_machine;

pub mod virtual_machine;
pub use virtual_machine::{
    Instruction, StackValue, VMArg, VMExecutionError, VMResults, VirtualMachine,
};

pub mod native_function;
pub use native_function::{NativeFunction, RustNativeObject};

mod exposed_native_function;
pub use exposed_native_function::ExposedNativeFunction;

mod exposed_native_object;
pub(crate) use exposed_native_object::{
    ExposedNativeObject, RustNativeTypeUtils, RustNativeUtilContainer,
};
