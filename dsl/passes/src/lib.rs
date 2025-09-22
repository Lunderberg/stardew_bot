mod error;
pub use error::*;

mod passes;
pub use passes::*;

mod no_op_rewrite;
pub(crate) use no_op_rewrite::*;

mod constant_fold;
pub(crate) use constant_fold::*;

mod remove_unused_downcast;
pub(crate) use remove_unused_downcast::*;

mod remove_unused_primcast;
pub(crate) use remove_unused_primcast::*;

mod remove_unused_pointer_cast;
pub(crate) use remove_unused_pointer_cast::*;

mod merge_parallel_reads;
pub(crate) use merge_parallel_reads::*;

mod copy_first_param;
pub(crate) use copy_first_param::*;

mod convert_boolean_operator_to_conditional;
pub(crate) use convert_boolean_operator_to_conditional::*;

mod convert_collect_to_reduce;
pub(crate) use convert_collect_to_reduce::*;

mod convert_find_map_to_filter_find;
pub(crate) use convert_find_map_to_filter_find::*;

mod convert_find_to_filter_first;
pub(crate) use convert_find_to_filter_first::*;

mod convert_first_to_reduce;
pub(crate) use convert_first_to_reduce::*;

mod infer_function_parameter_types;
pub(crate) use infer_function_parameter_types::*;

mod inline_function_calls;
pub(crate) use inline_function_calls::*;

mod inline_iterator_filter;
pub(crate) use inline_iterator_filter::*;

mod inline_iterator_map;
pub(crate) use inline_iterator_map::*;

mod legalize_operand_types;
pub(crate) use legalize_operand_types::*;

mod lower_symbolic_expr;
pub(crate) use lower_symbolic_expr::*;

mod merge_range_reduce_to_simple_reduce;
pub(crate) use merge_range_reduce_to_simple_reduce::*;

mod separate_read_and_parse_bytes;
pub(crate) use separate_read_and_parse_bytes::*;

mod split_iterator_chain_reduce;
pub(crate) use split_iterator_chain_reduce::*;

mod move_pointer_cast_after_conditional;
pub(crate) use move_pointer_cast_after_conditional::*;

mod unwrap_known_lazy_static;
pub(crate) use unwrap_known_lazy_static::*;
