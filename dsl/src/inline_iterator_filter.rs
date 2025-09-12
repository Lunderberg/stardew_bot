use crate::{CopyFirstParamExt as _, Error, GraphRewrite};
use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};

pub struct InlineIteratorFilter;

impl GraphRewrite for InlineIteratorFilter {
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            &ExprKind::Reduce {
                initial,
                iterator: SymbolicValue::Result(iterator),
                reduction,
            } => match &graph[iterator].kind {
                &ExprKind::Filter { iterator, filter } => {
                    let new_reduce_lhs = graph.copy_first_param(reduction);
                    let new_reduce_rhs = graph.copy_first_param(filter);

                    let condition =
                        graph.function_call(filter, vec![new_reduce_rhs]);
                    let reduced = graph.function_call(
                        reduction,
                        vec![new_reduce_lhs, new_reduce_rhs],
                    );
                    let filtered =
                        graph.if_else(condition, reduced, new_reduce_lhs);

                    let new_reduction = graph.function_def(
                        vec![new_reduce_lhs, new_reduce_rhs],
                        filtered,
                    );

                    let new_reduced_iter =
                        graph.reduce(initial, iterator, new_reduction);

                    Some(new_reduced_iter)
                }
                _ => None,
            },
            _ => None,
        })
    }
}
