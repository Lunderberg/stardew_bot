use super::{ExprKind, GraphRewrite, SymbolicValue};

pub struct InlineIteratorMap;

impl GraphRewrite for InlineIteratorMap {
    fn rewrite_expr(
        &self,
        graph: &mut super::SymbolicGraph,
        expr: &super::ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, crate::Error> {
        Ok(match expr {
            &ExprKind::Reduce {
                initial,
                iterator: SymbolicValue::Result(iterator),
                reduction,
            } => match graph[iterator].as_ref() {
                &ExprKind::Map { iterator, map } => {
                    let new_reduce_lhs = graph.copy_first_param(reduction);
                    let new_reduce_rhs = graph.copy_first_param(map);

                    let mapped = graph.function_call(map, vec![new_reduce_rhs]);
                    let reduced = graph
                        .function_call(reduction, vec![new_reduce_lhs, mapped]);

                    let new_reduction = graph.function_def(
                        vec![new_reduce_lhs, new_reduce_rhs],
                        reduced,
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
