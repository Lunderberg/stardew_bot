use super::{ExprKind, GraphRewrite, SymbolicValue};

/// Split up reduction of a chained iterator into sequential
/// reductions.
///
/// Before:
///      let iter_chain = iter_a.chain(iter_b);
///      iter_chain.reduce(initial, func)
///
/// After:
///      let reduced_a = iter_a.reduce(initial, func);
///      iter_b.reduce(reduced_a, func);
pub struct SplitIteratorChainReduce;

impl GraphRewrite for SplitIteratorChainReduce {
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
            } => match &graph[iterator].kind {
                &ExprKind::Chain(iter_a, iter_b) => {
                    let reduced_a = graph.reduce(initial, iter_a, reduction);
                    let reduced_b = graph.reduce(reduced_a, iter_b, reduction);

                    Some(reduced_b)
                }
                _ => None,
            },
            _ => None,
        })
    }
}
