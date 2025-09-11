use super::{ExprKind, GraphRewrite, SymbolicValue};

pub struct MergeRangeReduceToSimpleReduce;

impl GraphRewrite for MergeRangeReduceToSimpleReduce {
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
                &ExprKind::Range { extent } => {
                    let simple_reduce =
                        graph.simple_reduce(initial, extent, reduction);
                    Some(simple_reduce)
                }
                _ => None,
            },
            _ => None,
        })
    }
}
