use crate::{Error, GraphRewrite};
use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};

pub struct MergeRangeReduceToSimpleReduce;

impl GraphRewrite for MergeRangeReduceToSimpleReduce {
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
