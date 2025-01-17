use crate::Error;

use super::{GraphRewrite, SymbolicExpr, SymbolicGraph, SymbolicValue};

pub struct RecursiveRewrite<Inner> {
    inner: Inner,
}

impl<Inner> RecursiveRewrite<Inner> {
    pub fn new(inner: Inner) -> Self {
        Self { inner }
    }
}

impl<Inner> GraphRewrite for RecursiveRewrite<Inner>
where
    Inner: GraphRewrite,
{
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &SymbolicExpr,
    ) -> Result<Option<SymbolicValue>, Error> {
        let Some(mut value) = self.inner.rewrite_expr(graph, expr)? else {
            return Ok(None);
        };

        loop {
            let SymbolicValue::Result(index) = value else {
                return Ok(Some(value));
            };

            // The clone is unfortunate, but is necessary since the
            // rewriter has mutable access to the graph.  And the
            // mutable access is required in case rewrites require
            // additional internal nodes.
            let expr = &graph[index].clone();

            if let Some(new_value) = self.inner.rewrite_expr(graph, &expr)? {
                value = new_value;
            } else {
                return Ok(Some(value));
            }
        }
    }
}
