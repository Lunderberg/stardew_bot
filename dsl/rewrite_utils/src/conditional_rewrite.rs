use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};

use crate::GraphRewrite;

pub struct ConditionalRewrite<Inner> {
    inner: Inner,
    enabled: bool,
}

impl<Inner> ConditionalRewrite<Inner> {
    pub fn new(inner: Inner, enabled: bool) -> Self {
        Self { inner, enabled }
    }
}

impl<Inner> GraphRewrite for ConditionalRewrite<Inner>
where
    Inner: GraphRewrite,
{
    type Error = Inner::Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Self::Error> {
        if self.enabled {
            self.inner.rewrite_expr(graph, expr, name)
        } else {
            Ok(None)
        }
    }
}
