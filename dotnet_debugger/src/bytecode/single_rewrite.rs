use std::cell::Cell;

use crate::Error;

use super::{ExprKind, GraphRewrite, SymbolicGraph, SymbolicValue};

pub struct SingleRewrite<Inner> {
    inner: Inner,
    can_rewrite: Cell<bool>,
}

impl<Inner> SingleRewrite<Inner> {
    pub fn new(inner: Inner) -> Self {
        Self {
            inner,
            can_rewrite: true.into(),
        }
    }
}

impl<Inner> GraphRewrite for SingleRewrite<Inner>
where
    Inner: GraphRewrite,
{
    fn init(&self) {
        self.can_rewrite.set(true);
    }

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok((self.can_rewrite.get())
            .then(|| self.inner.rewrite_expr(graph, expr))
            .transpose()?
            .flatten()
            .inspect(|_| {
                self.can_rewrite.set(false);
            }))
    }
}
