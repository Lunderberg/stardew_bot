use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};

use crate::GraphRewrite;

pub struct SequentialRewrite<First, Second> {
    first: First,
    second: Second,
}

impl<First, Second> SequentialRewrite<First, Second> {
    pub fn new(first: First, second: Second) -> Self {
        Self { first, second }
    }
}

impl<First, Second, Error> GraphRewrite for SequentialRewrite<First, Second>
where
    First: GraphRewrite<Error = Error>,
    Second: GraphRewrite<Error = Error>,
{
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Self::Error> {
        if let Some(new_value) = self.first.rewrite_expr(graph, expr, name)? {
            Ok(Some(new_value))
        } else {
            self.second.rewrite_expr(graph, expr, name)
        }
    }
}
