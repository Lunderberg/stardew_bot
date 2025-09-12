use crate::{Error, GraphRewrite};
use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};

pub struct ConvertFindToFilterFirst;

impl GraphRewrite for ConvertFindToFilterFirst {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        let &ExprKind::Find {
            iterator,
            condition,
        } = expr
        else {
            return Ok(None);
        };

        let filtered = graph.filter(iterator, condition);
        let first_filtered = graph.first(filtered);

        Ok(Some(first_filtered))
    }
}
