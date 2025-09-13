use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};
use dsl_rewrite_utils::GraphRewrite;

use crate::Error;

pub struct ConvertFindToFilterFirst;

impl GraphRewrite for ConvertFindToFilterFirst {
    type Error = Error;

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
