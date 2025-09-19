use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};
use dsl_rewrite_utils::GraphRewrite;

use crate::Error;

pub struct NoOpRewrite;

impl GraphRewrite for NoOpRewrite {
    type Error = Error;

    fn rewrite_expr(
        &self,
        _graph: &mut SymbolicGraph,
        _expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Self::Error> {
        Ok(None)
    }
}
