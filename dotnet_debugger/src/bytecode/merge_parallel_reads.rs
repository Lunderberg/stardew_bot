use crate::Error;

use super::{ExprKind, GraphRewrite, SymbolicGraph, SymbolicValue};

pub struct MergeParallelReads;

impl GraphRewrite for MergeParallelReads {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            _ => None,
        })
    }
}
