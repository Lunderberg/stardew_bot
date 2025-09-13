use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};
use dsl_rewrite_utils::GraphRewrite;

use crate::Error;

pub struct SeparateReadAndParseBytes;

impl GraphRewrite for SeparateReadAndParseBytes {
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            &ExprKind::ReadPrim { ptr, prim_type } => {
                let bytes = graph.read_bytes(ptr, prim_type.size_bytes());
                let value = graph.cast_bytes(bytes, 0usize, prim_type);
                Some(value)
            }
            _ => None,
        })
    }
}
