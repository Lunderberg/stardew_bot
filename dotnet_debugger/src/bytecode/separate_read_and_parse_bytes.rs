use super::{ExprKind, GraphRewrite, SymbolicValue};

pub struct SeparateReadAndParseBytes;

impl GraphRewrite for SeparateReadAndParseBytes {
    fn rewrite_expr(
        &self,
        graph: &mut super::SymbolicGraph,
        expr: &super::ExprKind,
    ) -> Result<Option<SymbolicValue>, crate::Error> {
        Ok(match expr {
            &ExprKind::ReadPrim { ptr, prim_type } => {
                let bytes = graph.read_bytes(ptr, prim_type.size_bytes());
                let value = graph.cast_bytes(bytes, 0, prim_type);
                Some(value)
            }
            _ => None,
        })
    }
}
