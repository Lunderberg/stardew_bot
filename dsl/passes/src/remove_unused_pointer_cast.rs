use dsl_ir::{ExprKind, RuntimePrimType, SymbolicGraph, SymbolicValue};
use dsl_rewrite_utils::GraphRewrite;

use crate::Error;

pub struct RemoveUnusedPointerCast;

impl GraphRewrite for RemoveUnusedPointerCast {
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            ExprKind::PrimCast {
                value: SymbolicValue::Result(op_index),
                prim_type: RuntimePrimType::Ptr,
            } => match &graph[*op_index].kind {
                ExprKind::PointerCast { ptr, .. } => Some(*ptr),
                _ => None,
            },

            _ => None,
        })
    }
}
