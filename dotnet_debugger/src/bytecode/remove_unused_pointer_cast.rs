use crate::{runtime_type::RuntimePrimType, Error};

use super::{ExprKind, GraphRewrite, SymbolicGraph, SymbolicValue};

pub struct RemoveUnusedPointerCast;

impl GraphRewrite for RemoveUnusedPointerCast {
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
            } => match graph[*op_index].as_ref() {
                ExprKind::PointerCast { ptr, .. } => Some(*ptr),
                _ => None,
            },

            _ => None,
        })
    }
}
