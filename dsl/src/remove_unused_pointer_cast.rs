use dotnet_debugger::RuntimePrimType;

use crate::{Error, GraphRewrite};
use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};

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
