use crate::{runtime_type::RuntimePrimType, Error};

use super::{GraphRewrite, SymbolicExpr, SymbolicGraph, SymbolicValue};

pub struct RemoveUnusedPointerCast;

impl GraphRewrite for RemoveUnusedPointerCast {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &SymbolicExpr,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            SymbolicExpr::PrimCast {
                value: SymbolicValue::Result(op_index),
                prim_type: RuntimePrimType::Ptr,
            } => match graph[*op_index] {
                SymbolicExpr::PointerCast { ptr, .. } => Some(ptr),
                _ => None,
            },

            _ => None,
        })
    }
}
