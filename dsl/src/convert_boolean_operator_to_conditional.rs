use crate::{Error, GraphRewrite};
use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};

pub struct ConvertBooleanOperatorToConditional;

impl GraphRewrite for ConvertBooleanOperatorToConditional {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match *expr {
            ExprKind::And { lhs, rhs } => Some(graph.if_else(lhs, rhs, false)),
            ExprKind::Or { lhs, rhs } => Some(graph.if_else(lhs, true, rhs)),
            _ => None,
        })
    }
}
