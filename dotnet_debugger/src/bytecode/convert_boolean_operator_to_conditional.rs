use super::{ExprKind, GraphRewrite, SymbolicValue};

pub struct ConvertBooleanOperatorToConditional;

impl GraphRewrite for ConvertBooleanOperatorToConditional {
    fn rewrite_expr(
        &self,
        graph: &mut super::SymbolicGraph,
        expr: &super::ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, crate::Error> {
        Ok(match expr {
            &ExprKind::And { lhs, rhs } => Some(graph.if_else(lhs, rhs, false)),
            &ExprKind::Or { lhs, rhs } => Some(graph.if_else(lhs, true, rhs)),
            _ => None,
        })
    }
}
