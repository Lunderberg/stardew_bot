use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};
use dsl_rewrite_utils::GraphRewrite;

use crate::Error;

pub struct MovePointerCastAfterConditional;

impl GraphRewrite for MovePointerCastAfterConditional {
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            ExprKind::IfElse {
                condition,
                if_branch: SymbolicValue::Result(if_index),
                else_branch: SymbolicValue::Result(else_index),
            } => {
                let condition = *condition;
                let if_expr = &graph[*if_index].kind;
                let else_expr = &graph[*else_index].kind;

                match (if_expr, else_expr) {
                    (ExprKind::PointerCast { ptr, ty }, ExprKind::None) => {
                        let ty = ty.clone();
                        let new_if = *ptr;
                        let new_else = graph.none();
                        let new_conditional =
                            graph.if_else(condition, new_if, new_else);
                        Some(graph.pointer_cast(new_conditional, ty))
                    }

                    (ExprKind::None, ExprKind::PointerCast { ptr, ty }) => {
                        let ty = ty.clone();
                        let new_else = *ptr;
                        let new_if = graph.none();
                        let new_conditional =
                            graph.if_else(condition, new_if, new_else);
                        Some(graph.pointer_cast(new_conditional, ty))
                    }

                    _ => None,
                }
            }
            _ => None,
        })
    }
}
