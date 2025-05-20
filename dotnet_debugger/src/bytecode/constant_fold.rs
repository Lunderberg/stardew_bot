use crate::Error;

use super::{ExprKind, GraphRewrite, SymbolicGraph, SymbolicValue};

pub struct ConstantFold;

impl GraphRewrite for ConstantFold {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SymbolicValue>, Error> {
        let opt_value = match expr {
            &ExprKind::Add {
                lhs: SymbolicValue::Int(a),
                rhs: SymbolicValue::Int(b),
            } => Some(SymbolicValue::Int(a + b)),

            &ExprKind::Mul {
                lhs: SymbolicValue::Int(a),
                rhs: SymbolicValue::Int(b),
            } => Some(SymbolicValue::Int(a * b)),

            // lhs + 0 => lhs
            &ExprKind::Add {
                lhs,
                rhs: SymbolicValue::Int(0),
            } => Some(lhs),

            // 0 + rhs => rhs
            &ExprKind::Add {
                lhs: SymbolicValue::Int(0),
                rhs,
            } => Some(rhs),

            // 0 * rhs => 0
            ExprKind::Mul {
                rhs: SymbolicValue::Int(0),
                ..
            } => Some(SymbolicValue::Int(0)),

            // lhs * 0 => 0
            ExprKind::Mul {
                lhs: SymbolicValue::Int(0),
                ..
            } => Some(SymbolicValue::Int(0)),

            // lhs * 1 => lhs
            &ExprKind::Mul {
                lhs,
                rhs: SymbolicValue::Int(1),
            } => Some(lhs),

            // 1 * rhs => rhs
            &ExprKind::Mul {
                lhs: SymbolicValue::Int(1),
                rhs,
            } => Some(rhs),

            // const + rhs => rhs + const
            &ExprKind::Add {
                lhs: lhs @ SymbolicValue::Int(_),
                rhs,
            } => Some(graph.add(rhs, lhs)),

            // const * rhs => rhs * const
            &ExprKind::Mul {
                lhs: lhs @ SymbolicValue::Int(_),
                rhs,
            } => Some(graph.mul(rhs, lhs)),

            // lhs + a + b => lhs + (a+b)
            &ExprKind::Add {
                lhs: SymbolicValue::Result(lhs),
                rhs: SymbolicValue::Int(b),
            } => match graph[lhs].as_ref() {
                &ExprKind::Add {
                    lhs,
                    rhs: SymbolicValue::Int(a),
                } => Some(graph.add(lhs, a + b)),
                _ => None,
            },

            // lhs * a * b => lhs * (a*b)
            &ExprKind::Mul {
                lhs: SymbolicValue::Result(lhs),
                rhs: SymbolicValue::Int(b),
            } => match graph[lhs].as_ref() {
                &ExprKind::Mul {
                    lhs,
                    rhs: SymbolicValue::Int(a),
                } => Some(graph.mul(lhs, a * b)),
                _ => None,
            },

            &ExprKind::IsSome(SymbolicValue::Result(arg)) => {
                match &graph[arg].kind {
                    // (None).is_some() => false
                    &ExprKind::None => Some(SymbolicValue::Bool(false)),

                    // Function definitions are always non-None.
                    &ExprKind::Function { .. } => {
                        Some(SymbolicValue::Bool(true))
                    }

                    _ => None,
                }
            }

            // (not true) == false
            // (not false) == true
            &ExprKind::Not {
                arg: SymbolicValue::Bool(b),
            } => Some(SymbolicValue::Bool(!b)),

            // (X or true) == (true or X) == true
            ExprKind::Or {
                lhs: SymbolicValue::Bool(true),
                ..
            }
            | ExprKind::Or {
                rhs: SymbolicValue::Bool(true),
                ..
            } => Some(SymbolicValue::Bool(true)),

            // (X or false) == (false or X) == X
            ExprKind::Or {
                lhs: SymbolicValue::Bool(false),
                rhs: other,
            }
            | ExprKind::Or {
                rhs: SymbolicValue::Bool(false),
                lhs: other,
            } => Some(*other),

            // (X and false) == (false and X) == false
            ExprKind::And {
                lhs: SymbolicValue::Bool(false),
                ..
            }
            | ExprKind::And {
                rhs: SymbolicValue::Bool(false),
                ..
            } => Some(SymbolicValue::Bool(false)),

            // (X and true) == (true and X) == X
            &ExprKind::And {
                lhs: SymbolicValue::Bool(true),
                rhs: other,
            }
            | &ExprKind::And {
                rhs: SymbolicValue::Bool(true),
                lhs: other,
            } => Some(other),

            // if true {if_branch} else {else_branch} => if_branch
            // if false {if_branch} else {else_branch} => else_branch
            &ExprKind::IfElse {
                condition: SymbolicValue::Bool(true),
                if_branch: always_branch,
                ..
            }
            | &ExprKind::IfElse {
                condition: SymbolicValue::Bool(false),
                else_branch: always_branch,
                ..
            } => Some(always_branch),

            _ => None,
        };

        Ok(opt_value)
    }
}
