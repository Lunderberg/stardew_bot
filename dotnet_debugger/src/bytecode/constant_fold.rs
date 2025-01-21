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

            _ => None,
        };

        Ok(opt_value)
    }
}
