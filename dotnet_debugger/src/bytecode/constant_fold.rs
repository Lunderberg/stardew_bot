use crate::Error;

use super::{GraphRewrite, SymbolicExpr, SymbolicGraph, SymbolicValue};

pub struct ConstantFold;

impl GraphRewrite for ConstantFold {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &SymbolicExpr,
    ) -> Result<Option<SymbolicValue>, Error> {
        let opt_value = match expr {
            &SymbolicExpr::Add {
                lhs: SymbolicValue::Int(a),
                rhs: SymbolicValue::Int(b),
            } => Some(SymbolicValue::Int(a + b)),

            &SymbolicExpr::Mul {
                lhs: SymbolicValue::Int(a),
                rhs: SymbolicValue::Int(b),
            } => Some(SymbolicValue::Int(a * b)),

            // lhs + 0 => lhs
            &SymbolicExpr::Add {
                lhs,
                rhs: SymbolicValue::Int(0),
            } => Some(lhs),

            // 0 + rhs => rhs
            &SymbolicExpr::Add {
                lhs: SymbolicValue::Int(0),
                rhs,
            } => Some(rhs),

            // 0 * rhs => 0
            SymbolicExpr::Mul {
                rhs: SymbolicValue::Int(0),
                ..
            } => Some(SymbolicValue::Int(0)),

            // lhs * 0 => 0
            SymbolicExpr::Mul {
                lhs: SymbolicValue::Int(0),
                ..
            } => Some(SymbolicValue::Int(0)),

            // lhs * 1 => lhs
            &SymbolicExpr::Mul {
                lhs,
                rhs: SymbolicValue::Int(1),
            } => Some(lhs),

            // 1 * rhs => rhs
            &SymbolicExpr::Mul {
                lhs: SymbolicValue::Int(1),
                rhs,
            } => Some(rhs),

            // const + rhs => rhs + const
            &SymbolicExpr::Add {
                lhs: lhs @ SymbolicValue::Int(_),
                rhs,
            } => Some(graph.add(rhs, lhs)),

            // const * rhs => rhs * const
            &SymbolicExpr::Mul {
                lhs: lhs @ SymbolicValue::Int(_),
                rhs,
            } => Some(graph.mul(rhs, lhs)),

            // lhs + a + b => lhs + (a+b)
            &SymbolicExpr::Add {
                lhs: SymbolicValue::Result(lhs),
                rhs: SymbolicValue::Int(b),
            } => match &graph[lhs] {
                &SymbolicExpr::Add {
                    lhs,
                    rhs: SymbolicValue::Int(a),
                } => Some(graph.add(lhs, a + b)),
                _ => None,
            },

            // lhs * a * b => lhs * (a*b)
            &SymbolicExpr::Mul {
                lhs: SymbolicValue::Result(lhs),
                rhs: SymbolicValue::Int(b),
            } => match &graph[lhs] {
                &SymbolicExpr::Mul {
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
