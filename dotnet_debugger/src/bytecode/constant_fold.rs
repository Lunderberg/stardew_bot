use crate::{Error, RuntimePrimValue};

use super::{ExprKind, GraphRewrite, SymbolicGraph, SymbolicValue};

pub struct ConstantFold;

impl GraphRewrite for ConstantFold {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        let opt_value = match expr {
            &ExprKind::Add {
                lhs: SymbolicValue::Const(RuntimePrimValue::NativeUInt(a)),
                rhs: SymbolicValue::Const(RuntimePrimValue::NativeUInt(b)),
            } => Some((a + b).into()),

            &ExprKind::Mul {
                lhs: SymbolicValue::Const(RuntimePrimValue::NativeUInt(a)),
                rhs: SymbolicValue::Const(RuntimePrimValue::NativeUInt(b)),
            } => Some((a * b).into()),

            // lhs + 0 => lhs
            // 0 + rhs => rhs
            ExprKind::Add {
                lhs: other,
                rhs: SymbolicValue::Const(RuntimePrimValue::NativeUInt(0)),
            }
            | ExprKind::Add {
                lhs: SymbolicValue::Const(RuntimePrimValue::NativeUInt(0)),
                rhs: other,
            } => Some(*other),

            // 0 * rhs => 0
            // lhs * 0 => 0
            ExprKind::Mul {
                lhs: SymbolicValue::Const(RuntimePrimValue::NativeUInt(0)),
                ..
            }
            | ExprKind::Mul {
                rhs: SymbolicValue::Const(RuntimePrimValue::NativeUInt(0)),
                ..
            } => Some(0usize.into()),

            // lhs * 1 => lhs
            // 1 * rhs => rhs
            ExprKind::Mul {
                lhs: other,
                rhs: SymbolicValue::Const(RuntimePrimValue::NativeUInt(1)),
            }
            | ExprKind::Mul {
                lhs: SymbolicValue::Const(RuntimePrimValue::NativeUInt(1)),
                rhs: other,
            } => Some(*other),

            // const + rhs => rhs + const
            &ExprKind::Add {
                lhs: lhs @ SymbolicValue::Const(_),
                rhs: rhs @ SymbolicValue::Result(_),
            } => Some(graph.add(rhs, lhs)),

            // const * rhs => rhs * const
            &ExprKind::Mul {
                lhs: lhs @ SymbolicValue::Const(_),
                rhs: rhs @ SymbolicValue::Result(_),
            } => Some(graph.mul(rhs, lhs)),

            // lhs + a + b => lhs + (a+b)
            &ExprKind::Add {
                lhs: SymbolicValue::Result(lhs),
                rhs: SymbolicValue::Const(b),
            } => match graph[lhs].as_ref() {
                &ExprKind::Add {
                    lhs,
                    rhs: SymbolicValue::Const(a),
                } => {
                    let const_sum = graph.add(a, b);
                    Some(graph.add(lhs, const_sum))
                }
                _ => None,
            },

            // lhs * a * b => lhs * (a*b)
            &ExprKind::Mul {
                lhs: SymbolicValue::Result(lhs),
                rhs: SymbolicValue::Const(b),
            } => match graph[lhs].as_ref() {
                &ExprKind::Mul {
                    lhs,
                    rhs: SymbolicValue::Const(a),
                } => {
                    let const_prod = graph.mul(a, b);
                    Some(graph.mul(lhs, const_prod))
                }
                _ => None,
            },

            &ExprKind::IsSome(SymbolicValue::Result(arg)) => {
                match &graph[arg].kind {
                    // (None).is_some() => false
                    &ExprKind::None => Some(false.into()),

                    // Function definitions are always non-None.
                    &ExprKind::Function { .. } => Some(true.into()),

                    _ => None,
                }
            }

            // (not true) == false
            // (not false) == true
            &ExprKind::Not {
                arg: SymbolicValue::Const(RuntimePrimValue::Bool(b)),
            } => Some((!b).into()),

            // (X or true) == (true or X) == true
            ExprKind::Or {
                lhs: SymbolicValue::Const(RuntimePrimValue::Bool(true)),
                ..
            }
            | ExprKind::Or {
                rhs: SymbolicValue::Const(RuntimePrimValue::Bool(true)),
                ..
            } => Some(true.into()),

            // (X or false) == (false or X) == X
            ExprKind::Or {
                lhs: SymbolicValue::Const(RuntimePrimValue::Bool(false)),
                rhs: other,
            }
            | ExprKind::Or {
                rhs: SymbolicValue::Const(RuntimePrimValue::Bool(false)),
                lhs: other,
            } => Some(*other),

            // (X and false) == (false and X) == false
            ExprKind::And {
                lhs: SymbolicValue::Const(RuntimePrimValue::Bool(false)),
                ..
            }
            | ExprKind::And {
                rhs: SymbolicValue::Const(RuntimePrimValue::Bool(false)),
                ..
            } => Some(false.into()),

            // (X and true) == (true and X) == X
            &ExprKind::And {
                lhs: SymbolicValue::Const(RuntimePrimValue::Bool(true)),
                rhs: other,
            }
            | &ExprKind::And {
                rhs: SymbolicValue::Const(RuntimePrimValue::Bool(true)),
                lhs: other,
            } => Some(other),

            // if true {if_branch} else {else_branch} => if_branch
            // if false {if_branch} else {else_branch} => else_branch
            &ExprKind::IfElse {
                condition: SymbolicValue::Const(RuntimePrimValue::Bool(true)),
                if_branch: always_branch,
                ..
            }
            | &ExprKind::IfElse {
                condition: SymbolicValue::Const(RuntimePrimValue::Bool(false)),
                else_branch: always_branch,
                ..
            } => Some(always_branch),

            _ => None,
        };

        Ok(opt_value)
    }
}
