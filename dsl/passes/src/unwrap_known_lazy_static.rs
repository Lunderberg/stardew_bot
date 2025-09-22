use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};
use dsl_rewrite_utils::GraphRewrite;

use crate::Error;

pub struct UnwrapKnownLazyStatic;

impl GraphRewrite for UnwrapKnownLazyStatic {
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        let ExprKind::LazyStatic {
            init_func: SymbolicValue::Result(init_func_index),
        } = expr
        else {
            return Ok(None);
        };

        let func_output = match &graph[*init_func_index].kind {
            ExprKind::Function { params, .. } if !params.is_empty() => {
                return Err(Error::LazyStaticInitializationMayNotHaveParams(
                    params.len(),
                ));
            }
            ExprKind::Function { output, .. } => output,
            other => {
                return Err(Error::LazyStaticInitializationMustBeFunction(
                    other.op_name(),
                ));
            }
        };

        let known_value = match func_output {
            SymbolicValue::Const(value) => *value,
            _ => return Ok(None),
        };

        Ok(Some(known_value.into()))
    }
}
