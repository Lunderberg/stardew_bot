use crate::{Error, GraphRewrite, SymbolicGraphSubstitute as _};
use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};

pub struct InlineFunctionCalls;

impl GraphRewrite for InlineFunctionCalls {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            ExprKind::FunctionCall {
                func: SymbolicValue::Result(func_index),
                args,
            } => match &graph[*func_index].kind {
                ExprKind::Function { params, output } => {
                    assert!(params.len() == args.len());

                    let replacements = params
                        .iter()
                        .zip(args)
                        .filter_map(|(param, arg)| {
                            param
                                .as_op_index()
                                .map(|param_index| (param_index, *arg))
                        })
                        .collect();

                    let output = *output;
                    Some(
                        graph
                            .substitute(replacements, output)?
                            .unwrap_or(output),
                    )
                }
                _ => None,
            },

            _ => None,
        })
    }
}
