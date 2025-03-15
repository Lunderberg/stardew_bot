use std::collections::HashMap;

use crate::Error;

use super::{ExprKind, GraphRewrite, OpIndex, SymbolicGraph, SymbolicValue};

pub struct InlineFunctionCalls;

impl GraphRewrite for InlineFunctionCalls {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            ExprKind::FunctionCall {
                func: SymbolicValue::Result(func_index),
                args,
            } => match graph[*func_index].as_ref() {
                ExprKind::Function { params, output } => {
                    let output = *output;
                    let subgraph: Vec<_> = graph
                        .collect_subgraph(params.iter().cloned(), Some(output))
                        .into_iter()
                        .filter(|index| {
                            params.iter().all(|param| {
                                SymbolicValue::Result(*index) != *param
                            })
                        })
                        .collect();
                    let mut rewrites = HashMap::new();
                    graph.rewrite_subtree(
                        Substitute::new(params, args),
                        subgraph.into_iter(),
                        &mut rewrites,
                    )?;

                    let new_output = match output {
                        SymbolicValue::Result(op_index) => {
                            rewrites.get(&op_index).cloned()
                        }
                        _ => None,
                    }
                    .unwrap_or(output);
                    Some(new_output)
                }
                _ => None,
            },

            _ => None,
        })
    }
}

struct Substitute {
    lookup: HashMap<OpIndex, SymbolicValue>,
}

impl Substitute {
    fn new(params: &[SymbolicValue], args: &[SymbolicValue]) -> Self {
        assert!(params.len() == args.len());
        let lookup = params
            .iter()
            .zip(args)
            .filter_map(|(param, arg)| {
                param.as_op_index().map(|param_index| (param_index, *arg))
            })
            .collect();
        Self { lookup }
    }
}

impl GraphRewrite for Substitute {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(expr
            .try_remap(&self.lookup)
            .map(|remapped| graph.push(remapped)))
    }
}
