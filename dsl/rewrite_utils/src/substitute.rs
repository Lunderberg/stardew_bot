use std::collections::HashMap;

use dsl_ir::{Expr, ExprKind, OpIndex, SymbolicGraph, SymbolicValue};

use crate::{Error, GraphRewrite};

pub trait SymbolicGraphSubstitute {
    fn substitute(
        &mut self,
        replacements: HashMap<OpIndex, SymbolicValue>,
        value: SymbolicValue,
    ) -> Result<Option<SymbolicValue>, Error>;
}
impl SymbolicGraphSubstitute for SymbolicGraph {
    fn substitute(
        &mut self,
        mut replacements: HashMap<OpIndex, SymbolicValue>,
        value: SymbolicValue,
    ) -> Result<Option<SymbolicValue>, Error> {
        let Some(index) = value.as_op_index() else {
            return Ok(None);
        };
        if replacements.is_empty() {
            return Ok(None);
        }

        let subgraph = loop {
            let subgraph = self.collect_subgraph(
                replacements.keys().map(|key| (*key).into()),
                Some(value),
            );

            let mut must_also_replace_function = false;

            subgraph
                .iter()
                .cloned()
                .filter(|index| !replacements.contains_key(index))
                .filter_map(|index| match &self[index].kind {
                    ExprKind::Function { params, .. } => Some(params),
                    _ => None,
                })
                .flatten()
                .filter_map(|value| value.as_op_index())
                .filter(|index| !replacements.contains_key(index))
                .collect::<Vec<_>>()
                .into_iter()
                .for_each(|index| {
                    let ExprKind::FunctionArg(arg_ty) = &self[index].kind
                    else {
                        unreachable!(
                            "Ill-formed SymbolicGraph, \
                             function params must point to FunctionArg."
                        )
                    };
                    let new_arg = self.function_arg(arg_ty.clone());
                    if let Some(name) = self[index].name.clone() {
                        self.name(new_arg, name)
                            .expect("Existing name must be valid");
                    }

                    must_also_replace_function = true;
                    replacements.insert(index, new_arg);
                });

            if !must_also_replace_function {
                break subgraph;
            }
        };

        let to_rewrite = subgraph
            .into_iter()
            .filter(|index| !replacements.contains_key(index));

        let mut rewrites = HashMap::new();

        self.rewrite_subtree(
            Substitute(&replacements),
            to_rewrite,
            &mut rewrites,
        )?;

        return Ok(rewrites.get(&index).cloned());

        struct Substitute<'a>(&'a HashMap<OpIndex, SymbolicValue>);

        impl GraphRewrite for Substitute<'_> {
            type Error = Error;

            fn rewrite_expr(
                &self,
                graph: &mut SymbolicGraph,
                expr: &ExprKind,
                _name: Option<&str>,
            ) -> Result<Option<SymbolicValue>, Error> {
                Ok(expr.try_remap(self.0).map(|remapped| graph.push(remapped)))
            }
        }
    }
}

trait RewriteSubtree {
    fn rewrite_subtree(
        &mut self,
        rewriter: impl GraphRewrite<Error = Error>,
        indices: impl Iterator<Item = OpIndex>,
        rewrites_applied: &mut HashMap<OpIndex, SymbolicValue>,
    ) -> Result<(), Error>;
}
impl RewriteSubtree for SymbolicGraph {
    fn rewrite_subtree(
        &mut self,
        rewriter: impl GraphRewrite<Error = Error>,
        indices: impl Iterator<Item = OpIndex>,
        rewrites_applied: &mut HashMap<OpIndex, SymbolicValue>,
    ) -> Result<(), Error> {
        rewriter.init();

        for old_index in indices {
            let op = self[old_index].clone();
            let opt_remapped = op.kind.try_remap(rewrites_applied);
            let kind = opt_remapped.as_ref().unwrap_or(&op.kind);

            let opt_new_value = if let Some(rewritten) =
                rewriter.rewrite_expr(self, kind, op.name.as_deref())?
            {
                if let Some(name) = &op.name {
                    if let SymbolicValue::Result(new_index) = rewritten {
                        if self[new_index].name.is_none() {
                            self.name(rewritten, name)?;
                        }
                    }
                }
                Some(rewritten)
            } else if let Some(remapped) = opt_remapped {
                let expr = Expr {
                    kind: remapped,
                    name: op.name,
                };

                Some(self.push(expr))
            } else {
                None
            };

            if let Some(new_value) = opt_new_value {
                rewrites_applied.insert(old_index, new_value);
            }
        }

        Ok(())
    }
}
