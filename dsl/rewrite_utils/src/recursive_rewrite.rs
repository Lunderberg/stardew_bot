use std::collections::HashMap;

use dsl_ir::{ExprKind, OpIndex, SymbolicGraph, SymbolicValue};

use crate::GraphRewrite;

pub struct RecursiveRewrite<Inner> {
    inner: Inner,
}

impl<Inner> RecursiveRewrite<Inner> {
    pub fn new(inner: Inner) -> Self {
        Self { inner }
    }
}

impl<Inner> GraphRewrite for RecursiveRewrite<Inner>
where
    Inner: GraphRewrite,
{
    type Error = <Inner as GraphRewrite>::Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Self::Error> {
        // All nodes currently in the output graph have been fully
        // simplified.  If a rewrite produces a known node, then it
        // can be returned without any further simplification.
        let initial_size = graph.num_operations();

        let Some(mut value) = self.inner.rewrite_expr(graph, expr, name)?
        else {
            // Early bailout for the common case where the node does
            // not require any rewriting.
            return Ok(None);
        };

        // The rewriter may have produced a nested expression,
        // requiring intermediate nodes.  Those intermediate nodes
        // should also be simplified.
        let mut lookup_simplified = HashMap::<OpIndex, SymbolicValue>::new();
        let mut validated_nodes = initial_size;
        while validated_nodes < graph.num_operations() {
            let index = OpIndex(validated_nodes);
            let intermediate_expr = {
                let mut expr: ExprKind = graph[index].kind.clone();
                while let Some(remapped) = expr.try_remap(&lookup_simplified) {
                    expr = remapped;
                }
                expr
            };

            let opt_simplified = self
                .inner
                .rewrite_expr(graph, &intermediate_expr, name)?
                .or_else(|| {
                    (intermediate_expr != graph[index].kind)
                        .then(|| graph.push(intermediate_expr))
                });

            if let Some(simplified) = opt_simplified {
                lookup_simplified.insert(index, simplified);
            }

            validated_nodes += 1;
        }

        // Now that all intermediate nodes have been simplified, the
        // initial expression may have been simplified.
        while let SymbolicValue::Result(index) = value {
            if let Some(simplified) = lookup_simplified.get(&index) {
                value = *simplified;
            } else {
                break;
            }
        }

        Ok(Some(value))
    }
}
