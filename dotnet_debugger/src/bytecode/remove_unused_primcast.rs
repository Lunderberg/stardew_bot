use crate::Error;

use super::{
    graph_rewrite::Analysis, ExprKind, GraphRewrite, SymbolicGraph,
    SymbolicValue,
};

pub struct RemoveUnusedPrimcast<'a>(pub &'a Analysis<'a>);

impl<'a> GraphRewrite for RemoveUnusedPrimcast<'a> {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            ExprKind::PrimCast { value, prim_type } => {
                let value_type = self.0.infer_type(graph, *value)?;
                (value_type == prim_type).then(|| *value)
            }

            _ => None,
        })
    }
}
