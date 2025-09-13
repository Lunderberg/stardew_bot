use dsl_ir::{DSLType, ExprKind, SymbolicGraph, SymbolicValue};
use dsl_rewrite_utils::GraphRewrite;

use crate::Error;

pub struct ConvertFirstToReduce;

impl GraphRewrite for ConvertFirstToReduce {
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        let &ExprKind::First { iterator } = expr else {
            return Ok(None);
        };

        let initial = graph.none();
        let prev = graph.function_arg(DSLType::Unknown);
        graph.name(prev, "prev")?;
        let item = graph.function_arg(DSLType::Unknown);
        graph.name(item, "item")?;

        let condition = graph.is_some(prev);
        let reduced = graph.if_else(condition, prev, item);

        let reduction = graph.function_def(vec![prev, item], reduced);

        let result = graph.reduce(initial, iterator, reduction);

        Ok(Some(result))
    }
}
