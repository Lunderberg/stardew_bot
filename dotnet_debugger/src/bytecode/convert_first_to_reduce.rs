use crate::{Error, RuntimeType};

use super::{ExprKind, GraphRewrite, SymbolicGraph, SymbolicValue};

pub struct ConvertFirstToReduce;

impl GraphRewrite for ConvertFirstToReduce {
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
        let prev = graph.function_arg(RuntimeType::Unknown);
        graph.name(prev, "prev")?;
        let item = graph.function_arg(RuntimeType::Unknown);
        graph.name(item, "item")?;

        let condition = graph.is_some(prev);
        let reduced = graph.if_else(condition, prev, item);

        let reduction = graph.function_def(vec![prev, item], reduced);

        let result = graph.reduce(initial, iterator, reduction);

        Ok(Some(result))
    }
}
