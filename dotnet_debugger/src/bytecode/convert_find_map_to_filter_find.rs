use crate::{Error, RuntimeType};

use super::{ExprKind, GraphRewrite, SymbolicGraph, SymbolicValue};

pub struct ConvertFindMapToFilterFind;

impl GraphRewrite for ConvertFindMapToFilterFind {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        let &ExprKind::FindMap {
            iterator,
            condition: find_map,
        } = expr
        else {
            return Ok(None);
        };

        let item = graph.function_arg(RuntimeType::Unknown);
        graph.name(item, "item")?;
        let item_not_none = graph.is_some(item);
        let find_condition = graph.function_def(vec![item], item_not_none);

        let mapped = graph.map(iterator, find_map);
        let result = graph.find(mapped, find_condition);

        Ok(Some(result))
    }
}
