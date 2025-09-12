use crate::{Error, GraphRewrite};
use dsl_ir::{DSLType, ExprKind, SymbolicGraph, SymbolicValue};

pub struct ConvertFindMapToFilterFind;

impl GraphRewrite for ConvertFindMapToFilterFind {
    type Error = Error;

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

        let item = graph.function_arg(DSLType::Unknown);
        graph.name(item, "item")?;
        let item_not_none = graph.is_some(item);
        let find_condition = graph.function_def(vec![item], item_not_none);

        let mapped = graph.map(iterator, find_map);
        let result = graph.find(mapped, find_condition);

        Ok(Some(result))
    }
}
