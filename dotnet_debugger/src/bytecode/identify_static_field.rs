use crate::{bytecode::expr::StaticField, Error};

use super::{
    graph_rewrite::Analysis, GraphRewrite, SymbolicExpr, SymbolicGraph,
    SymbolicValue,
};

pub struct IdentifyStaticField<'a>(pub &'a Analysis<'a>);

impl<'a> GraphRewrite for IdentifyStaticField<'a> {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &SymbolicExpr,
    ) -> Result<Option<SymbolicValue>, Error> {
        let SymbolicExpr::FieldAccess {
            obj: SymbolicValue::Result(obj_index),
            field: subfield_name,
        } = expr
        else {
            return Ok(None);
        };
        let SymbolicExpr::StaticField(StaticField {
            class: symbolic_type,
            field_name,
        }) = &graph[*obj_index]
        else {
            return Ok(None);
        };

        if !symbolic_type.generics.is_empty() {
            return Ok(None);
        }

        let class = symbolic_type.full_name.as_ref();

        if self.0.reader().class_exists(class)? {
            return Ok(None);
        }

        let full_name = format!("{class}.{field_name}");
        let static_field = graph.static_field(full_name, subfield_name);

        Ok(Some(static_field))
    }
}
