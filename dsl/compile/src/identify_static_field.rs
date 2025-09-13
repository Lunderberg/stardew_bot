use dsl_analysis::Analysis;
use dsl_ir::{ExprKind, StaticField, SymbolicGraph, SymbolicValue};
use dsl_rewrite_utils::GraphRewrite;

use crate::Error;

pub struct IdentifyStaticField<'a>(pub &'a Analysis<'a>);

impl<'a> GraphRewrite for IdentifyStaticField<'a> {
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        let ExprKind::FieldAccess {
            obj: SymbolicValue::Result(obj_index),
            field: subfield_name,
        } = expr
        else {
            return Ok(None);
        };
        let ExprKind::StaticField(StaticField {
            class: symbolic_type,
            field_name,
        }) = &graph[*obj_index].kind
        else {
            return Ok(None);
        };

        if !symbolic_type.generics.is_empty() {
            return Ok(None);
        }

        let class = symbolic_type.full_name.as_ref();

        if self.0.reader()?.class_exists(class)? {
            return Ok(None);
        }

        let full_name = format!("{class}.{field_name}");
        let static_field = graph.static_field(full_name, subfield_name);

        Ok(Some(static_field))
    }
}
