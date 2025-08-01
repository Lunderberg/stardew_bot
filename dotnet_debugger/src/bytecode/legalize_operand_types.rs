use crate::{Error, RuntimePrimType, RuntimeType};

use super::{
    graph_rewrite::Analysis, ExprKind, GraphRewrite, SymbolicGraph,
    SymbolicValue,
};

pub struct LegalizeOperandTypes<'a>(pub &'a Analysis<'a>);

impl LegalizeOperandTypes<'_> {
    fn get_type(
        &self,
        graph: &SymbolicGraph,
        value: SymbolicValue,
    ) -> Result<Option<RuntimeType>, Error> {
        let value_type = self.0.infer_type(graph, value)?;
        Ok(match value_type {
            RuntimeType::Unknown => None,
            other => Some(other.clone()),
        })
    }

    fn require_prim_type(
        &self,
        graph: &mut SymbolicGraph,
        value: SymbolicValue,
        required: RuntimePrimType,
    ) -> Result<Option<SymbolicValue>, Error> {
        let Some(current_type) = self.get_type(graph, value)? else {
            return Ok(None);
        };

        Ok(match current_type {
            RuntimeType::Prim(current_prim_type)
                if current_prim_type == required =>
            {
                None
            }
            RuntimeType::Prim(_) => Some(graph.prim_cast(value, required)),
            RuntimeType::Unknown => None,

            other => {
                return Err(Error::TypeNotConvertibleToIndex(other));
            }
        })
    }

    fn require_prim_array(
        &self,
        graph: &mut SymbolicGraph,
        values: &[SymbolicValue],
        required: RuntimePrimType,
    ) -> Result<Option<Vec<SymbolicValue>>, Error> {
        let mut is_changed = false;
        let mut new_values = Vec::new();

        for value in values.iter().cloned() {
            let opt_new_value =
                self.require_prim_type(graph, value, required)?;
            is_changed = is_changed || opt_new_value.is_some();
            new_values.push(opt_new_value.unwrap_or(value));
        }

        Ok(if is_changed { Some(new_values) } else { None })
    }
}

impl GraphRewrite for LegalizeOperandTypes<'_> {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            ExprKind::IndexAccess { obj, indices } => self
                .require_prim_array(
                    graph,
                    indices,
                    RuntimePrimType::NativeUInt,
                )?
                .map(|indices| {
                    graph.push(ExprKind::IndexAccess { obj: *obj, indices })
                }),

            _ => None,
        })
    }
}
