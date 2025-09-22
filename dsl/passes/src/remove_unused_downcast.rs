use dsl_analysis::{Analysis, DSLTypeExt as _};
use dsl_ir::{DSLType, ExprKind, SymbolicGraph, SymbolicValue};
use dsl_rewrite_utils::GraphRewrite;

use crate::Error;

pub struct RemoveUnusedDowncast<'a: 'b, 'b>(pub &'b Analysis<'a>);

impl<'a: 'b, 'b> GraphRewrite for RemoveUnusedDowncast<'a, 'b> {
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        let ExprKind::SymbolicDowncast { obj, ty } = expr else {
            return Ok(None);
        };

        let obj_type = self.0.infer_type(graph, *obj)?;

        if matches!(obj_type, DSLType::Unknown) {
            return Ok(None);
        }

        let reader = self.0.reader()?;

        let static_method_table_ptr = obj_type.method_table_for_downcast()
            .unwrap()
            //?
            ;
        let Some(target_method_table_ptr) =
            reader.symbolic_type_to_method_table(ty)?
        else {
            return Ok(None);
        };

        if reader
            .is_base_of(target_method_table_ptr, static_method_table_ptr)?
        {
            // Static type is the same, or superclass of
            // the desired runtime type.  This downcast
            // can be simplified away.
            Ok(Some(*obj))
        } else if reader.method_table(static_method_table_ptr)?.is_interface()
            || reader
                .is_base_of(static_method_table_ptr, target_method_table_ptr)?
        {
            // Target type is a subclass of the
            // statically-known type.  The downcast must
            // be retained.
            Ok(None)
        } else {
            // Types are in separate hierachies.  This
            // downcast is illegal.
            Err(Error::DowncastRequiresRelatedClasses(
                format!("{obj_type}"),
                format!("{ty}"),
            ))
        }
    }
}
