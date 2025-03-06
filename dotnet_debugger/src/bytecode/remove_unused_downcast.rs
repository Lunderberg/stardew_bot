use crate::Error;

use super::{
    graph_rewrite::Analysis, ExprKind, GraphRewrite, SymbolicGraph,
    SymbolicValue,
};

pub struct RemoveUnusedDowncast<'a>(pub &'a Analysis<'a>);

impl<'a> GraphRewrite for RemoveUnusedDowncast<'a> {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            ExprKind::SymbolicDowncast { obj, ty } => {
                let obj_type = self.0.infer_type(graph, *obj)?;

                let reader = self.0.reader()?;

                let static_method_table_ptr =
                    obj_type.method_table_for_downcast()?;
                let target_method_table_ptr = ty.method_table(reader)?;

                if reader.is_base_of(
                    target_method_table_ptr,
                    static_method_table_ptr,
                )? {
                    // Static type is the same, or superclass of
                    // the desired runtime type.  This downcast
                    // can be simplified away.
                    Some(*obj)
                } else if reader
                    .method_table(static_method_table_ptr)?
                    .is_interface()
                    || reader.is_base_of(
                        static_method_table_ptr,
                        target_method_table_ptr,
                    )?
                {
                    // Target type is a subclass of the
                    // statically-known type.  The downcast must
                    // be retained.
                    None
                } else {
                    // Types are in separate hierachies.  This
                    // downcast is illegal.
                    return Err(Error::DowncastRequiresRelatedClasses(
                        format!("{obj_type}"),
                        format!("{ty}"),
                    ));
                }
            }

            _ => None,
        })
    }
}
