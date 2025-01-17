use crate::{CachedReader, Error};

use super::{
    GraphRewrite, SymbolicExpr, SymbolicGraph, SymbolicValue, TypeInference,
};

pub struct RemoveUnusedDowncast<'a> {
    reader: CachedReader<'a>,
    type_inference: TypeInference<'a>,
}

impl<'a> RemoveUnusedDowncast<'a> {
    pub fn new(reader: CachedReader<'a>) -> Self {
        Self {
            reader,
            type_inference: TypeInference::new(reader),
        }
    }
}

impl<'a> GraphRewrite for RemoveUnusedDowncast<'a> {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &SymbolicExpr,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            SymbolicExpr::SymbolicDowncast { obj, ty } => {
                let obj_type = self.type_inference.lookup_type(graph, *obj)?;

                let static_method_table_ptr =
                    obj_type.method_table_for_downcast()?;
                let target_method_table_ptr = ty.method_table(self.reader)?;

                if self.reader.is_base_of(
                    target_method_table_ptr,
                    static_method_table_ptr,
                )? {
                    // Static type is the same, or superclass of
                    // the desired runtime type.  This downcast
                    // can be simplified away.
                    Some(*obj)
                } else if self
                    .reader
                    .method_table(static_method_table_ptr)?
                    .is_interface()
                    || self.reader.is_base_of(
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
