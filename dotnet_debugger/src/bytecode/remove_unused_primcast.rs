use crate::{CachedReader, Error};

use super::{
    GraphRewrite, SymbolicExpr, SymbolicGraph, SymbolicValue, TypeInference,
};

pub struct RemoveUnusedPrimcast<'a> {
    type_inference: TypeInference<'a>,
}

impl<'a> RemoveUnusedPrimcast<'a> {
    pub fn new(reader: CachedReader<'a>) -> Self {
        Self {
            type_inference: TypeInference::new(reader),
        }
    }
}

impl<'a> GraphRewrite for RemoveUnusedPrimcast<'a> {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &SymbolicExpr,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            SymbolicExpr::PrimCast { value, prim_type } => {
                let value_type =
                    self.type_inference.lookup_type(graph, *value)?;
                (value_type == prim_type).then(|| *value)
            }

            _ => None,
        })
    }
}
