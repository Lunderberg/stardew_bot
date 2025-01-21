use crate::{CachedReader, Error, RuntimeType};

use super::{
    ExprKind, RecursiveRewrite, SequentialRewrite, SymbolicGraph,
    SymbolicValue, TypeInference,
};

pub trait GraphRewrite {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SymbolicValue>, Error>;

    fn apply_recursively(self) -> impl GraphRewrite
    where
        Self: Sized,
    {
        RecursiveRewrite::new(self)
    }

    fn then(self, second: impl GraphRewrite) -> impl GraphRewrite
    where
        Self: Sized,
    {
        SequentialRewrite::new(self, second)
    }
}

/// Structure to hold analyzers used in common by several different
/// rewriters.
///
/// While the rewriters could each have their own copy of the analysis
/// routines, the same results may be cached and used by each
/// rewriter.  Having a common structure allows the analysis routines
/// to be extended with additional checks/validation in the future.
///
/// Which is probably a bit of overengineering given that type
/// inference is currently the only analysis performed.
pub struct Analysis<'a> {
    reader: CachedReader<'a>,
    type_inference: TypeInference<'a>,
}

impl<'a> Analysis<'a> {
    pub fn new(reader: CachedReader<'a>) -> Self {
        let type_inference = TypeInference::new(reader);
        Self {
            reader,
            type_inference,
        }
    }

    pub fn infer_type(
        &'a self,
        graph: &SymbolicGraph,
        value: SymbolicValue,
    ) -> Result<&'a RuntimeType, Error> {
        self.type_inference.infer_type(graph, value)
    }

    pub fn reader(&'a self) -> CachedReader<'a> {
        self.reader
    }
}
