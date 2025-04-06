use crate::{CachedReader, Error, RuntimeType};

use super::{
    ExprKind, RecursiveRewrite, SequentialRewrite, SingleRewrite,
    SymbolicGraph, SymbolicValue, TypeInference,
};

pub trait GraphRewrite {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SymbolicValue>, Error>;

    fn init(&self) {}

    fn apply_once(self) -> impl GraphRewrite
    where
        Self: Sized,
    {
        SingleRewrite::new(self)
    }

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

impl<'a, T> GraphRewrite for &'a T
where
    T: GraphRewrite,
{
    fn init(&self) {
        <T as GraphRewrite>::init(self)
    }

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SymbolicValue>, Error> {
        <T as GraphRewrite>::rewrite_expr(self, graph, expr)
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
    opt_reader: Option<CachedReader<'a>>,
    type_inference: TypeInference<'a>,
}

impl<'a> Analysis<'a> {
    pub fn new(reader: impl Into<Option<CachedReader<'a>>>) -> Self {
        let opt_reader = reader.into();
        let type_inference = TypeInference::new(opt_reader);
        Self {
            opt_reader,
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

    pub fn reader(&'a self) -> Result<CachedReader<'a>, Error> {
        Ok(self.opt_reader.unwrap())
    }
}
