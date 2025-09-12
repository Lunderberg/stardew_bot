use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};

use crate::{Error, RecursiveRewrite, SequentialRewrite, SingleRewrite};

pub trait GraphRewrite {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
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

impl<T> GraphRewrite for &T
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
        name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        <T as GraphRewrite>::rewrite_expr(self, graph, expr, name)
    }
}
