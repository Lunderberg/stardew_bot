use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};

use crate::{MapErr, RecursiveRewrite, SequentialRewrite, SingleRewrite};

pub trait GraphRewrite {
    type Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Self::Error>;

    fn map_err<OutError, Func>(
        self,
        func: Func,
    ) -> impl GraphRewrite<Error = OutError>
    where
        Self: Sized,
        Func: Fn(Self::Error) -> OutError,
    {
        MapErr::new(self, func)
    }

    fn init(&self) {}

    fn apply_once(self) -> impl GraphRewrite<Error = Self::Error>
    where
        Self: Sized,
    {
        SingleRewrite::new(self)
    }

    fn apply_recursively(self) -> impl GraphRewrite<Error = Self::Error>
    where
        Self: Sized,
    {
        RecursiveRewrite::new(self)
    }

    fn then(
        self,
        second: impl GraphRewrite<Error = Self::Error>,
    ) -> impl GraphRewrite<Error = Self::Error>
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
    type Error = <T as GraphRewrite>::Error;

    fn init(&self) {
        <T as GraphRewrite>::init(self)
    }

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Self::Error> {
        <T as GraphRewrite>::rewrite_expr(self, graph, expr, name)
    }
}
