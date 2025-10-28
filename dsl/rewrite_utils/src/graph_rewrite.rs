use dsl_ir::{ExprKind, SymbolicGraph, SymbolicValue};

use crate::{
    ConditionalRewrite, MapErr, RecursiveRewrite, SequentialRewrite,
    SingleRewrite,
};

pub trait GraphRewrite {
    type Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        _name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Self::Error>;

    fn map_err<OutError, Func>(self, func: Func) -> MapErr<Self, Func>
    where
        Self: Sized,
        Func: Fn(Self::Error) -> OutError,
    {
        MapErr::new(self, func)
    }

    fn init(&self) {}

    fn apply_once(self) -> SingleRewrite<Self>
    where
        Self: Sized,
    {
        SingleRewrite::new(self)
    }

    fn apply_recursively(self) -> RecursiveRewrite<Self>
    where
        Self: Sized,
        Self::Error: From<dsl_ir::Error>,
    {
        RecursiveRewrite::new(self)
    }

    fn enabled(self, enabled: bool) -> ConditionalRewrite<Self>
    where
        Self: Sized,
    {
        ConditionalRewrite::new(self, enabled)
    }

    fn then<Second>(self, second: Second) -> SequentialRewrite<Self, Second>
    where
        Self: Sized,
        Second: GraphRewrite<Error = Self::Error>,
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
