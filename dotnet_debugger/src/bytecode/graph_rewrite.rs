use crate::Error;

use super::{
    RecursiveRewrite, SequentialRewrite, SymbolicExpr, SymbolicGraph,
    SymbolicValue,
};

pub trait GraphRewrite {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &SymbolicExpr,
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
