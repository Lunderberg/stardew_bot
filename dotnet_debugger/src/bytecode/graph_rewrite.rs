use crate::Error;

use super::{SymbolicExpr, SymbolicGraph, SymbolicValue};

pub trait GraphRewrite {
    fn rewrite(
        &self,
        graph: &mut SymbolicGraph,
        expr: &SymbolicExpr,
    ) -> Result<Option<SymbolicValue>, Error>;
}
