use crate::GraphRewrite;

pub struct MapErr<Inner, Func> {
    inner: Inner,
    func: Func,
}

impl<Inner, Func> MapErr<Inner, Func> {
    pub fn new(inner: Inner, func: Func) -> Self {
        Self { inner, func }
    }
}

impl<Inner, Func, InError, OutError> GraphRewrite for MapErr<Inner, Func>
where
    Inner: GraphRewrite<Error = InError>,
    Func: Fn(InError) -> OutError,
{
    type Error = OutError;

    fn rewrite_expr(
        &self,
        graph: &mut dsl_ir::SymbolicGraph,
        expr: &dsl_ir::ExprKind,
        name: Option<&str>,
    ) -> Result<Option<dsl_ir::SymbolicValue>, Self::Error> {
        <Inner as GraphRewrite>::rewrite_expr(&self.inner, graph, expr, name)
            .map_err(&self.func)
    }
}
