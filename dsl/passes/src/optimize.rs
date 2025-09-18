use dotnet_debugger::CachedReader;
use dsl_analysis::Analysis;
use dsl_ir::SymbolicGraph;
use dsl_rewrite_utils::{GraphRewrite, SymbolicGraphRewrite as _};

use crate::Error;

pub fn optimization_passes(
    analysis: &Analysis,
) -> impl GraphRewrite<Error = Error> {
    super::ConstantFold
        .then(super::RemoveUnusedDowncast(&analysis))
        .then(super::RemoveUnusedPrimcast(&analysis))
        .then(super::RemoveUnusedPointerCast)
    // Currently, the MergeParallelReads pass works, but
    // doesn't seem to provide a performance benefit.
    //
    // .then(super::MergeParallelReads)
}

pub trait SymbolicGraphSimplify: Sized {
    fn simplify<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<Self, Error>;
}
impl SymbolicGraphSimplify for SymbolicGraph {
    fn simplify<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<Self, Error> {
        let analysis = Analysis::new(reader);
        let rewriter = super::RemoveUnusedDowncast(&analysis)
            .then(super::ConstantFold)
            .then(super::RemoveUnusedPrimcast(&analysis))
            .apply_recursively();
        self.rewrite(rewriter)
    }
}
