use dotnet_debugger::CachedReader;
use env_var_flag::env_var_flag;

use dsl_analysis::Analysis;
use dsl_ir::SymbolicGraph;
use dsl_rewrite_utils::{GraphRewrite, SymbolicGraphRewrite as _};

use crate::Error;

pub enum TargetRuntime {
    VirtualMachine,
    Interpreter,
}

pub struct PassSelector {
    runtime: TargetRuntime,
    optimize: bool,
}

impl PassSelector {
    pub fn new(runtime: TargetRuntime) -> Self {
        Self {
            runtime,
            optimize: true,
        }
    }

    pub fn optimize(self, optimize: bool) -> Self {
        Self { optimize, ..self }
    }

    pub fn runtime(self, runtime: TargetRuntime) -> Self {
        Self { runtime, ..self }
    }

    pub fn passes<'a: 'b, 'b>(
        self,
        analysis: &'b Analysis<'a>,
    ) -> impl GraphRewrite<Error = Error> + 'b {
        let vm = matches!(self.runtime, TargetRuntime::VirtualMachine);
        let optimize = self.optimize;

        super::NoOpRewrite
            .then(super::InferFunctionParameterTypes(analysis))
            .then(super::LegalizeOperandTypes(analysis))
            .then(super::InlineFunctionCalls.enabled(optimize || vm))
            .then(super::UnwrapKnownLazyStatic)
            .then(super::MergeRangeReduceToSimpleReduce)
            .then(super::InlineIteratorMap)
            .then(super::InlineIteratorFilter)
            .then(super::SplitIteratorChainReduce)
            .then(super::ConvertCollectToReduce(analysis))
            .then(super::ConvertFindToFilterFirst)
            .then(super::ConvertFirstToReduce)
            .then(super::ConvertFindMapToFilterFind)
            .then(super::ConvertBooleanOperatorToConditional)
            .then(super::LowerSymbolicExpr(analysis))
            .then(
                super::SeparateReadAndParseBytes
                    .enabled(env_var_flag("SEPARATE_READ_AND_PARSE_BYTES")),
            )
            .then(super::ConstantFold.enabled(optimize))
            .then(super::RemoveUnusedDowncast(analysis).enabled(optimize))
            .then(super::RemoveUnusedPrimcast(analysis).enabled(optimize))
            .then(super::RemoveUnusedPointerCast.enabled(optimize))
            .then(super::MovePointerCastAfterConditional.enabled(optimize))
            // Currently, the MergeParallelReads pass works, but
            // doesn't seem to provide a performance benefit.
            .then(super::MergeParallelReads.enabled(false))
    }
}

pub trait SymbolicGraphSimplify: Sized {
    fn simplify<'a: 'b, 'b>(
        &'a self,
        reader: impl Into<Option<CachedReader<'b>>>,
    ) -> Result<Self, Error>;
}
impl SymbolicGraphSimplify for SymbolicGraph {
    fn simplify<'a: 'b, 'b>(
        &'a self,
        reader: impl Into<Option<CachedReader<'b>>>,
    ) -> Result<Self, Error> {
        let analysis = Analysis::new(reader);
        let rewriter = super::RemoveUnusedDowncast(&analysis)
            .then(super::ConstantFold)
            .then(super::RemoveUnusedPrimcast(&analysis))
            .apply_recursively();
        self.rewrite(rewriter)
    }
}
