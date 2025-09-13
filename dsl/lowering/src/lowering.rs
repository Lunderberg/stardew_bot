use dsl_analysis::Analysis;
use dsl_rewrite_utils::GraphRewrite;

use crate::Error;

pub fn lowering_passes(
    analysis: &Analysis,
) -> impl GraphRewrite<Error = Error> {
    super::InferFunctionParameterTypes(&analysis)
        .then(super::LegalizeOperandTypes(&analysis))
        .then(super::InlineFunctionCalls)
        .then(super::MergeRangeReduceToSimpleReduce)
        .then(super::InlineIteratorMap)
        .then(super::InlineIteratorFilter)
        .then(super::SplitIteratorChainReduce)
        .then(super::ConvertCollectToReduce(&analysis))
        .then(super::ConvertFindToFilterFirst)
        .then(super::ConvertFirstToReduce)
        .then(super::ConvertFindMapToFilterFind)
        .then(super::ConvertBooleanOperatorToConditional)
        .then(super::LowerSymbolicExpr(&analysis))
        .then(super::SeparateReadAndParseBytes)
}
