use std::collections::HashSet;

use itertools::Itertools as _;

use dsl_analysis::{Analysis, TypeInferenceError};
use dsl_ir::{
    DSLType, ExposedNativeFunction, ExprKind, FunctionType, IteratorType,
    NativeFunction, OpIndex, StackValue, SymbolicGraph, SymbolicValue,
};

use crate::{Error, GraphRewrite};

pub struct ConvertCollectToReduce<'a>(pub &'a Analysis<'a>);

struct MakeVector {
    element_type: DSLType,
}

struct CollectIntoVector {
    element_type: DSLType,
    output_name: String,
}

impl NativeFunction for MakeVector {
    fn apply(
        &self,
        _args: &mut [&mut Option<StackValue>],
    ) -> Result<Option<StackValue>, dsl_ir::Error> {
        let obj = self.element_type.new_vector()?;
        Ok(Some(obj.into()))
    }

    fn signature(&self) -> Result<DSLType, dsl_ir::Error> {
        let vector_type = self.element_type.vector_type()?;
        Ok(FunctionType {
            params: None,
            output: Box::new(vector_type),
        }
        .into())
    }

    fn mutates_first_argument(&self) -> bool {
        false
    }
}

impl NativeFunction for CollectIntoVector {
    fn apply(
        &self,
        args: &mut [&mut Option<super::StackValue>],
    ) -> Result<Option<StackValue>, dsl_ir::Error> {
        assert_eq!(args.len(), 2);

        let (vec, item) = args
            .iter_mut()
            .collect_tuple()
            .expect("Exactly two arguments");

        let Some(vec) = vec.as_mut() else {
            return Ok(None);
        };

        self.element_type
            .collect_into_vector(vec, item, &self.output_name)?;

        Ok(None)
    }

    fn signature(&self) -> Result<DSLType, dsl_ir::Error> {
        let vector_type = self.element_type.vector_type()?;
        Ok(FunctionType {
            params: Some(vec![vector_type.clone(), self.element_type.clone()]),
            output: Box::new(vector_type),
        }
        .into())
    }

    fn mutates_first_argument(&self) -> bool {
        true
    }
}

fn collect_dummy_args(
    graph: &SymbolicGraph,
    initial: SymbolicValue,
) -> Vec<SymbolicValue> {
    let SymbolicValue::Result(initial) = initial else {
        return Vec::new();
    };

    enum VisitItem {
        PreVisit(OpIndex),
        RemoveDefinition(OpIndex),
    }

    let mut to_visit = vec![VisitItem::PreVisit(initial)];

    let mut used_without_definition = HashSet::<OpIndex>::new();
    let mut used_as_function = HashSet::<OpIndex>::new();
    let mut currently_defined = HashSet::<OpIndex>::new();

    while let Some(visiting) = to_visit.pop() {
        match visiting {
            VisitItem::PreVisit(op_index) => match &graph[op_index].kind {
                ExprKind::FunctionArg(_) => {
                    if !currently_defined.contains(&op_index) {
                        used_without_definition.insert(op_index);
                    }
                }
                ExprKind::Function { params, output } => {
                    params.iter().filter_map(|p| p.as_op_index()).for_each(
                        |param_index| {
                            assert!(!currently_defined.contains(&param_index));
                            currently_defined.insert(param_index);
                            to_visit
                                .push(VisitItem::RemoveDefinition(param_index));
                        },
                    );
                    if let Some(out_index) = output.as_op_index() {
                        to_visit.push(VisitItem::PreVisit(out_index));
                    }
                }
                ExprKind::FunctionCall { func, args } => {
                    if let Some(func_index) = func.as_op_index() {
                        used_as_function.insert(func_index);
                    }
                    args.iter()
                        .chain([func])
                        .filter_map(|value| value.as_op_index())
                        .map(VisitItem::PreVisit)
                        .for_each(|item| {
                            to_visit.push(item);
                        });
                }
                other => {
                    other.iter_input_nodes().map(VisitItem::PreVisit).for_each(
                        |item| {
                            to_visit.push(item);
                        },
                    );
                }
            },
            VisitItem::RemoveDefinition(op_index) => {
                currently_defined.remove(&op_index);
            }
        }
    }

    used_without_definition
        .into_iter()
        .sorted()
        .filter(|index| !used_as_function.contains(index))
        .map(Into::into)
        .collect()
}

impl<'a> GraphRewrite for ConvertCollectToReduce<'a> {
    type Error = Error;

    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
        name: Option<&str>,
    ) -> Result<Option<SymbolicValue>, Error> {
        let &ExprKind::Collect { iterator } = expr else {
            return Ok(None);
        };

        let iterator_type = self.0.infer_type(graph, iterator)?;
        let item_type = match iterator_type {
            DSLType::Iterator(IteratorType { item }) => Ok(item),
            other => {
                Err(TypeInferenceError::CollectRequiresIterator(other.clone()))
            }
        }?;

        let make_vector =
            graph.raw_native_function(ExposedNativeFunction::new(MakeVector {
                element_type: *item_type.clone(),
            }));
        let collect_into_vector = graph.raw_native_function(
            ExposedNativeFunction::new(CollectIntoVector {
                element_type: *item_type.clone(),
                output_name: name.unwrap_or("(anon)").to_string(),
            }),
        );

        let dummy_initial_args = collect_dummy_args(graph, iterator);

        let initial = graph.function_call(make_vector, dummy_initial_args);

        let collected = graph.reduce(initial, iterator, collect_into_vector);

        Ok(Some(collected))
    }
}
