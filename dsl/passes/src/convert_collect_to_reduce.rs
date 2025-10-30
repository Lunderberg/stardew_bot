use dsl_rewrite_utils::GraphRewrite;
use itertools::Itertools as _;

use dsl_analysis::{Analysis, TypeInferenceError};
use dsl_ir::{
    DSLType, ExposedNativeFunction, ExprKind, FunctionType, IteratorType,
    NativeFunction, OpIndex, StackValue, SymbolicGraph, SymbolicValue,
};

use crate::Error;

pub struct ConvertCollectToReduce<'a: 'b, 'b>(pub &'b Analysis<'a>);

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
        _args: &mut [&mut StackValue],
    ) -> Result<StackValue, dsl_ir::Error> {
        let obj = self.element_type.new_vector()?;
        Ok(obj.into())
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
        args: &mut [&mut StackValue],
    ) -> Result<StackValue, dsl_ir::Error> {
        assert_eq!(args.len(), 2);

        let (vec, item) = args
            .iter_mut()
            .collect_tuple()
            .expect("Exactly two arguments");

        if vec.is_none() {
            return Ok(StackValue::None);
        }

        self.element_type
            .collect_into_vector(vec, item, &self.output_name)?;

        Ok(StackValue::None)
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

    let num_op = graph.num_operations();
    let mut seen = vec![false; num_op];
    let mut used_without_definition = vec![false; num_op];
    let mut used_as_function = vec![false; num_op];
    let mut currently_defined = vec![false; num_op];

    macro_rules! mark {
        ($op_index:expr) => {
            let op_index: OpIndex = $op_index;
            if !seen[op_index.0] {
                seen[op_index.0] = true;
                to_visit.push(VisitItem::PreVisit(op_index));
            }
        };
    }

    while let Some(visiting) = to_visit.pop() {
        match visiting {
            VisitItem::PreVisit(op_index) => match &graph[op_index].kind {
                ExprKind::FunctionArg(_) => {
                    if !currently_defined[op_index.0] {
                        used_without_definition[op_index.0] = true;
                    }
                }
                ExprKind::Function { params, output } => {
                    params.iter().filter_map(|p| p.as_op_index()).for_each(
                        |param_index| {
                            assert!(!currently_defined[param_index.0]);
                            currently_defined[param_index.0] = true;
                            to_visit
                                .push(VisitItem::RemoveDefinition(param_index));
                        },
                    );
                    if let Some(out_index) = output.as_op_index() {
                        mark!(out_index);
                    }
                }
                ExprKind::FunctionCall { func, args } => {
                    if let Some(func_index) = func.as_op_index() {
                        used_as_function[func_index.0] = true;
                    }
                    args.iter()
                        .chain([func])
                        .filter_map(|value| value.as_op_index())
                        .for_each(|item| {
                            mark!(item);
                        });
                }
                other => {
                    other.iter_input_nodes().for_each(|item| {
                        mark!(item);
                    });
                }
            },
            VisitItem::RemoveDefinition(op_index) => {
                currently_defined[op_index.0] = false;
            }
        }
    }

    used_without_definition
        .into_iter()
        .enumerate()
        .filter(|(_, val)| *val)
        .map(|(i, _)| OpIndex(i))
        .filter(|index| !used_as_function[index.0])
        .map(Into::into)
        .collect()
}

impl GraphRewrite for ConvertCollectToReduce<'_, '_> {
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
        graph.name(make_vector, "make_vector")?;

        let collect_into_vector = graph.raw_native_function(
            ExposedNativeFunction::new(CollectIntoVector {
                element_type: *item_type.clone(),
                output_name: name.unwrap_or("(anon)").to_string(),
            }),
        );
        graph.name(collect_into_vector, "collect_into_vector")?;

        let dummy_initial_args = collect_dummy_args(graph, iterator);

        let initial = graph.function_call(make_vector, dummy_initial_args);

        let collected = graph.reduce(initial, iterator, collect_into_vector);

        Ok(Some(collected))
    }
}
