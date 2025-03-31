use itertools::Itertools as _;

use crate::{
    bytecode::ExposedNativeFunction,
    runtime_type::{FunctionType, IteratorType},
    Error, RuntimeType, TypeInferenceError,
};

use super::{
    graph_rewrite::Analysis, ExprKind, GraphRewrite, NativeFunction,
    SymbolicGraph, SymbolicValue,
};

pub struct ConvertCollectToReduce<'a>(pub &'a Analysis<'a>);

struct MakeVector {
    element_type: RuntimeType,
}

struct CollectIntoVector {
    element_type: RuntimeType,
}

impl NativeFunction for MakeVector {
    fn apply(
        &self,
        args: &mut [&mut Option<super::StackValue>],
    ) -> Result<Option<super::StackValue>, Error> {
        assert!(args.is_empty());
        let obj = self.element_type.new_vector()?;
        Ok(Some(obj.into()))
    }

    fn signature(&self) -> Result<RuntimeType, Error> {
        let vector_type = self.element_type.vector_type()?;
        Ok(FunctionType {
            params: Some(vec![]),
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
    ) -> Result<Option<super::StackValue>, Error> {
        assert_eq!(args.len(), 2);

        let (vec, item) = args
            .iter_mut()
            .collect_tuple()
            .expect("Exactly two arguments");

        let Some(vec) = vec.as_mut() else {
            return Ok(None);
        };

        self.element_type.collect_into_vector(vec, item)?;

        Ok(None)
    }

    fn signature(&self) -> Result<RuntimeType, Error> {
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

impl<'a> GraphRewrite for ConvertCollectToReduce<'a> {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            &ExprKind::Collect { iterator } => {
                let iterator_type = self.0.infer_type(graph, iterator)?;
                let item_type = match iterator_type {
                    RuntimeType::Iterator(IteratorType { item }) => Ok(item),
                    other => Err(TypeInferenceError::CollectRequiresIterator(
                        other.clone(),
                    )),
                }?;

                let make_vector = graph.raw_native_function(
                    ExposedNativeFunction::new(MakeVector {
                        element_type: *item_type.clone(),
                    }),
                );
                let collect_into_vector = graph.raw_native_function(
                    ExposedNativeFunction::new(CollectIntoVector {
                        element_type: *item_type.clone(),
                    }),
                );

                let initial = graph.function_call(make_vector, vec![]);

                let collected =
                    graph.reduce(initial, iterator, collect_into_vector);

                Some(collected)
            }
            _ => None,
        })
    }
}
