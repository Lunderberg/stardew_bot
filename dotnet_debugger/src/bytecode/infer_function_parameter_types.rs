use std::collections::HashMap;

use crate::{runtime_type::IteratorType, Error, RuntimePrimType, RuntimeType};

use super::{
    graph_rewrite::Analysis, ExprKind, GraphRewrite, OpIndex, SymbolicGraph,
    SymbolicValue,
};

pub struct InferFunctionParameterTypes<'a>(pub &'a Analysis<'a>);

impl InferFunctionParameterTypes<'_> {
    fn get_type(
        &self,
        graph: &SymbolicGraph,
        value: SymbolicValue,
    ) -> Result<Option<RuntimeType>, Error> {
        let value_type = self.0.infer_type(graph, value)?;
        Ok(match value_type {
            RuntimeType::Unknown => None,
            other => Some(other.clone()),
        })
    }

    fn get_iterator_element_type(
        &self,
        graph: &SymbolicGraph,
        iterator: SymbolicValue,
    ) -> Result<Option<RuntimeType>, Error> {
        let iter_type = self.0.infer_type(graph, iterator)?;
        Ok(match iter_type {
            RuntimeType::Iterator(IteratorType { item }) => match item.as_ref()
            {
                RuntimeType::Unknown => None,
                other => Some(other.clone()),
            },
            _ => None,
        })
    }

    fn get_new_param(
        graph: &mut SymbolicGraph,
        func: SymbolicValue,
        param_index: usize,
        gen_type: impl FnOnce(&SymbolicGraph) -> Result<Option<RuntimeType>, Error>,
    ) -> Result<Option<(OpIndex, SymbolicValue)>, Error> {
        let SymbolicValue::Result(func) = func else {
            return Ok(None);
        };

        let param = match &graph[func].kind {
            ExprKind::Function { params, .. } => {
                if params.len() < param_index {
                    todo!("Define TypeError for incorrect arity")
                }
                params[param_index]
            }
            _ => {
                return Ok(None);
            }
        };

        let param = param
            .as_op_index()
            .expect("Function parameter should be FunctionArg");

        if !matches!(
            graph[param].kind,
            ExprKind::FunctionArg(RuntimeType::Unknown)
        ) {
            return Ok(None);
        }

        let Some(param_type) = gen_type(graph)? else {
            return Ok(None);
        };

        let new_param = graph.function_arg(param_type.clone());
        if let Some(name) = graph[param].name.clone() {
            graph
                .name(new_param, name)
                .expect("Existing name must already be valid");
        }

        Ok(Some((param, new_param)))
    }
}

impl GraphRewrite for InferFunctionParameterTypes<'_> {
    fn rewrite_expr(
        &self,
        graph: &mut SymbolicGraph,
        expr: &ExprKind,
    ) -> Result<Option<SymbolicValue>, Error> {
        Ok(match expr {
            &ExprKind::Map { iterator, map } => {
                let replacements: HashMap<_, _> =
                    [Self::get_new_param(graph, map, 0, |graph| {
                        self.get_iterator_element_type(graph, iterator)
                    })]
                    .into_iter()
                    .filter_map(|opt_pair| opt_pair.transpose())
                    .collect::<Result<_, _>>()?;

                graph
                    .substitute(replacements, map)?
                    .map(|new_map| graph.map(iterator, new_map))
            }

            &ExprKind::Filter { iterator, filter } => {
                let replacements: HashMap<_, _> =
                    [Self::get_new_param(graph, filter, 0, |graph| {
                        self.get_iterator_element_type(graph, iterator)
                    })]
                    .into_iter()
                    .filter_map(|opt_pair| opt_pair.transpose())
                    .collect::<Result<_, _>>()?;

                graph
                    .substitute(replacements, filter)?
                    .map(|new_filter| graph.filter(iterator, new_filter))
            }

            &ExprKind::Reduce {
                initial,
                iterator,
                reduction,
            } => {
                let replacements: HashMap<_, _> = [
                    Self::get_new_param(graph, reduction, 0, |graph| {
                        self.get_type(graph, initial)
                    }),
                    Self::get_new_param(graph, reduction, 1, |graph| {
                        self.get_iterator_element_type(graph, iterator)
                    }),
                ]
                .into_iter()
                .filter_map(|opt_pair| opt_pair.transpose())
                .collect::<Result<_, _>>()?;

                graph.substitute(replacements, reduction)?.map(
                    |new_reduction| {
                        graph.reduce(initial, iterator, new_reduction)
                    },
                )
            }

            &ExprKind::SimpleReduce {
                initial,
                extent,
                reduction,
            } => {
                let replacements: HashMap<_, _> = [
                    Self::get_new_param(graph, reduction, 0, |graph| {
                        self.get_type(graph, initial)
                    }),
                    Self::get_new_param(graph, reduction, 1, |_| {
                        Ok(Some(RuntimeType::Prim(RuntimePrimType::NativeUInt)))
                    }),
                ]
                .into_iter()
                .filter_map(|opt_pair| opt_pair.transpose())
                .collect::<Result<_, _>>()?;

                graph.substitute(replacements, reduction)?.map(
                    |new_reduction| {
                        graph.simple_reduce(initial, extent, new_reduction)
                    },
                )
            }

            _ => None,
        })
    }
}
