use super::{ExprKind, GraphRewrite, SymbolicValue};

pub struct InlineIteratorMap;

fn copy_first_param(
    graph: &mut super::SymbolicGraph,
    func: SymbolicValue,
) -> SymbolicValue {
    let SymbolicValue::Result(func) = func else {
        panic!("")
    };
    let ExprKind::Function { params, .. } = &graph[func].kind else {
        panic!("")
    };

    let SymbolicValue::Result(first_param_index) = params[0] else {
        panic!("")
    };

    let first_param = &graph[first_param_index];

    let ExprKind::FunctionArg(param_ty) = &first_param.kind else {
        panic!("")
    };

    let opt_name = first_param.name.clone();
    let new_param = graph.function_arg(param_ty.clone());
    if let Some(name) = opt_name {
        graph
            .name(new_param, name)
            .expect("Existing name must already be valid");
    }

    new_param
}

impl GraphRewrite for InlineIteratorMap {
    fn rewrite_expr(
        &self,
        graph: &mut super::SymbolicGraph,
        expr: &super::ExprKind,
    ) -> Result<Option<SymbolicValue>, crate::Error> {
        Ok(match expr {
            &ExprKind::Reduce {
                initial,
                iterator: SymbolicValue::Result(iterator),
                reduction,
            } => match graph[iterator].as_ref() {
                &ExprKind::Map { iterator, map } => {
                    let new_reduce_lhs = copy_first_param(graph, reduction);
                    let new_reduce_rhs = copy_first_param(graph, map);

                    let mapped = graph.function_call(map, vec![new_reduce_rhs]);
                    let reduced = graph
                        .function_call(reduction, vec![new_reduce_lhs, mapped]);

                    let new_reduction = graph.function_def(
                        vec![new_reduce_lhs, new_reduce_rhs],
                        reduced,
                    );

                    let new_reduced_iter =
                        graph.reduce(initial, iterator, new_reduction);

                    Some(new_reduced_iter)
                }
                _ => None,
            },
            _ => None,
        })
    }
}
