use itertools::Itertools;
use std::collections::HashSet;

use crate::{ExprKind, OpIndex, SymbolicGraph, SymbolicValue};

/// Indicates which scope contains an operation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Scope {
    /// The operation is contained within the global scope.
    Global,

    /// The operation is contained within the body of a function.  A
    /// function's body is identified as the set of all expressions
    /// that depend on at least one of the function's parameters, and
    /// are depended on by the function's output.
    Function(OpIndex),

    /// The operation is contained within the `if` branch of a
    /// conditional.  A conditional branch's body is identified as the
    /// set of all expressions that contribute to the branch's value,
    /// and only contribute to the parent scope's value through the
    /// branch.
    IfBranch(OpIndex),

    /// The operation is contained within the `else` branch of a
    /// conditional.  See also, `Scope::IfBranch`.
    ElseBranch(OpIndex),
}

impl SymbolicGraph {
    /// Collect the subgraph of all nodes between the specified
    ///
    /// Returns the set of all nodes such that there exists a path
    /// from at least one of the inputs to the node, and from the node
    /// to at least one of the outputs.  The set is inclusive, and
    /// will include all input nodes that can reach an output node,
    /// and all output nodes that can be reached from an input node.
    ///
    /// This is used when inspecting a function.  For example,
    /// identifying the set of nodes that should be printed within the
    /// body of a function.  Alternatively, the set of nodes which
    /// need to be rewritten when inlining a function.
    pub fn collect_subgraph(
        &self,
        inputs: impl IntoIterator<Item = SymbolicValue>,
        outputs: impl IntoIterator<Item = SymbolicValue>,
    ) -> Vec<OpIndex> {
        let inputs: Vec<OpIndex> = inputs
            .into_iter()
            .filter_map(|input| input.as_op_index())
            .collect();

        if inputs.is_empty() {
            // Early return in the case that there are no inputs.
            // This can occur for nullary functions.
            return vec![];
        };

        let earliest_input = inputs
            .iter()
            .map(|index| index.0)
            .min()
            .expect("Only empty sequences can return None");

        // Step 1, walk backwards from the outputs to the inputs.  The
        // filter on `earliest_input` reduces the number of nodes that
        // must be inspected.  Because nodes may only ever reference
        // nodes that appear earlier in the operation list, a node
        // that appears prior to the earliest input may not depend on
        // any inputs.
        let used_by_outputs: Vec<OpIndex> = {
            let mut to_visit = Vec::<OpIndex>::new();
            let mut used_by_outputs = HashSet::<OpIndex>::new();

            macro_rules! mark {
                ($value:expr) => {
                    $value
                        .as_op_index()
                        .filter(|index| index.0 >= earliest_input)
                        .filter(|index| !used_by_outputs.contains(index))
                        .into_iter()
                        .for_each(|index| {
                            to_visit.push(index);
                            used_by_outputs.insert(index);
                        })
                };
            }

            for output in outputs {
                mark!(output);
            }

            while let Some(visiting) = to_visit.pop() {
                self[visiting]
                    .iter_input_values()
                    .for_each(|value| mark!(value));
            }

            used_by_outputs
                .into_iter()
                .sorted_by_key(|index| index.0)
                .collect()
        };

        // Step 2, walk forward from the inputs to the outputs.  This
        // only considers nodes that were found earlier when walking
        // from the outputs.
        let mut depend_on_inputs: HashSet<OpIndex> =
            inputs.into_iter().collect();
        let mut subgraph: Vec<OpIndex> =
            depend_on_inputs.iter().cloned().collect();

        for index in used_by_outputs {
            let uses_input = self[index]
                .iter_input_nodes()
                .any(|upstream| depend_on_inputs.contains(&upstream));
            if uses_input {
                subgraph.push(index);
                depend_on_inputs.insert(index);
            }
        }

        subgraph
    }

    /// For each expression, determine which function owns it.
    ///
    /// `Scope::Function(func_index)`: The expression uses at least
    /// one parameter from the function declared at `func_index`, and
    /// the expression is used by the function's output.
    ///
    /// `Scope::IfBranch(if_else_index)`: The expression is used by
    /// the if branch of the `ExprKind::IfElse` declared at
    /// `if_else_index`, and is not used by any other
    pub fn operation_scope(&self, reachable: &[bool]) -> Vec<Scope> {
        assert_eq!(reachable.len(), self.num_operations());

        let mut outermost_legal_scope =
            vec![Scope::Global; self.num_operations()];

        // Step 1: Visit each function in reverse order of
        // declaration.  For each function, mark all expressions that
        // are part of the subgraph defined by the function parameters
        // and output.  This handles nested scopes, since inner-scoped
        // functions will be visited after their containing scope.
        //
        // This step produces a valid scope assignment, preferentially
        // placing expressions in the outermost scope that may legally
        // be applied.
        self.iter_ops()
            .rev()
            .filter(|(OpIndex(i), _)| reachable[*i])
            .for_each(|(func_index, op)| {
                if let ExprKind::Function { params, output } = &op.kind {
                    self.collect_subgraph(
                        params.iter().cloned(),
                        Some(*output),
                    )
                    .into_iter()
                    .for_each(|index| {
                        outermost_legal_scope[index.0] =
                            Scope::Function(func_index);
                    });
                }
            });

        // Step 2: Visit each expression in reverse order of
        // declaration.  For each expression, claim the inputs as
        // being in the same scope as the expression.  If the inputs
        // are already claimed by another scope, resolve the two
        // claims with the innermost scope that contains both claims.
        //
        // This step produces a valid scope assignment, preferentially
        // placing expressions in the innermost scope that may legally
        // be applied.
        let mut innermost_legal_scope: Vec<Option<Scope>> =
            vec![None; self.num_operations()];
        self.iter_extern_funcs().for_each(|OpIndex(i)| {
            innermost_legal_scope[i] = Some(Scope::Global);
        });

        for current_index in
            (0..self.num_operations()).rev().filter(|i| reachable[*i])
        {
            let current_op = OpIndex(current_index);
            let opt_current_scope = innermost_legal_scope[current_index];

            let mut mark_scope =
                |value: SymbolicValue, new_scope: Option<Scope>| {
                    let Some(new_scope) = new_scope else {
                        return;
                    };
                    let Some(upstream_op) = value.as_op_index() else {
                        return;
                    };
                    let OpIndex(upstream_index) = upstream_op;

                    if matches!(
                        self[upstream_op].kind,
                        ExprKind::FunctionArg(_)
                    ) && !matches!(
                        self[current_op].kind,
                        ExprKind::Function { .. }
                    ) {
                        return;
                    }

                    innermost_legal_scope[upstream_index] =
                        match innermost_legal_scope[upstream_index] {
                            None => Some(new_scope),
                            Some(prev_scope) if prev_scope == new_scope => {
                                Some(prev_scope)
                            }
                            Some(prev_scope) => {
                                let iter_parent_scopes = |scope: Scope| {
                                    std::iter::successors(
                                        Some(scope),
                                        |scope| {
                                            scope.op_index().and_then(
                                                |OpIndex(j)| {
                                                    innermost_legal_scope[j]
                                                },
                                            )
                                        },
                                    )
                                    .collect::<Vec<_>>()
                                    .into_iter()
                                    .rev()
                                };

                                let joint_scope = iter_parent_scopes(prev_scope)
                                    .zip(iter_parent_scopes(new_scope))
                                    .take_while(|(a, b)| a == b)
                                    .map(|(a, _)| a)
                                    .last()
                                    .expect(
                                        "All expressions are within the Global scope",
                                    );

                                Some(joint_scope)
                            }
                        };
                };

            match &self[current_op].kind {
                ExprKind::Function { params, output } => {
                    let scope = Some(Scope::Function(current_op));
                    mark_scope(*output, scope);
                    params
                        .iter()
                        .cloned()
                        .for_each(|param| mark_scope(param, scope));
                }
                &ExprKind::IfElse {
                    condition,
                    if_branch,
                    else_branch,
                } => {
                    mark_scope(condition, opt_current_scope);
                    mark_scope(if_branch, Some(Scope::IfBranch(current_op)));
                    mark_scope(
                        else_branch,
                        Some(Scope::ElseBranch(current_op)),
                    );
                }
                other => other
                    .iter_input_values()
                    .for_each(|value| mark_scope(value, opt_current_scope)),
            }
        }

        // Step 3: Combine the results of steps (1) and (2)
        //
        // To minimize the amount of duplicate evaluations performed
        // in function evaluation, especially reductions, prefer to
        // have as few expressions as possible within the body of a
        // funciton.  To minimize the amount of unused evaluations in
        // branching expressions, prefer to have as many expressions
        // as possible within the body of conditional branches.

        let scope: Vec<Scope> = innermost_legal_scope
            .iter()
            .cloned()
            .zip(outermost_legal_scope.iter().cloned())
            .enumerate()
            .map(|(i, (opt_scope, func_scope))| {
                opt_scope
                    .map(|mut scope| {
                        loop {
                            if scope == func_scope {
                                break;
                            }
                            if let Scope::Function(func_index) = scope {
                                let parent_scope = innermost_legal_scope
                                    [func_index.0]
                                    .unwrap_or(
                                        outermost_legal_scope[func_index.0],
                                    );
                                assert_ne!(
                                    scope,
                                    parent_scope,
                                    "Loop in scopes, \
                                     expr {} in scope {scope:?}",
                                    OpIndex(i).pprint(self)
                                );
                                scope = parent_scope;
                            } else {
                                break;
                            }
                        }
                        scope
                    })
                    .unwrap_or(func_scope)
            })
            .collect();

        scope
    }
}

impl Scope {
    pub fn op_index(&self) -> Option<OpIndex> {
        match self {
            Scope::Global => None,
            Scope::Function(op_index) => Some(*op_index),
            Scope::IfBranch(op_index) => Some(*op_index),
            Scope::ElseBranch(op_index) => Some(*op_index),
        }
    }

    #[allow(dead_code)]
    fn printer<'a>(
        &'a self,
        graph: &'a SymbolicGraph,
    ) -> impl std::fmt::Display + 'a {
        return Printer { scope: self, graph };

        struct Printer<'a> {
            scope: &'a Scope,
            graph: &'a SymbolicGraph,
        }
        impl<'a> std::fmt::Display for Printer<'a> {
            fn fmt(
                &self,
                fmt: &mut std::fmt::Formatter<'_>,
            ) -> std::fmt::Result {
                match self.scope {
                    Scope::Global => write!(fmt, "(global)")?,
                    Scope::Function(op_index) => {
                        if let Some(name) = &self.graph[*op_index].name {
                            write!(fmt, "Function '{name}' at {op_index}")?;
                        } else {
                            write!(fmt, "Function at {op_index}")?;
                        }
                    }
                    Scope::IfBranch(op_index) => {
                        if let Some(name) = &self.graph[*op_index].name {
                            write!(fmt, "If-branch of '{name}' at {op_index}")?;
                        } else {
                            write!(fmt, "If-branch at {op_index}")?;
                        }
                    }
                    Scope::ElseBranch(op_index) => {
                        if let Some(name) = &self.graph[*op_index].name {
                            write!(
                                fmt,
                                "Else-branch of '{name}' at {op_index}"
                            )?;
                        } else {
                            write!(fmt, "Else-branch at {op_index}")?;
                        }
                    }
                }
                Ok(())
            }
        }
    }
}
