use std::{
    collections::{BTreeSet, HashMap, HashSet},
    fmt::Display,
};

use itertools::{Either, Itertools as _};

use crate::{
    bytecode::expr::Scope, Error, ExprKind, OpIndex, RuntimeType, SymbolicValue,
};

use super::{
    graph_rewrite::Analysis,
    virtual_machine::{
        AnnotationLocation, FunctionIndex, InstructionIndex, StackIndex,
        VirtualMachineBuilder,
    },
    ExposedNativeFunction, Instruction, SymbolicGraph, VMArg, VirtualMachine,
};

/// The result of analyzing a function
#[derive(Debug)]
struct ScopeInfo {
    /// The index of the function definition, which will always be a
    /// `ExprKind::Function`.  If `None`, refers to expressions that
    /// are outside the scope of any function definition.
    scope: Scope,

    /// An ordered list of expressions that are part of the body of
    /// the function.  A function's body is defined as all the set of
    /// all expressions such that the expression depends on a function
    /// parameter, and the function output depends on the expression.
    body: Vec<OpIndex>,

    /// Variables from outside of the function's body that are used by
    /// the function.
    enclosed: Vec<OpIndex>,

    /// The enclosing scope that contains this function.  Note: For
    /// Scope::Global, this field contains Scope::Global.
    parent_scope: Scope,
}

#[derive(Debug, Clone, Copy)]
struct LastUsage {
    /// The expression in which the expression is used.
    usage_point: OpIndex,

    /// The expression that was used.
    expr_used: OpIndex,
}

struct LastUsageCollector<'a> {
    graph: &'a SymbolicGraph,
    scope_info_lookup: &'a HashMap<Scope, ScopeInfo>,
}

/// Helper struct for collecting VM instructions
struct ExpressionTranslator<'a> {
    /// The graph being translated
    graph: &'a SymbolicGraph,

    /// The instructions being collected.
    instructions: &'a mut VirtualMachineBuilder,

    instructions_by_scope: &'a HashMap<Scope, Vec<OpIndex>>,

    /// Indicates the last time that an expression is used.
    last_usage: &'a [LastUsage],

    native_functions: &'a [ExposedNativeFunction],
    native_function_lookup: &'a HashMap<OpIndex, FunctionIndex>,

    index_tracking: IndexTracking,

    analysis: Analysis<'a>,

    show_steps: bool,
}

#[derive(Clone, Default)]
struct IndexTracking {
    next_free_index: usize,
    dead_indices: BTreeSet<StackIndex>,

    expr_to_reserved_location: HashMap<OpIndex, StackIndex>,

    current_location: HashMap<OpIndex, StackIndex>,
    num_aliasing: HashMap<StackIndex, usize>,

    previously_consumed: HashMap<OpIndex, OpIndex>,
}

impl SymbolicGraph {
    pub fn to_virtual_machine(
        &self,
        show_steps: bool,
    ) -> Result<VirtualMachine, Error> {
        let mut builder = VirtualMachine::builder();

        let main_func_index = self
            .iter_extern_funcs()
            .exactly_one()
            .unwrap_or_else(|_| todo!("Handle VMs with multiple functions"));

        let main_func = &self[main_func_index];

        let Some(main_func_name) = &main_func.name else {
            unreachable!(
                "Should already be checked in \
                 SymbolicGraph::mark_extern_func()"
            )
        };

        builder.mark_entry_point(main_func_name)?;

        let ExprKind::Function { params, output } = &main_func.kind else {
            panic!(
                "Internal error, \
                 `extern_funcs` should only point to functions."
            )
        };
        let output = *output;

        if !params.is_empty() {
            todo!("Handle extern functions with parameters");
        }

        let (native_functions, native_function_lookup): (
            Vec<ExposedNativeFunction>,
            HashMap<OpIndex, FunctionIndex>,
        ) = self
            .iter_ops()
            .filter_map(|(op_index, op)| match &op.kind {
                ExprKind::NativeFunction(func) => Some((op_index, func)),
                _ => None,
            })
            .map(|(op_index, func)| {
                let func_index = builder.push_raw_native_function(func.clone());
                (func.clone(), (op_index, func_index))
            })
            .unzip();

        let mut expr_to_reserved_location =
            HashMap::<OpIndex, StackIndex>::new();
        let mut next_free_index = 0;

        let iter_elements = |value: SymbolicValue| match value {
            SymbolicValue::Result(op_index) => match &self[op_index].kind {
                ExprKind::Tuple(elements) => {
                    Either::Left(elements.iter().cloned())
                }
                _ => Either::Right(Some(output).into_iter()),
            },
            _ => Either::Right(Some(output).into_iter()),
        };

        iter_elements(output).enumerate().for_each(|(i, value)| {
            let stack_index = StackIndex(i);
            next_free_index += 1;
            match value {
                SymbolicValue::Result(op_index) => {
                    expr_to_reserved_location.insert(op_index, stack_index);
                }
                other => {
                    let value: VMArg = other
                        .as_prim_value()
                        .expect("Only SymbolicValue::Result returns None")
                        .into();
                    builder.push(Instruction::Copy {
                        value,
                        output: stack_index,
                    });
                }
            }
        });
        let num_outputs = next_free_index;

        let reachable = self.reachable(self.iter_extern_funcs());
        let scope = self.operation_scope(&reachable);

        let last_usage = self.last_usage();

        let operations_by_scope = scope
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, scope)| (scope, OpIndex::new(i)))
            .into_group_map();

        let iter_op_indices = scope
            .iter()
            .enumerate()
            .filter(|(_, scope)| **scope == Scope::Global)
            .map(|(i, _)| OpIndex::new(i))
            .filter(|index| {
                !matches!(self[*index].kind, ExprKind::Function { .. })
            });

        let index_tracking = IndexTracking {
            next_free_index,
            expr_to_reserved_location,
            ..Default::default()
        };

        let mut translator = ExpressionTranslator {
            graph: self,
            instructions: &mut builder,
            instructions_by_scope: &operations_by_scope,
            last_usage: &last_usage,
            native_functions: &native_functions,
            native_function_lookup: &native_function_lookup,
            show_steps,
            analysis: Analysis::new(None),
            index_tracking,
        };
        translator.translate(iter_op_indices)?;

        let return_instruction = Instruction::Return {
            outputs: iter_elements(output)
                .map(|output_value| -> Result<VMArg, Error> {
                    Ok(match output_value {
                        SymbolicValue::Result(output_index) => translator
                            .index_tracking
                            .expr_to_location(output_index)?
                            .unwrap_or_else(|| {
                                panic!(
                                    "All outputs should be produced by now, \
                                     but {output_index} was not in the index tracker."
                                )
                            })
                            .into(),

                        other => other
                            .as_prim_value()
                            .expect("Should be result or constant")
                            .into(),
                    })
                })
                .collect::<Result<_, _>>()?,
        };
        translator.push_annotated(return_instruction, || {
            format!("return from top-level function")
        });

        Ok(builder.num_outputs(num_outputs).build())
    }

    fn analyze_scopes(&self) -> Vec<ScopeInfo> {
        let iter_scopes = self.iter_ops().flat_map(|(index, op)| {
            let (a, b) = match op.kind {
                ExprKind::Function { .. } => {
                    (Some(Scope::Function(index)), None)
                }
                ExprKind::IfElse { .. } => (
                    Some(Scope::IfBranch(index)),
                    Some(Scope::ElseBranch(index)),
                ),
                _ => (None, None),
            };
            a.into_iter().chain(b)
        });

        let mut info: Vec<_> = std::iter::once(Scope::Global)
            .chain(iter_scopes)
            .map(|scope| ScopeInfo {
                scope,
                parent_scope: Scope::Global,
                body: vec![],
                enclosed: vec![],
            })
            .collect();

        let mut scope_lookup: HashMap<Scope, &mut ScopeInfo> = info
            .iter_mut()
            .map(|func_info| (func_info.scope, func_info))
            .collect();

        let reachable = self.reachable(self.iter_extern_funcs());
        let operation_to_scope = self.operation_scope(&reachable);

        // Step 1: Collect the body of each scope.
        operation_to_scope
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, scope)| (OpIndex::new(i), scope))
            .for_each(|(op_index, scope_info)| {
                if let Some(func_info) = scope_lookup.get_mut(&scope_info) {
                    func_info.body.push(op_index);
                }
            });

        // Step 2: Collect the parent of each scope.
        for (_, scope_info) in scope_lookup.iter_mut() {
            if let Some(OpIndex(i)) = scope_info.scope.op_index() {
                scope_info.parent_scope = operation_to_scope[i];
            }
        }

        // Step 3: Collect variables that are enclosed because they
        // are directly used within a scope.
        self.iter_ops()
            .zip(operation_to_scope.iter().cloned())
            .flat_map(|((op_index, op), scope)| match &op.kind {
                // Generate an iterator whose elements are the node
                // that was used, and the scope in which that usage
                // occurred.  For most expressions, this just iterates
                // over the inputs.  The Function and IfElse
                // expression types are handled separately, as their
                // inputs are used within a child scope.
                ExprKind::Function { params, output } => {
                    let iter = params
                        .iter()
                        .chain(Some(output))
                        .filter_map(|value| value.as_op_index())
                        .map(move |usage| (usage, Scope::Function(op_index)));
                    Either::Left(Either::Left(iter))
                }
                ExprKind::IfElse {
                    condition,
                    if_branch,
                    else_branch,
                } => {
                    let iter = [
                        (condition, scope),
                        (if_branch, Scope::IfBranch(op_index)),
                        (else_branch, Scope::ElseBranch(op_index)),
                    ]
                    .into_iter()
                    .filter_map(
                        |(value_used, used_in_scope)| {
                            value_used
                                .as_op_index()
                                .map(|node_used| (node_used, used_in_scope))
                        },
                    );
                    Either::Left(Either::Right(iter))
                }
                other => {
                    let iter = other
                        .iter_input_nodes()
                        .map(move |usage| (usage, scope));
                    Either::Right(iter)
                }
            })
            .flat_map(|(index_used, used_in_scope)| {
                // For each usage, walk up the scope tree until
                // encountering the scope that defines the expression.
                let OpIndex(i) = index_used;
                let definition_scope = operation_to_scope[i];
                std::iter::successors(Some(used_in_scope), |scope| {
                    scope_lookup
                        .get(scope)
                        .and_then(|info| Some(info.parent_scope))
                })
                .take_while(move |scope| {
                    *scope != Scope::Global && *scope != definition_scope
                })
                .map(move |scope| (scope, index_used))
            })
            .unique()
            .sorted_by_key(|(_, op_index)| *op_index)
            .into_group_map()
            .into_iter()
            .for_each(|(scope, enclosed)| {
                scope_lookup
                    .get_mut(&scope)
                    .expect("Scope lookup should include all scopes")
                    .enclosed = enclosed;
            });

        info
    }

    fn last_usage(&self) -> Vec<LastUsage> {
        let scope_info_lookup = self
            .analyze_scopes()
            .into_iter()
            .map(|info| (info.scope, info))
            .collect();

        let collector = LastUsageCollector {
            graph: self,
            scope_info_lookup: &scope_info_lookup,
        };

        let mut last_usage = Vec::<LastUsage>::new();
        let mut encountered = HashSet::<OpIndex>::new();

        collector.walk_tree(
            &mut encountered,
            &mut last_usage,
            collector.iter_scope(Scope::Global),
        );

        last_usage
            .into_iter()
            .filter(|last_usage| match &self[last_usage.expr_used].kind {
                ExprKind::Function { .. } => false,
                ExprKind::FunctionArg(_) => false,
                ExprKind::NativeFunction(_) => false,
                _ => true,
            })
            .sorted_by_key(|last_usage| {
                (last_usage.usage_point, last_usage.expr_used)
            })
            .collect()
    }
}

impl IndexTracking {
    fn expr_to_location(
        &self,
        op_index: OpIndex,
    ) -> Result<Option<StackIndex>, Error> {
        if self.previously_consumed.contains_key(&op_index) {
            return Err(Error::AttemptedUseOfConsumedValue);
        }

        Ok(self.current_location.get(&op_index).cloned())
    }

    fn reserve_index(&mut self, op_index: OpIndex, stack_index: StackIndex) {
        {
            let previous_reserved_location =
                self.expr_to_reserved_location.get(&op_index);
            assert!(
                previous_reserved_location.is_none(),
                "Attempted to reserve {stack_index} to store {op_index}, \
                 but {} is already reserved for {op_index}",
                previous_reserved_location.unwrap(),
            );
        }

        self.expr_to_reserved_location.insert(op_index, stack_index);
        self.dead_indices.remove(&stack_index);
    }

    fn release_reservation(&mut self, op_index: OpIndex) -> StackIndex {
        let Some(stack_index) =
            self.expr_to_reserved_location.remove(&op_index)
        else {
            unreachable!(
                "Internal error: \
                 Attempted to release reservation for {op_index}, \
                 but no such reservation existed."
            )
        };

        assert!(
            self.current_location.contains_key(&op_index),
            "Releasing reservation of {stack_index} for {op_index}, \
             but the reservation had never been used."
        );

        stack_index
    }

    fn define_contents(&mut self, expr: OpIndex, loc: StackIndex) {
        assert!(
            !self.dead_indices.contains(&loc),
            "Attempted to define {loc} as containing {expr}, \
             but {loc} is currently listed as a dead index."
        );
        {
            let current_loc = self.current_location.get(&expr);
            assert!(
                current_loc.is_none(),
                "Attempted to define {loc} as containing {expr}, \
                 but {expr} is already stored in {}.",
                current_loc.unwrap(),
            );
        }

        self.current_location.insert(expr, loc);
        *self.num_aliasing.entry(loc).or_insert(0) += 1;
    }

    fn release_expr(&mut self, expr: OpIndex) -> StackIndex {
        let Some(stack_index) = self.current_location.remove(&expr) else {
            panic!(
                "Internal error: \
                 Attempted to release expr {expr}, \
                 but it wasn't actually stored anywhere."
            )
        };

        let Some(num_aliasing) = self.num_aliasing.get_mut(&stack_index) else {
            panic!(
                "Internal inconsistency: \
                 When removing {expr} from {stack_index}, \
                 reverse map had no entry for {stack_index}."
            );
        };

        assert!(*num_aliasing > 0);

        *num_aliasing -= 1;

        if *num_aliasing == 0 {
            // The location-to-expr map points to the current
            // expression.  The current expression owns the stack
            // location, and so the stack location can be reused.
            self.num_aliasing.remove(&stack_index);
            self.dead_indices.insert(stack_index);
        } else {
            // The location-to-expr map points to a different
            // expression.  The current expression is an alias,
            // and the original expression may still use the stack
            // location.
        }

        stack_index
    }

    fn merge_conditional_branches(&mut self, mut other: Self) {
        self.next_free_index = self.next_free_index.max(other.next_free_index);

        // After the conditional, dead indices marked as dead
        // while following either branch are considered
        // dead.
        self.dead_indices.append(&mut other.dead_indices);

        // Likewise, only values that are currently stored
        // at the end of both branches have a known
        // storage location after the conditional.
        self.current_location = other
            .current_location
            .into_iter()
            .filter(|(op_index, _)| {
                self.current_location.contains_key(op_index)
            })
            .collect();

        // A value is considered consumed if either branch consumed
        // it.
        for (consumed, consumed_by) in other.previously_consumed {
            self.previously_consumed.insert(consumed, consumed_by);
        }

        // All reservations should expire after the instruction that
        // produced them.  Both branches of a conditional should
        // conclude with the same set of reserved output locations.
        assert_eq!(
            self.expr_to_reserved_location,
            other.expr_to_reserved_location
        );
    }
}

impl<'a> LastUsageCollector<'a> {
    fn iter_scope(&self, scope: Scope) -> impl Iterator<Item = OpIndex> + '_ {
        self.scope_info_lookup
            .get(&scope)
            .into_iter()
            .flat_map(|info| info.body.iter().rev())
            .cloned()
    }

    fn iter_enclosed(
        &self,
        func: SymbolicValue,
    ) -> impl Iterator<Item = OpIndex> + '_ {
        func.as_op_index()
            .and_then(|func_index| {
                self.scope_info_lookup.get(&Scope::Function(func_index))
            })
            .map(|info| info.enclosed.iter())
            .into_iter()
            .flatten()
            .cloned()
    }

    fn walk_tree(
        &self,
        encountered: &mut HashSet<OpIndex>,
        last_usage: &mut Vec<LastUsage>,
        to_visit: impl Iterator<Item = OpIndex>,
    ) {
        for visiting in to_visit {
            macro_rules! mark_value {
                ($node_set:expr, $value:expr) => {
                    if let SymbolicValue::Result(index) = $value {
                        if !$node_set.contains(&index) {
                            last_usage.push(LastUsage {
                                usage_point: visiting,
                                expr_used: index,
                            });
                            $node_set.insert(index);
                        }
                    }
                };
            }

            match &self.graph[visiting].kind {
                ExprKind::Function { output, .. } => {
                    self.walk_tree(
                        encountered,
                        last_usage,
                        self.iter_scope(Scope::Function(visiting)),
                    );
                    mark_value!(encountered, *output);
                }

                ExprKind::IfElse {
                    condition,
                    if_branch,
                    else_branch,
                } => {
                    mark_value!(encountered, *condition);

                    let mut encountered_if: HashSet<OpIndex> =
                        encountered.clone();
                    self.walk_tree(
                        &mut encountered_if,
                        last_usage,
                        self.iter_scope(Scope::IfBranch(visiting)),
                    );
                    mark_value!(encountered_if, *if_branch);

                    let mut encountered_else: HashSet<OpIndex> =
                        encountered.clone();
                    self.walk_tree(
                        &mut encountered_else,
                        last_usage,
                        self.iter_scope(Scope::ElseBranch(visiting)),
                    );
                    mark_value!(encountered_else, *else_branch);

                    if let Some(start_of_else) =
                        self.iter_scope(Scope::ElseBranch(visiting)).next()
                    {
                        for index in &encountered_if {
                            if encountered.contains(index) {
                                if !encountered_else.contains(index) {
                                    last_usage.push(LastUsage {
                                        usage_point: start_of_else,
                                        expr_used: *index,
                                    })
                                }
                            }
                        }
                    }

                    if let Some(start_of_if) =
                        self.iter_scope(Scope::IfBranch(visiting)).next()
                    {
                        for index in &encountered_else {
                            if encountered.contains(index) {
                                if !encountered_if.contains(index) {
                                    last_usage.push(LastUsage {
                                        usage_point: start_of_if,
                                        expr_used: *index,
                                    });
                                }
                            }
                        }
                    }

                    encountered_if
                        .into_iter()
                        .chain(encountered_else)
                        .for_each(|index| {
                            encountered.insert(index);
                        });
                }

                ExprKind::SimpleReduce {
                    initial,
                    extent,
                    reduction,
                } => {
                    mark_value!(encountered, *initial);
                    mark_value!(encountered, *extent);
                    self.iter_enclosed(*reduction)
                        .map(SymbolicValue::Result)
                        .for_each(|value| mark_value!(encountered, value));
                    mark_value!(encountered, *reduction);
                }

                other => other.visit_reachable_nodes(|value| {
                    mark_value!(encountered, value)
                }),
            }
        }
    }
}

struct LocalIndexPrinter<'a> {
    index: OpIndex,
    name: Option<&'a str>,
}
impl<'a> LocalIndexPrinter<'a> {
    fn new(index: OpIndex, graph: &'a SymbolicGraph) -> Self {
        Self {
            index,
            name: graph[index].name.as_ref().map(|string| string.as_str()),
        }
    }
}
impl<'a> std::fmt::Display for LocalIndexPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = self.name {
            write!(f, "'{name}' {}", self.index)
        } else {
            write!(f, "{}", self.index)
        }
    }
}

impl ExpressionTranslator<'_> {
    fn value_to_arg(
        &self,
        usage: OpIndex,
        value: &SymbolicValue,
    ) -> Result<VMArg, Error> {
        let op_index = match value {
            SymbolicValue::Result(op_index) => *op_index,
            other => {
                let prim = other
                    .as_prim_value()
                    .expect("Argument must be primitive or a prior result");
                let arg = VMArg::Const(prim);
                return Ok(arg);
            }
        };

        Ok(self
            .index_tracking
            .expr_to_location(op_index)?
            .unwrap_or_else(|| {
                panic!(
                    "Internal error, \
                         expression {usage} ({}) attempted to use \
                         {op_index} ({}), \
                         but {op_index} was not previously translated.",
                    self.graph[usage].kind, self.graph[op_index].kind,
                )
            })
            .into())
    }

    fn annotate<Func, Annot>(&mut self, generate_annotation: Func)
    where
        Func: FnOnce(&SymbolicGraph) -> Annot,
        Annot: Display,
    {
        if self.show_steps {
            let index = self.instructions.current_index();
            let loc = AnnotationLocation::Before(index);
            self.instructions
                .annotate(loc, generate_annotation(self.graph));
        }
    }

    fn reserve_index(&mut self, op_index: OpIndex, stack_index: StackIndex) {
        self.annotate(|graph| {
            format!(
                "Reserving {stack_index} for {}",
                LocalIndexPrinter::new(op_index, graph)
            )
        });
        self.index_tracking.reserve_index(op_index, stack_index);
    }

    fn release_reservation(&mut self, op_index: OpIndex) {
        let stack_index = self.index_tracking.release_reservation(op_index);

        self.annotate(|graph| {
            format!(
                "Releasing reservation of {stack_index} for {}",
                LocalIndexPrinter::new(op_index, graph),
            )
        });
    }

    fn alloc_index(&mut self) -> StackIndex {
        if let Some(index) = self.index_tracking.dead_indices.pop_first() {
            self.annotate(|_| format!("Reusing dead index {index}"));
            index
        } else {
            let index = StackIndex(self.index_tracking.next_free_index);
            self.index_tracking.next_free_index += 1;
            self.annotate(|_| {
                format!("No dead indices, using new index {index}")
            });
            index
        }
    }

    fn get_output_index(&mut self, op_index: OpIndex) -> StackIndex {
        if let Some(&index) =
            self.index_tracking.expr_to_reserved_location.get(&op_index)
        {
            self.annotate(|graph| {
                format!(
                    "For op {}, \
                     using reserved output {index}",
                    LocalIndexPrinter::new(op_index, graph),
                )
            });
            index
        } else {
            self.annotate(|graph| {
                format!(
                    "For op {}, allocating output index.",
                    LocalIndexPrinter::new(op_index, graph),
                )
            });
            self.alloc_index()
        }
    }

    fn free_index(&mut self, stack_index: StackIndex) {
        self.annotate(|_| {
            format!(
                "Location {stack_index} no longer needed, \
                 marking as dead."
            )
        });
        self.index_tracking.dead_indices.insert(stack_index);
    }

    fn free_dead_indices(&mut self, op_index: OpIndex) {
        let first_index = self.last_usage.partition_point(|last_usage| {
            last_usage.usage_point.0 < op_index.0
        });
        self.last_usage[first_index..]
            .iter()
            .take_while(|last_usage| last_usage.usage_point == op_index)
            .for_each(|last_usage| {
                let expr_used = last_usage.expr_used;

                let stack_index = self.index_tracking.release_expr(expr_used);

                self.annotate(|graph| {
                    format!(
                        "After {}, {} is no longer needed.  \
                         Marking {stack_index} as dead.",
                        LocalIndexPrinter::new(op_index, graph),
                        LocalIndexPrinter::new(expr_used, graph),
                    )
                });
            });
    }

    fn push_annotated<Func, Annot>(
        &mut self,
        inst: Instruction,
        generate_annotation: Func,
    ) -> InstructionIndex
    where
        Func: FnOnce() -> Annot,
        Annot: Display,
    {
        let index = self.instructions.push(inst);
        if self.show_steps {
            self.instructions.annotate(index, generate_annotation());
        }
        index
    }

    fn translate(
        &mut self,
        instructions: impl Iterator<Item = OpIndex>,
    ) -> Result<(), Error> {
        for op_index in instructions {
            let op = &self.graph[op_index];
            let expr_name = LocalIndexPrinter::new(op_index, self.graph);

            macro_rules! handle_binary_op {
                ($variant:ident, $lhs:expr, $rhs:expr) => {{
                    let lhs = self.value_to_arg(op_index, $lhs)?;
                    let rhs = self.value_to_arg(op_index, $rhs)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::$variant {
                            lhs,
                            rhs,
                            output: op_output,
                        },
                        || format!("evaluate {expr_name}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }};
            }

            match op.as_ref() {
                ExprKind::None => {
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::Clear { loc: op_output },
                        || format!("generate None for {expr_name}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }

                ExprKind::Function { .. } => {
                    unreachable!(
                        "Function calls should be inlined, \
                         and should only be encountered in SimpleReduce.  \
                         But at index {op_index}, encountered function named {:?}",
                        op.name
                    )
                }
                ExprKind::FunctionArg(_) => {
                    assert!(self
                        .index_tracking
                        .current_location
                        .contains_key(&op_index));
                }
                ExprKind::Tuple(_) => {
                    // Eventually I should put something here, but I
                    // think this will just barely work for the
                    // current use cases.  A tuple of function outputs
                    // is assigned the appropriate StackIndex values
                    // for each tuple element.  When encountering
                    // those operations, their output was written to
                    // the appropriate output index.  So now, when
                    // encountering the tuple itself, I just
                    // do...nothing.
                    //
                    // This will break horribly if tuples are used in
                    // any other context, but thankfully I don't yet
                    // support any other contexts.
                }
                ExprKind::FunctionCall { func, args } => {
                    self.translate_function_call(op_index, *func, args)?;
                }

                iterator @ (ExprKind::Range { .. }
                | ExprKind::Map { .. }
                | ExprKind::Filter { .. }
                | ExprKind::Collect { .. }
                | ExprKind::Reduce { .. }) => panic!(
                    "All {} expressions should be simplified \
                     to ExprKind::SimpleReduce",
                    iterator.op_name(),
                ),
                ExprKind::SimpleReduce {
                    initial,
                    extent,
                    reduction,
                } => {
                    self.translate_simple_reduce(
                        op_index, *initial, *extent, *reduction,
                    )?;
                }
                ExprKind::NativeFunction(_) => {
                    assert!(
                        self.native_function_lookup.contains_key(&op_index),
                        "Internal error: \
                         Lookup should be populated with \
                         all native functions."
                    );
                }

                ExprKind::IsSome(value) => {
                    let value = self.value_to_arg(op_index, value)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::IsSome {
                            value,
                            output: op_output,
                        },
                        || format!("evaluate {expr_name}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }

                ExprKind::IfElse {
                    condition,
                    if_branch,
                    else_branch,
                } => {
                    self.translate_if_else(
                        op_index,
                        *condition,
                        *if_branch,
                        *else_branch,
                    )?;
                }

                ExprKind::Not { arg } => {
                    let arg = self.value_to_arg(op_index, arg)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::Not {
                            arg,
                            output: op_output,
                        },
                        || format!("evaluate {expr_name}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }

                ExprKind::Equal { lhs, rhs } => {
                    handle_binary_op!(Equal, lhs, rhs)
                }
                ExprKind::NotEqual { lhs, rhs } => {
                    handle_binary_op!(NotEqual, lhs, rhs)
                }
                ExprKind::GreaterThan { lhs, rhs } => {
                    handle_binary_op!(GreaterThan, lhs, rhs)
                }
                ExprKind::LessThan { lhs, rhs } => {
                    handle_binary_op!(LessThan, lhs, rhs)
                }
                ExprKind::GreaterThanOrEqual { lhs, rhs } => {
                    handle_binary_op!(GreaterThanOrEqual, lhs, rhs)
                }
                ExprKind::LessThanOrEqual { lhs, rhs } => {
                    handle_binary_op!(LessThanOrEqual, lhs, rhs)
                }

                ExprKind::Add { lhs, rhs } => handle_binary_op!(Add, lhs, rhs),
                ExprKind::Mul { lhs, rhs } => handle_binary_op!(Mul, lhs, rhs),
                ExprKind::Div { lhs, rhs } => handle_binary_op!(Div, lhs, rhs),
                ExprKind::Mod { lhs, rhs } => handle_binary_op!(Mod, lhs, rhs),

                ExprKind::PrimCast { value, prim_type } => {
                    let value = self.value_to_arg(op_index, value)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::PrimCast {
                            value,
                            prim_type: *prim_type,
                            output: op_output,
                        },
                        || format!("eval {expr_name}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }
                ExprKind::PhysicalDowncast { obj, ty } => {
                    let obj = self.value_to_arg(op_index, obj)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::Downcast {
                            obj,
                            subtype: *ty,
                            output: op_output,
                        },
                        || format!("eval {expr_name}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }
                ExprKind::ReadValue { ptr, prim_type } => {
                    let ptr = self.value_to_arg(op_index, ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::Read {
                            ptr,
                            prim_type: *prim_type,
                            output: op_output,
                        },
                        || format!("eval {expr_name}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }
                ExprKind::ReadString { ptr } => {
                    let ptr = self.value_to_arg(op_index, ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::ReadString {
                            ptr,
                            output: op_output,
                        },
                        || format!("eval {expr_name}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }
                ExprKind::PointerCast { ptr, .. } => {
                    let ptr = self.value_to_arg(op_index, ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::Copy {
                            value: ptr,
                            output: op_output,
                        },
                        || format!("eval {expr_name}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }

                boolean @ (ExprKind::And { .. } | ExprKind::Or { .. }) => {
                    return Err(Error::BooleanOperatorRequiresLowering(
                        boolean.clone(),
                    ));
                }

                symbolic @ (ExprKind::StaticField(_)
                | ExprKind::FieldAccess { .. }
                | ExprKind::SymbolicDowncast { .. }
                | ExprKind::IndexAccess { .. }
                | ExprKind::NumArrayElements { .. }
                | ExprKind::ArrayExtent { .. }) => {
                    return Err(Error::SymbolicExpressionRequiresLowering(
                        symbolic.clone(),
                    ));
                }
            }
        }

        Ok(())
    }

    /// Translate a scoped expression
    ///
    /// * `op_index`: The index of the expression that requires the
    ///       scoped.  (e.g. A `FunctionCall`, `IfElse`, or
    ///       `SimpleReduce` expression)
    ///
    /// * `value`: The value that should be produced as the output of
    ///       the scope.
    ///
    /// * `out_index`: The index to which `value` should be written.
    ///
    /// * `scope`: The scope to be evaluated.
    fn translate_scope(
        &mut self,
        op_index: OpIndex,
        value: SymbolicValue,
        out_stack_index: StackIndex,
        scope: Scope,
    ) -> Result<(), Error> {
        let expr_name = LocalIndexPrinter::new(op_index, self.graph);

        let scope_output = match value {
            SymbolicValue::Result(i) => i,
            other => {
                // Early return in case the scope returns a constant.
                let value = other.as_prim_value().unwrap().into();
                self.push_annotated(
                    Instruction::Copy {
                        value,
                        output: out_stack_index,
                    },
                    || format!("write output of {expr_name}"),
                );
                return Ok(());
            }
        };

        if let Some(&currently_at) =
            self.index_tracking.current_location.get(&scope_output)
        {
            if currently_at == out_stack_index {
                // Already in the desired location, no action needed
            } else if matches!(
                self.analysis.infer_type(self.graph, value)?,
                RuntimeType::Prim(_)
            ) {
                // A primitive value can be copied into the desired
                // location.
                self.push_annotated(
                    Instruction::Copy {
                        value: currently_at.into(),
                        output: out_stack_index,
                    },
                    || format!("copy value to output of {expr_name}"),
                );
            } else if self
                .last_usage
                .binary_search_by_key(&(op_index, scope_output), |last_usage| {
                    (last_usage.usage_point, last_usage.expr_used)
                })
                .is_ok()
            {
                // A rust-native object may be moved to the output
                // location, if this is the last usage of the
                // rust-native object in its current state.
                self.push_annotated(
                    Instruction::Swap(out_stack_index, currently_at),
                    || format!("copy value to output of {expr_name}"),
                );
            } else {
                todo!(
                    "Limitation of current register assignment.  \
                     Evaluation of {scope:?} requires \
                     placing output in location {out_stack_index}.  \
                     However, it is currently located in {currently_at}, \
                     and can neither be copied to the output location \
                     (only primitives can be copied), \
                     nor can it be moved be moved to the output location \
                     (requires this usage to be the last usage).  \
                     Fixing this will require implementing \
                     better aliasing variable analysis."
                )
            }
        } else {
            // The output does not yet exist, need to evaluate
            // statements in the scope to generate it.
            self.reserve_index(scope_output, out_stack_index);
            let iter_scope = self
                .instructions_by_scope
                .get(&scope)
                .into_iter()
                .flatten()
                .cloned()
                .filter(|&op| {
                    !matches!(self.graph[op].kind, ExprKind::Function { .. },)
                });
            let iter_scope: Box<dyn Iterator<Item = OpIndex>> =
                Box::new(iter_scope);

            self.translate(iter_scope)?;
            self.release_reservation(scope_output);

            // In case the output was stored somewhere other than the
            // reserved address, move it to the correct location.
            let body_output = self
                .index_tracking
                .current_location
                .get(&scope_output)
                .expect("Output of scope should not be generated")
                .clone();
            if body_output != out_stack_index {
                self.push_annotated(
                    Instruction::Swap(body_output, out_stack_index),
                    || format!("move output of {expr_name}"),
                );
            }
        }

        Ok(())
    }

    fn translate_function_call(
        &mut self,
        op_index: OpIndex,
        func: SymbolicValue,
        args: &[SymbolicValue],
    ) -> Result<(), Error> {
        let expr_name = LocalIndexPrinter::new(op_index, self.graph);

        let Some(func) = func.as_op_index() else {
            panic!("Internal error, callee must be function")
        };

        if let Some(&native_func_index) = self.native_function_lookup.get(&func)
        {
            let mut vm_args = Vec::new();
            for arg in args {
                vm_args.push(self.value_to_arg(op_index, arg)?);
            }

            let mutates_first_argument = self.native_functions
                [native_func_index.0]
                .mutates_first_argument();

            if mutates_first_argument {
                // The function mutates its input
                // argument.  No output index is required.
                // However, the `current_location` lookup
                // should be updated to no longer contain
                // the first argument.
                let first_arg_op = match &args[0] {
                                SymbolicValue::Result(op_index) => *op_index,
                                SymbolicValue::Bool(_) |SymbolicValue::Int(_) |
                                SymbolicValue::Ptr(_) => todo!(
                                    "Attempted mutation of const SymbolicValue.  \
                                     Should handle this case earlier using \
                                     a new ExprKind to represent mutable constants."
                                ),

                            };
                let first_arg_loc = match &vm_args[0] {
                                VMArg::SavedValue(stack_index) => *stack_index,
                                VMArg::Const(_) => todo!(
                                    "Attempted mutation of VMArg::Const.  \
                                     Should handle this case earlier using \
                                     a new ExprKind to represent mutable constants."
                                ),
                            };

                self.push_annotated(
                    Instruction::NativeFunctionCall {
                        index: native_func_index,
                        args: vm_args,
                        output: None,
                    },
                    || format!("produce {expr_name}"),
                );

                // self.index_tracking.current_location.remove(&first_arg_op);
                // self.index_tracking.release_expr(first_arg_op);
                self.index_tracking
                    .previously_consumed
                    .insert(first_arg_op, op_index);

                if let Some(&required_output) =
                    self.index_tracking.expr_to_reserved_location.get(&op_index)
                {
                    if first_arg_loc != required_output {
                        self.push_annotated(
                                        Instruction::Swap(first_arg_loc, required_output),
                                        || format!(
                                            "swap {expr_name} \
                                             to mandatory output loc {required_output}"
                                        ),
                                    );
                    }
                    self.index_tracking
                        .define_contents(op_index, required_output);
                } else {
                    self.index_tracking
                        .define_contents(op_index, first_arg_loc);
                }
            } else {
                // The function produces an output value, to
                // be stored in the output index.
                let op_output = self.get_output_index(op_index);
                self.push_annotated(
                    Instruction::NativeFunctionCall {
                        index: native_func_index,
                        args: vm_args,
                        output: Some(op_output),
                    },
                    || format!("produce {expr_name}"),
                );
                self.index_tracking.define_contents(op_index, op_output);
            }
        } else {
            todo!(
                "Handle IR-defined function calls in the VM.  \
                             Until then, all functions should be inlined."
            )
        }

        self.free_dead_indices(op_index);

        Ok(())
    }

    fn translate_simple_reduce(
        &mut self,
        op_index: OpIndex,
        initial: SymbolicValue,
        extent: SymbolicValue,
        reduction: SymbolicValue,
    ) -> Result<(), Error> {
        let expr_name = LocalIndexPrinter::new(op_index, self.graph);

        let initial = self.value_to_arg(op_index, &initial)?;
        let extent = self.value_to_arg(op_index, &extent)?;

        let op_output = self.get_output_index(op_index);

        match initial {
            VMArg::Const(_) => {
                self.push_annotated(
                    Instruction::Copy {
                        value: initial,
                        output: op_output,
                    },
                    || {
                        format!(
                            "copy initial value \
                             of reduction {expr_name}"
                        )
                    },
                );
            }
            VMArg::SavedValue(stack_index) => {
                if stack_index != op_output {
                    self.push_annotated(
                        Instruction::Swap(stack_index, op_output),
                        || {
                            format!(
                                "move initial value \
                                 of reduction {expr_name}"
                            )
                        },
                    );
                }
            }
        }

        let extent_is_none = self.alloc_index();
        self.push_annotated(
            Instruction::IsSome {
                value: extent,
                output: extent_is_none,
            },
            || format!("extent {extent} is some"),
        );
        self.push_annotated(
            Instruction::Not {
                arg: extent_is_none.into(),
                output: extent_is_none,
            },
            || format!("extent {extent} is none"),
        );
        let extent_is_zero = self.alloc_index();
        self.push_annotated(
            Instruction::Equal {
                lhs: extent,
                rhs: 0usize.into(),
                output: extent_is_zero,
            },
            || format!("extent {extent} is zero"),
        );

        self.free_index(extent_is_none);
        self.free_index(extent_is_zero);
        let can_skip_loop = self.alloc_index();
        self.push_annotated(
            Instruction::Or {
                lhs: extent_is_none.into(),
                rhs: extent_is_zero.into(),
                output: can_skip_loop,
            },
            || format!("loop over {extent} can be skipped"),
        );

        // Placeholder for the conditional jump to break
        // out of the loop.  To be updated after the loop
        // body is generated, when we know the destination
        // index of the jump.
        let jump_to_end_instruction_index = self
            .push_annotated(Instruction::NoOp, || {
                format!("skip empty reduction loop for {expr_name}")
            });
        self.free_index(can_skip_loop);

        let reduction_index = match reduction {
            SymbolicValue::Result(op_index) => op_index,
            _ => todo!(
                "Better error message \
                             when SimpleReduce points to non-function"
            ),
        };
        let loop_iter_name = match &self.graph[reduction_index].kind {
            ExprKind::Function { params, .. } => Some(params[1]),
            _ => None,
        }
        .and_then(|param| param.as_op_index())
        .and_then(|param_index| self.graph[param_index].name.as_ref())
        .map(|name| format!("loop iterator '{name}'"))
        .unwrap_or_else(|| "loop iterator".into());

        let loop_iter = self.alloc_index();
        self.push_annotated(
            Instruction::Copy {
                value: 0usize.into(),
                output: loop_iter,
            },
            || format!("initialize {loop_iter_name} for {expr_name}"),
        );

        let loop_start = self.instructions.current_index();

        match &self.graph[reduction_index].kind {
            ExprKind::Function { params, output } => {
                if params.len() != 2 {
                    todo!(
                        "Better error message for invalid reduction function"
                    );
                }
                let accumulator = match params[0] {
                    SymbolicValue::Result(op_index) => op_index,
                    _ => todo!("Better error message for ill-formed function"),
                };
                let index = match params[1] {
                    SymbolicValue::Result(op_index) => op_index,
                    _ => todo!("Better error message for ill-formed function"),
                };

                self.index_tracking.define_contents(accumulator, op_output);
                self.index_tracking.define_contents(index, loop_iter);

                self.translate_scope(
                    op_index,
                    *output,
                    op_output,
                    Scope::Function(reduction_index),
                )?;

                self.index_tracking.current_location.remove(&accumulator);
                self.index_tracking.current_location.remove(&index);
                // self.index_tracking.release_expr(accumulator);
                // self.index_tracking.release_expr(index);
            }
            ExprKind::NativeFunction(_) => {
                let native_func_index =
                    self.native_function_lookup.get(&reduction_index).expect(
                        "Internal error, \
                                     function should have \
                                     already been encountered.",
                    );
                let mutates_first_argument = self.native_functions
                    [native_func_index.0]
                    .mutates_first_argument();

                let func_output = if mutates_first_argument {
                    None
                } else {
                    Some(op_output)
                };

                self.push_annotated(
                    Instruction::NativeFunctionCall {
                        index: *native_func_index,
                        args: vec![op_output.into(), loop_iter.into()],
                        output: func_output,
                    },
                    || format!("native call to reduce into {expr_name}"),
                );
            }
            _ => todo!(
                "Better error message \
                             when SimpleReduce points to non-function"
            ),
        }

        self.push_annotated(
            Instruction::Add {
                lhs: loop_iter.into(),
                rhs: 1usize.into(),
                output: loop_iter,
            },
            || format!("increment {loop_iter_name} for {expr_name}"),
        );

        let loop_condition = self.alloc_index();
        self.push_annotated(
            Instruction::LessThan {
                lhs: loop_iter.into(),
                rhs: extent.into(),
                output: loop_condition,
            },
            || format!("loop condition for {expr_name}"),
        );

        self.push_annotated(
            Instruction::ConditionalJump {
                cond: loop_condition.into(),
                dest: loop_start,
            },
            || format!("jump to beginning of {expr_name}"),
        );
        self.free_index(loop_condition);

        let after_loop = self.instructions.current_index();
        self.instructions.update(
            jump_to_end_instruction_index,
            Instruction::ConditionalJump {
                cond: can_skip_loop.into(),
                dest: after_loop,
            },
        );

        self.free_dead_indices(op_index);

        self.index_tracking.define_contents(op_index, op_output);

        Ok(())
    }

    fn translate_if_else(
        &mut self,
        op_index: OpIndex,
        condition: SymbolicValue,
        if_branch: SymbolicValue,
        else_branch: SymbolicValue,
    ) -> Result<(), Error> {
        let expr_name = LocalIndexPrinter::new(op_index, self.graph);

        let condition = self.value_to_arg(op_index, &condition)?;

        let op_output = self.get_output_index(op_index);

        let mut cached = self.index_tracking.clone();

        let jump_to_if_branch_index = self
            .push_annotated(Instruction::NoOp, || {
                format!("jump to if branch of {expr_name}")
            });

        self.translate_scope(
            op_index,
            else_branch,
            op_output,
            Scope::ElseBranch(op_index),
        )?;

        std::mem::swap(&mut self.index_tracking, &mut cached);

        let jump_to_branch_end_index =
            self.push_annotated(Instruction::NoOp, || {
                format!(
                    "after else branch {expr_name}, \
                             skip the if branch"
                )
            });

        self.instructions.update(
            jump_to_if_branch_index,
            Instruction::ConditionalJump {
                cond: condition,
                dest: self.instructions.current_index(),
            },
        );

        self.translate_scope(
            op_index,
            if_branch,
            op_output,
            Scope::IfBranch(op_index),
        )?;

        self.instructions.update(
            jump_to_branch_end_index,
            Instruction::ConditionalJump {
                cond: true.into(),
                dest: self.instructions.current_index(),
            },
        );

        self.index_tracking.merge_conditional_branches(cached);

        self.index_tracking.define_contents(op_index, op_output);

        Ok(())
    }
}
