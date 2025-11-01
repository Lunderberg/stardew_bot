use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use itertools::{Either, Itertools as _};

use dsl_analysis::{Analysis, TypeInferenceError};
use dsl_ir::{DSLType, ExprKind, OpIndex, Scope, SymbolicGraph, SymbolicValue};
use dsl_vm::{
    AnnotationLocation, FunctionIndex, Instruction, InstructionIndex,
    StackIndex, StaticIndex, VMArg, VMByteRange, VirtualMachine,
    VirtualMachineBuilder,
};

use crate::{
    index_tracking::AllocIndexDebug, Error, GetOutputDebug, IndexTracking,
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

#[derive(Debug)]
struct LastUsageLookup {
    elements: Vec<LastUsage>,
}

struct LastUsageLookupPrinter<'a> {
    lookup: &'a LastUsageLookup,
    graph: &'a SymbolicGraph,
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
    builder: &'a mut VirtualMachineBuilder,

    operations_by_scope: &'a HashMap<Scope, Vec<OpIndex>>,

    /// Indicates the last time that an expression is used.
    last_usage: &'a LastUsageLookup,

    native_function_lookup: &'a HashMap<OpIndex, FunctionIndex>,

    index_tracking: IndexTracking,

    lazy_static_tracking: HashMap<OpIndex, LazyStaticTracking>,

    analysis: Analysis<'a>,

    show_steps: bool,
}

#[derive(Clone)]
struct LazyStaticTracking {
    lazy_static: OpIndex,
    func: OpIndex,
    value: SymbolicValue,
    loc: StackIndex,
    static_loc: StaticIndex,
}

pub trait SymbolicGraphToVirtualMachine {
    fn to_virtual_machine(
        &self,
        show_steps: bool,
    ) -> Result<VirtualMachine, Error>;
}
impl SymbolicGraphToVirtualMachine for SymbolicGraph {
    fn to_virtual_machine(
        &self,
        show_steps: bool,
    ) -> Result<VirtualMachine, Error> {
        let mut builder = VirtualMachine::builder();

        let native_function_lookup: HashMap<OpIndex, FunctionIndex> = self
            .iter_ops()
            .filter_map(|(op_index, op)| match &op.kind {
                ExprKind::NativeFunction(func) => Some((op_index, func)),
                _ => None,
            })
            .map(|(op_index, func)| {
                let func_index = builder.push_raw_native_function(func.clone());
                (op_index, func_index)
            })
            .collect();

        self.iter_extern_funcs().try_for_each(|func_index| {
            self.func_to_virtual_machine(
                &mut builder,
                func_index,
                &native_function_lookup,
                show_steps,
            )
        })?;

        Ok(builder.build())
    }
}

trait LocalSymbolicGraphExt {
    fn func_to_virtual_machine(
        &self,
        builder: &mut VirtualMachineBuilder,
        main_func_index: OpIndex,
        native_function_lookup: &HashMap<OpIndex, FunctionIndex>,
        show_steps: bool,
    ) -> Result<(), Error>;

    fn analyze_scopes(&self, reachable: &[bool]) -> Vec<ScopeInfo>;

    fn last_usage(&self, reachable: &[bool]) -> Vec<LastUsage>;
}
impl LocalSymbolicGraphExt for SymbolicGraph {
    fn func_to_virtual_machine(
        &self,
        builder: &mut VirtualMachineBuilder,
        main_func_index: OpIndex,
        native_function_lookup: &HashMap<OpIndex, FunctionIndex>,
        show_steps: bool,
    ) -> Result<(), Error> {
        let reachable = self.reachable(Some(main_func_index));
        let last_usage = LastUsageLookup {
            elements: self.last_usage(&reachable),
        };
        if show_steps {
            println!("{}", last_usage.print(self));
        }

        let scope = self.operation_scope(&reachable);

        let operations_by_scope = scope
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, scope)| (scope, OpIndex::new(i)))
            .into_group_map();

        let main_func = &self[main_func_index];

        let Some(main_func_name) = &main_func.name else {
            unreachable!(
                "Should already be checked in \
                 SymbolicGraph::mark_extern_func()"
            )
        };

        let ExprKind::Function {
            params,
            output: output_value,
        } = &main_func.kind
        else {
            panic!(
                "Internal error, \
                 `extern_funcs` should only point to functions."
            )
        };
        let output_value = *output_value;

        builder.mark_entry_point(main_func_name, params.len())?;

        let mut index_tracking = IndexTracking::default();
        for (i, param) in params.iter().enumerate() {
            let param = param.as_op_index().unwrap();

            let (index, _) = index_tracking.alloc_index();
            assert_eq!(index.0, i);
            index_tracking.define_contents(param, index);
        }

        let iter_op_indices = self
            .iter_ops()
            .filter(|(_, expr)| !matches!(expr.kind, ExprKind::Function { .. }))
            .filter(|(OpIndex(i), _)| reachable[*i])
            .filter(|(OpIndex(i), _)| match scope[*i] {
                Scope::Global => true,
                Scope::Function(func_index) => func_index == main_func_index,
                _ => false,
            })
            .map(|(op_index, _)| op_index);

        let mut translator = ExpressionTranslator {
            graph: self,
            builder,
            operations_by_scope: &operations_by_scope,
            last_usage: &last_usage,
            native_function_lookup,
            show_steps,
            analysis: Analysis::new(None),
            index_tracking,
            lazy_static_tracking: HashMap::default(),
        };
        translator
            .annotate(|_| format!("Start of function '{main_func_name}'"));

        translator.translate(iter_op_indices)?;

        let iter_outputs = || match output_value {
            SymbolicValue::Result(op_index) => match &self[op_index].kind {
                ExprKind::Tuple(elements) => {
                    Either::Left(elements.iter().cloned())
                }
                _ => Either::Right(Some(output_value).into_iter()),
            },
            _ => Either::Right(Some(output_value).into_iter()),
        };

        let outputs = iter_outputs()
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
            .collect::<Result<_, _>>()?;

        let return_instruction = Instruction::Return { outputs };
        translator.push_annotated(return_instruction, || {
            "return from top-level function".to_string()
        });

        params
            .iter()
            .cloned()
            .chain(iter_outputs())
            .filter_map(|value| value.as_op_index())
            .for_each(|index| {
                translator.index_tracking.release_expr(index);
            });

        translator.index_tracking.assert_empty();

        translator.annotate(|_| format!("End of function '{main_func_name}'"));

        Ok(())
    }

    fn analyze_scopes(&self, reachable: &[bool]) -> Vec<ScopeInfo> {
        let operation_to_scope = self.operation_scope(reachable);

        let iter_scopes = self
            .iter_ops()
            .zip(reachable)
            .filter(|(_, op_reachable)| **op_reachable)
            .flat_map(|((index, op), _)| {
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

        // Step 1: Collect the body of each scope.
        operation_to_scope
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, scope)| (OpIndex::new(i), scope))
            .zip(reachable)
            .filter(|(_, op_reachable)| **op_reachable)
            .map(|(op_scope, _)| op_scope)
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
            .zip(reachable)
            .filter(|(_, op_reachable)| **op_reachable)
            .map(|(op_with_index, _)| op_with_index)
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
                    scope_lookup.get(scope).map(|info| info.parent_scope)
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

    fn last_usage(&self, reachable: &[bool]) -> Vec<LastUsage> {
        assert_eq!(self.num_operations(), reachable.len());

        let scope_info_lookup = self
            .analyze_scopes(reachable)
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

        let reduction_accumulators: HashSet<OpIndex> = self
            .iter_ops()
            .filter_map(|(_, expr)| match &expr.kind {
                ExprKind::SimpleReduce { reduction, .. } => Some(reduction),
                _ => None,
            })
            .filter_map(|reduction_value| reduction_value.as_op_index())
            .map(|reduction_index| &self[reduction_index])
            .filter_map(|reduction| match &reduction.kind {
                ExprKind::Function { params, .. } => params.first(),
                _ => None,
            })
            .filter_map(|accumulator| accumulator.as_op_index())
            .collect();

        last_usage
            .into_iter()
            .filter(|last_usage| match &self[last_usage.expr_used].kind {
                ExprKind::Function { .. } => false,
                ExprKind::FunctionArg(_) => {
                    reduction_accumulators.contains(&last_usage.expr_used)
                }
                ExprKind::NativeFunction(_) => false,
                _ => true,
            })
            .sorted_by_key(|last_usage| {
                (last_usage.usage_point, last_usage.expr_used)
            })
            .collect()
    }
}

impl LastUsageLookup {
    fn iter_dead_indices(
        &self,
        op_index: OpIndex,
    ) -> impl Iterator<Item = OpIndex> + '_ {
        let first_index = self.elements.partition_point(|last_usage| {
            last_usage.usage_point.0 < op_index.0
        });
        self.elements[first_index..]
            .iter()
            .take_while(move |last_usage| last_usage.usage_point == op_index)
            .map(|last_usage| last_usage.expr_used)
    }

    fn contains(&self, op_index: OpIndex, usage: OpIndex) -> bool {
        self.iter_dead_indices(op_index).contains(&usage)
    }

    fn print<'a>(
        &'a self,
        graph: &'a SymbolicGraph,
    ) -> LastUsageLookupPrinter<'a> {
        LastUsageLookupPrinter {
            lookup: self,
            graph,
        }
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
                            if encountered.contains(index)
                                && !encountered_else.contains(index)
                            {
                                last_usage.push(LastUsage {
                                    usage_point: start_of_else,
                                    expr_used: *index,
                                })
                            }
                        }
                    }

                    if let Some(start_of_if) =
                        self.iter_scope(Scope::IfBranch(visiting)).next()
                    {
                        for index in &encountered_else {
                            if encountered.contains(index)
                                && !encountered_if.contains(index)
                            {
                                last_usage.push(LastUsage {
                                    usage_point: start_of_if,
                                    expr_used: *index,
                                });
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

                other => other
                    .iter_input_values()
                    .for_each(|value| mark_value!(encountered, value)),
            }
        }
    }
}

impl ExpressionTranslator<'_> {
    fn value_to_arg(
        &mut self,
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

        if let Some(lazy_static) = self.lazy_static_tracking.get(&op_index) {
            let lazy_static = lazy_static.clone();
            let name = lazy_static.lazy_static.pprint(self.graph);
            let condition = self.alloc_index();
            self.push_annotated(
                Instruction::IsSome {
                    value: lazy_static.loc.into(),
                    output: condition,
                },
                || format!("check if {name} is initialized"),
            );

            let jump_if_initialized_index = self
                .push_annotated(Instruction::NoOp, || {
                    format!("skip re-initialization of {name}")
                });

            self.free_index(condition);

            self.translate_scope(
                lazy_static.lazy_static,
                lazy_static.value,
                lazy_static.loc,
                Scope::Function(lazy_static.func),
            )?;
            self.push_annotated(
                Instruction::StackToStatic {
                    value: lazy_static.loc.into(),
                    saved: lazy_static.static_loc,
                },
                || format!("save {name} for next execution"),
            );
            let after_init = self.builder.current_index();
            self.builder.update(
                jump_if_initialized_index,
                Instruction::ConditionalJump {
                    cond: condition.into(),
                    dest: after_init,
                },
            );
            return Ok(lazy_static.loc.into());
        }

        let stack_index = self
            .index_tracking
            .expr_to_location(op_index)?
            .unwrap_or_else(|| {
                let current_name = usage.pprint(self.graph);
                let previous_name = op_index.pprint(self.graph);
                panic!(
                    "Internal error, \
                     expression {current_name} ({}) \
                     attempted to use \
                     {previous_name} ({}), \
                     but {previous_name} was not previously translated.",
                    self.graph[usage].kind, self.graph[op_index].kind,
                )
            });

        Ok(stack_index.into())
    }

    fn annotate<Func, Annot>(&mut self, generate_annotation: Func)
    where
        Func: FnOnce(&SymbolicGraph) -> Annot,
        Annot: Display,
    {
        if self.show_steps {
            let index = self.builder.current_index();
            let loc = AnnotationLocation::Before(index);
            let annotation = generate_annotation(self.graph);
            self.builder.annotate(loc, annotation);
        }
    }

    fn reserve_index(&mut self, op_index: OpIndex, stack_index: StackIndex) {
        self.annotate(|graph| {
            format!("Reserving {stack_index} for {}", op_index.pprint(graph))
        });
        self.index_tracking.reserve_index(op_index, stack_index);
    }

    fn release_reservation(&mut self, op_index: OpIndex) {
        let stack_index = self.index_tracking.release_reservation(op_index);

        self.annotate(|graph| {
            format!(
                "Releasing reservation of {stack_index} for {}",
                op_index.pprint(graph)
            )
        });
    }

    fn alloc_index(&mut self) -> StackIndex {
        let (index, alloc_debug) = self.index_tracking.alloc_index();
        self.annotate(|_| match alloc_debug {
            AllocIndexDebug::NewIndex => {
                format!("No dead indices, using new index {index}")
            }
            AllocIndexDebug::PreviouslyUsedForUnknown => {
                "Reusing dead index {index}, \
                 which previously held a value \
                 that doesn't correspond to an OpIndex."
                    .to_string()
            }
            AllocIndexDebug::PreviouslyUsedFor(prev_index) => format!(
                "Reusing dead index {index}, \
                 which previously held {prev_index}.",
            ),
        });
        index
    }

    fn get_output_index(&mut self, op_index: OpIndex) -> StackIndex {
        let (index, debug) = self.index_tracking.get_output_index(op_index);
        self.annotate(|graph| {
            let expr = op_index.pprint(graph);
            match debug {
                GetOutputDebug::FromReserved => {
                    format!("For op {expr}, using reserved output {index}")
                }
                GetOutputDebug::FromAlloc(AllocIndexDebug::NewIndex) => {
                    format!("For op {expr}, allocated new output {index}")
                }
                GetOutputDebug::FromAlloc(
                    AllocIndexDebug::PreviouslyUsedForUnknown,
                ) => format!(
                    "For op {expr}, re-using dead index {index}, \
                              which previously held an unknown value"
                ),
                GetOutputDebug::FromAlloc(
                    AllocIndexDebug::PreviouslyUsedFor(prev_index),
                ) => format!(
                    "For op {expr}, re-using dead index {index}, \
                              which previously held {prev_index}"
                ),
            }
        });
        index
    }

    fn free_index(&mut self, stack_index: StackIndex) {
        self.annotate(|_| {
            format!(
                "Location {stack_index} no longer needed, \
                 marking as dead."
            )
        });
        self.index_tracking.free_index(stack_index);
    }

    fn free_dead_indices(&mut self, op_index: OpIndex) {
        self.last_usage
            .iter_dead_indices(op_index)
            .for_each(|expr_used| {
                let opt_stack_index =
                    self.index_tracking.release_expr(expr_used);

                if let Some(stack_index) = opt_stack_index {
                    self.annotate(|graph| {
                        format!(
                            "After {}, {} is no longer needed.  \
                             Marking {stack_index} as dead.",
                            op_index.pprint(graph),
                            expr_used.pprint(graph),
                        )
                    });
                }
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
        let index = self.builder.push(inst);
        if self.show_steps {
            self.builder.annotate(index, generate_annotation());
        }
        index
    }

    fn translate(
        &mut self,
        instructions: impl Iterator<Item = OpIndex>,
    ) -> Result<(), Error> {
        for op_index in instructions {
            let op = &self.graph[op_index];
            let expr_name = op_index.pprint(self.graph);

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

            match &op.kind {
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
                        .expr_to_location(op_index)?
                        .is_some());
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
                | ExprKind::Chain { .. }
                | ExprKind::First { .. }
                | ExprKind::Find { .. }
                | ExprKind::FindMap { .. }
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

                ExprKind::BitwiseAnd { lhs, rhs } => {
                    handle_binary_op!(BitwiseAnd, lhs, rhs)
                }
                ExprKind::BitwiseOr { lhs, rhs } => {
                    handle_binary_op!(BitwiseOr, lhs, rhs)
                }

                ExprKind::Add { lhs, rhs } => handle_binary_op!(Add, lhs, rhs),
                ExprKind::Sub { lhs, rhs } => handle_binary_op!(Sub, lhs, rhs),
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
                ExprKind::IsSubclassOf {
                    child_method_table_ptr: child,
                    parent_method_table_ptr: parent,
                } => {
                    let child = self.value_to_arg(op_index, child)?;
                    let parent = self.value_to_arg(op_index, parent)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::IsSubclassOf {
                            child_method_table_ptr: child,
                            parent_method_table_ptr: parent,
                            output: op_output,
                        },
                        || format!("eval {expr_name}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }

                ExprKind::ReadBytes(regions) => {
                    let regions = regions
                        .iter()
                        .map(|region| {
                            let ptr =
                                self.value_to_arg(op_index, &region.ptr)?;
                            let num_bytes =
                                self.value_to_arg(op_index, &region.num_bytes)?;
                            Ok(VMByteRange { ptr, num_bytes })
                        })
                        .collect::<Result<_, Error>>()?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::ReadBytes {
                            regions,
                            output: op_output,
                        },
                        || format!("eval {expr_name}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }
                ExprKind::CastBytes {
                    bytes,
                    offset,
                    prim_type,
                } => {
                    let bytes = self.value_to_arg(op_index, bytes)?;
                    let offset = self.value_to_arg(op_index, offset)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::CastBytes {
                            bytes,
                            offset,
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

                ExprKind::TypeToMethodTable { ty } => {
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::TypeToMethodTable {
                            ty: ty.clone(),
                            output: op_output,
                        },
                        || format!("find method table for '{ty}'"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }

                ExprKind::FieldOffset {
                    method_table_ptr,
                    field,
                } => {
                    let method_table_ptr =
                        self.value_to_arg(op_index, method_table_ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::FieldOffset {
                            method_table: method_table_ptr,
                            field: field.clone(),
                            output: op_output,
                        },
                        || format!("offset of field '{field}'"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }

                ExprKind::ArrayStride { method_table_ptr } => {
                    let method_table_ptr =
                        self.value_to_arg(op_index, method_table_ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::ArrayStride {
                            method_table: method_table_ptr,
                            output: op_output,
                        },
                        || format!("array stride"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }

                ExprKind::LazyStatic { init_func } => {
                    let init_func = match init_func {
                        SymbolicValue::Result(index) => Ok(*index),
                        SymbolicValue::Const(prim) => {
                            Err(Error::LazyStaticInitializationMustBeFunction(
                                format!("{}", prim.runtime_type()),
                            ))
                        }
                    }?;

                    let output = match &self.graph[init_func].kind {
                        ExprKind::Function { params, .. }
                            if !params.is_empty() =>
                        {
                            Err(
                                Error::LazyStaticInitializationMayNotHaveParams(
                                    params.len(),
                                ),
                            )
                        }
                        ExprKind::Function { output, .. } => Ok(*output),
                        other => {
                            Err(Error::LazyStaticInitializationMustBeFunction(
                                other.op_name().to_string(),
                            ))
                        }
                    }?;

                    let op_output = self.get_output_index(op_index);
                    let static_loc =
                        StaticIndex(self.lazy_static_tracking.len());

                    self.push_annotated(
                        Instruction::StaticToStack { saved: static_loc, output: op_output  },
                        || format!("load previous execution's value of {expr_name}"),
                    );
                    self.lazy_static_tracking.insert(
                        op_index,
                        LazyStaticTracking {
                            lazy_static: op_index,
                            func: init_func,
                            value: output,
                            loc: op_output,
                            static_loc,
                        },
                    );
                }

                ExprKind::ReadPrim { ptr, prim_type } => {
                    let ptr = self.value_to_arg(op_index, ptr)?;
                    self.free_dead_indices(op_index);
                    let bytes = self.alloc_index();
                    let region = VMByteRange {
                        ptr,
                        num_bytes: prim_type.size_bytes().into(),
                    };
                    self.push_annotated(
                        Instruction::ReadBytes {
                            regions: vec![region],
                            output: bytes,
                        },
                        || format!("read bytes for {prim_type}"),
                    );

                    self.free_index(bytes);
                    let op_output = self.get_output_index(op_index);
                    self.push_annotated(
                        Instruction::CastBytes {
                            bytes: bytes.into(),
                            offset: 0usize.into(),
                            prim_type: *prim_type,
                            output: op_output,
                        },
                        || format!("cast bytes to {prim_type}"),
                    );
                    self.index_tracking.define_contents(op_index, op_output);
                }

                // read_prim @ ExprKind::ReadPrim { .. } => {
                //     return Err(Error::ReadPrimOperatorRequiresLowering(
                //         read_prim.clone(),
                //     ));
                // }
                boolean @ (ExprKind::And { .. } | ExprKind::Or { .. }) => {
                    return Err(Error::BooleanOperatorRequiresLowering(
                        boolean.clone(),
                    ));
                }

                ExprKind::StaticField(_)
                | ExprKind::FieldAccess { .. }
                | ExprKind::ObjectMethodTable { .. }
                | ExprKind::SymbolicDowncast { .. }
                | ExprKind::IndexAccess { .. }
                | ExprKind::NumArrayElements { .. }
                | ExprKind::ArrayExtent { .. } => {
                    return Err(Error::SymbolicExpressionRequiresLowering(
                        format!("{}", self.graph.print(op_index.into())),
                    ));
                }
            }
        }

        Ok(())
    }

    /// Translate a scoped expression
    ///
    /// * `op_index`: The index of the expression that requires the
    ///   scoped.  (e.g. A `FunctionCall`, `IfElse`, or `SimpleReduce`
    ///   expression)
    ///
    /// * `value`: The value that should be produced as the output of
    ///   the scope.
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
        let expr_name = op_index.pprint(self.graph);

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

        if let Some(currently_at) =
            self.index_tracking.expr_to_location(scope_output)?
        {
            if currently_at == out_stack_index {
                // Already in the desired location, no action needed
            } else if matches!(
                self.analysis.infer_type(self.graph, value)?,
                DSLType::Prim(_)
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
            } else if self.last_usage.contains(op_index, scope_output) {
                // A rust-native object may be moved to the output
                // location, if this is the last usage of the
                // rust-native object in its current state.
                self.push_annotated(
                    Instruction::Swap(out_stack_index, currently_at),
                    || format!("copy value to output of {expr_name}"),
                );
            } else if value
                .as_op_index()
                .map(|index| matches!(self.graph[index].kind, ExprKind::None))
                .unwrap_or(false)
            {
                // A 'None' value can be generated
                self.push_annotated(
                    Instruction::Clear {
                        loc: out_stack_index,
                    },
                    || format!("writing None to output of {expr_name}"),
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
                .operations_by_scope
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
            let Some(body_output) = self.index_tracking.drop_expr(scope_output)
            else {
                panic!("Output of {scope:?} was not produced")
            };
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
        let expr_name = op_index.pprint(self.graph);

        let Some(func) = func.as_op_index() else {
            panic!("Internal error, callee must be function")
        };

        match &self.graph[func].kind {
            ExprKind::NativeFunction(native_func) => {
                let native_func_index = *self
                    .native_function_lookup
                    .get(&func)
                    .expect("All functions should already be checked");

                let vm_args: Vec<_> = args
                    .iter()
                    .map(|arg| self.value_to_arg(op_index, arg))
                    .collect::<Result<_, _>>()?;

                let mutates_first_argument =
                    native_func.mutates_first_argument();

                if mutates_first_argument {
                    // The function mutates its input
                    // argument.  No output index is required.
                    // However, the `current_location` lookup
                    // should be updated to no longer contain
                    // the first argument.
                    let first_arg_op = match &args[0] {
                        SymbolicValue::Result(op_index) => *op_index,
                        SymbolicValue::Const(_) => todo!(
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
                    self.index_tracking.consume_expr(first_arg_op, op_index);

                    if let Some(required_output) =
                        self.index_tracking.expr_to_reserved_location(op_index)
                    {
                        if first_arg_loc != required_output {
                            self.push_annotated(
                                Instruction::Swap(
                                    first_arg_loc,
                                    required_output,
                                ),
                                || {
                                    format!(
                                        "swap {expr_name} \
                                     to mandatory output loc {required_output}"
                                    )
                                },
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
            }
            ExprKind::Function { .. } => todo!(
                "Handle IR-defined function calls in the VM.  \
                 Until then, all functions should be inlined."
            ),
            _ => {
                return Err(
                    TypeInferenceError::AttemptedCallOnNonFunction.into()
                );
            }
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
        let expr_name = op_index.pprint(self.graph);

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

        let loop_start = self.builder.current_index();

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

                // self.index_tracking.current_location.remove(&accumulator);
                //self.index_tracking.current_location.remove(&index);
                self.index_tracking.drop_expr(accumulator);
                self.index_tracking.drop_expr(index);
                // self.index_tracking.release_expr(accumulator);
                // self.index_tracking.release_expr(index);
            }
            ExprKind::NativeFunction(native_func) => {
                let native_func_index =
                    self.native_function_lookup.get(&reduction_index).expect(
                        "Internal error, \
                         function should have already been encountered.",
                    );
                let mutates_first_argument =
                    native_func.mutates_first_argument();

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
                rhs: extent,
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
        self.free_index(loop_iter);

        let after_loop = self.builder.current_index();
        self.builder.update(
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
        let expr_name = op_index.pprint(self.graph);

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

        self.builder.update(
            jump_to_if_branch_index,
            Instruction::ConditionalJump {
                cond: condition,
                dest: self.builder.current_index(),
            },
        );

        self.translate_scope(
            op_index,
            if_branch,
            op_output,
            Scope::IfBranch(op_index),
        )?;

        self.builder.update(
            jump_to_branch_end_index,
            Instruction::ConditionalJump {
                cond: true.into(),
                dest: self.builder.current_index(),
            },
        );

        self.index_tracking.merge_conditional_branches(cached);

        self.free_dead_indices(op_index);
        self.index_tracking.define_contents(op_index, op_output);

        Ok(())
    }
}

impl Display for LastUsageLookupPrinter<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut usage_point = None;
        for last_usage in &self.lookup.elements {
            if usage_point != Some(last_usage.usage_point) {
                write!(
                    f,
                    "\nPoint of use: {}",
                    last_usage.usage_point.pprint(self.graph)
                )?;
                usage_point = Some(last_usage.usage_point);
            }
            write!(f, "\n\tUsed: {}", last_usage.expr_used.pprint(self.graph))?;
        }
        Ok(())
    }
}
