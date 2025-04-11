use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap, HashSet},
};

use itertools::{Either, Itertools as _};

use crate::{
    bytecode::expr::Scope, Error, ExprKind, OpIndex, RuntimePrimValue,
    SymbolicValue,
};

use super::{
    virtual_machine::{FunctionIndex, StackIndex, VirtualMachineBuilder},
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

    next_free_index: &'a mut usize,
    dead_indices: &'a mut BinaryHeap<Reverse<usize>>,
    native_functions: &'a [ExposedNativeFunction],
    native_function_lookup: &'a HashMap<OpIndex, FunctionIndex>,

    reserved_outputs: &'a mut HashMap<OpIndex, StackIndex>,

    currently_stored: &'a mut HashMap<OpIndex, VMArg>,

    previously_consumed: HashMap<OpIndex, OpIndex>,

    show_steps: bool,
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

        let mut reserved_outputs = HashMap::<OpIndex, StackIndex>::new();
        let mut next_free_index = 0;
        match output {
            SymbolicValue::Result(op_index) => match &self[op_index].kind {
                ExprKind::Tuple(elements) => {
                    Either::Left(elements.iter().cloned())
                }
                _ => Either::Right(Some(output).into_iter()),
            },
            _ => Either::Right(Some(output).into_iter()),
        }
        .enumerate()
        .for_each(|(i, value)| {
            let stack_index = StackIndex(i);
            next_free_index += 1;
            match value {
                SymbolicValue::Result(op_index) => {
                    reserved_outputs.insert(op_index, stack_index);
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

        let scope = self.operation_scope();

        let last_usage = self.last_usage();

        let mut dead_indices = BinaryHeap::new();
        let mut currently_stored = HashMap::new();

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

        let mut translater = ExpressionTranslator {
            graph: self,
            instructions: &mut builder,
            instructions_by_scope: &operations_by_scope,
            last_usage: &last_usage,
            next_free_index: &mut next_free_index,
            dead_indices: &mut dead_indices,
            native_functions: &native_functions,
            native_function_lookup: &native_function_lookup,
            reserved_outputs: &mut reserved_outputs,
            currently_stored: &mut currently_stored,
            previously_consumed: HashMap::new(),
            show_steps,
        };
        translater.translate(iter_op_indices)?;

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

        let operation_to_scope = self.operation_scope();

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
        match value {
            &SymbolicValue::Bool(value) => {
                Ok(VMArg::Const(RuntimePrimValue::Bool(value)))
            }
            &SymbolicValue::Int(value) => {
                Ok(VMArg::Const(RuntimePrimValue::NativeUInt(value)))
            }
            &SymbolicValue::Ptr(ptr) => {
                Ok(VMArg::Const(RuntimePrimValue::Ptr(ptr)))
            }
            SymbolicValue::Result(op_index)
                if self.previously_consumed.contains_key(&op_index) =>
            {
                Err(Error::AttemptedUseOfConsumedValue)
            }
            SymbolicValue::Result(op_index) => Ok(self
                .currently_stored
                .get(&op_index)
                .unwrap_or_else(|| {
                    panic!(
                        "Internal error, \
                         expression {usage} ({}) attempted to use \
                         {op_index} ({}), \
                         but {op_index} was not previously translated.",
                        self.graph[usage].kind, self.graph[*op_index].kind,
                    )
                })
                .clone()),
        }
    }

    fn alloc_index(&mut self) -> StackIndex {
        self.dead_indices
            .pop()
            .map(|Reverse(i)| StackIndex(i))
            .unwrap_or_else(|| {
                let index = *self.next_free_index;
                *self.next_free_index += 1;
                StackIndex(index)
            })
    }

    fn free_index(&mut self, stack_index: StackIndex) {
        let StackIndex(index) = stack_index;
        self.dead_indices.push(Reverse(index));
    }

    fn free_dead_indices(&mut self, op_index: OpIndex) {
        let first_index = self.last_usage.partition_point(|last_usage| {
            last_usage.usage_point.0 < op_index.0
        });
        self.last_usage[first_index..]
            .iter()
            .take_while(|last_usage| last_usage.usage_point == op_index)
            .for_each(|last_usage| {
                let Some(old_value) =
                    self.currently_stored.remove(&last_usage.expr_used)
                else {
                    panic!(
                        "Internal error: \
                         Last usage of {} occurred after {}, \
                         (expr '{}' used in '{}') \
                         but {} was not actually defined.  \
                         Currently, [{}] are defined.",
                        last_usage.expr_used,
                        last_usage.usage_point,
                        self.graph[last_usage.expr_used].kind,
                        self.graph[last_usage.usage_point].kind,
                        last_usage.expr_used,
                        self.currently_stored
                            .iter()
                            .map(|(op_index, _)| *op_index)
                            .sorted()
                            .map(|op_index| format!("{op_index}"))
                            .join(", "),
                    )
                };

                if let VMArg::SavedValue(StackIndex(old_index)) = old_value {
                    self.dead_indices.push(Reverse(old_index));
                }
            });
    }

    fn get_output_index(&mut self, op_index: OpIndex) -> StackIndex {
        self.reserved_outputs
            .get(&op_index)
            .cloned()
            .unwrap_or_else(|| self.alloc_index())
    }

    fn translate(
        &mut self,
        instructions: impl Iterator<Item = OpIndex>,
    ) -> Result<(), Error> {
        for op_index in instructions {
            let op = &self.graph[op_index];
            let expr_name = LocalIndexPrinter::new(op_index, self.graph);

            macro_rules! push_annotated {
                ($inst:expr $(,)?) => {{
                    self.instructions.push($inst)
                }};

                ($inst:expr, $annot:expr $(,)?) => {{
                    let inst = self.instructions.push($inst);
                    if self.show_steps {
                        self.instructions.annotate(inst, $annot);
                    }
                    inst
                }};
            }

            macro_rules! handle_binary_op {
                ($variant:ident, $lhs:expr, $rhs:expr) => {{
                    let lhs = self.value_to_arg(op_index, $lhs)?;
                    let rhs = self.value_to_arg(op_index, $rhs)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::$variant {
                            lhs,
                            rhs,
                            output: op_output,
                        },
                        format!("evaluate {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }};
            }

            macro_rules! handle_scope {
                ($value:expr,
                 $stack_index:expr,
                 $scope:expr $(,)?
                ) => {{
                    if let &SymbolicValue::Result(scope_output) = $value {
                        if !self.currently_stored.contains_key(&scope_output) {
                            self.reserved_outputs
                                .insert(scope_output, $stack_index);
                            let iter_scope = self
                                .instructions_by_scope
                                .get(&$scope)
                                .into_iter()
                                .flatten()
                                .cloned()
                                .filter(|&op| {
                                    !matches!(
                                        self.graph[op].kind,
                                        ExprKind::Function { .. },
                                    )
                                });
                            let iter_scope: Box<dyn Iterator<Item = OpIndex>> =
                                Box::new(iter_scope);

                            self.translate(iter_scope)?;
                            self.reserved_outputs.remove(&scope_output);
                        }
                    }
                    match self.value_to_arg(op_index, $value)? {
                        VMArg::Const(value) => {
                            push_annotated!(
                                Instruction::Copy {
                                    value: value.into(),
                                    output: $stack_index,
                                },
                                format!("copy output of {expr_name}"),
                            );
                        }
                        VMArg::SavedValue(body_output) => {
                            if body_output != $stack_index {
                                push_annotated!(
                                    Instruction::Swap(
                                        body_output,
                                        $stack_index,
                                    ),
                                    format!("move output of {expr_name}"),
                                );
                            }
                        }
                    }
                }};
            }

            match op.as_ref() {
                ExprKind::Function { .. } => {
                    unreachable!(
                        "Function calls should be inlined, \
                         and should only be encountered in SimpleReduce.  \
                         But at index {op_index}, encountered function named {:?}",
                        op.name
                    )
                }
                ExprKind::FunctionArg(_) => {
                    assert!(self.currently_stored.contains_key(&op_index));
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
                    let Some(func) = func.as_op_index() else {
                        panic!("Internal error, callee must be function")
                    };

                    if let Some(&native_func_index) =
                        self.native_function_lookup.get(&func)
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
                            // However, the `currently_stored` lookup
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

                            push_annotated!(
                                Instruction::NativeFunctionCall {
                                    index: native_func_index,
                                    args: vm_args,
                                    output: None,
                                },
                                format!("produce {expr_name}"),
                            );

                            self.currently_stored.remove(&first_arg_op);
                            self.previously_consumed
                                .insert(first_arg_op, op_index);

                            if let Some(&required_output) =
                                self.reserved_outputs.get(&op_index)
                            {
                                if first_arg_loc != required_output {
                                    push_annotated!(
                                        Instruction::Swap(first_arg_loc, required_output),
                                        format!(
                                            "swap {expr_name} \
                                             to mandatory output loc {required_output}"
                                        ),
                                    );
                                }
                                self.currently_stored
                                    .insert(op_index, required_output.into());
                            } else {
                                self.currently_stored
                                    .insert(op_index, first_arg_loc.into());
                            }
                        } else {
                            // The function produces an output value, to
                            // be stored in the output index.
                            let op_output = self.get_output_index(op_index);
                            push_annotated!(
                                Instruction::NativeFunctionCall {
                                    index: native_func_index,
                                    args: vm_args,
                                    output: Some(op_output),
                                },
                                format!("produce {expr_name}"),
                            );
                            self.currently_stored
                                .insert(op_index, op_output.into());
                        }
                    } else {
                        todo!(
                            "Handle IR-defined function calls in the VM.  \
                             Until then, all functions should be inlined."
                        )
                    }
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
                    let initial = self.value_to_arg(op_index, initial)?;
                    let extent = self.value_to_arg(op_index, extent)?;

                    let op_output = self.get_output_index(op_index);

                    match initial {
                        VMArg::Const(_) => {
                            push_annotated!(
                                Instruction::Copy {
                                    value: initial,
                                    output: op_output,
                                },
                                format!(
                                    "copy initial value \
                                     of reduction {expr_name}"
                                ),
                            );
                        }
                        VMArg::SavedValue(stack_index) => {
                            if stack_index != op_output {
                                push_annotated!(
                                    Instruction::Swap(stack_index, op_output),
                                    format!(
                                        "move initial value \
                                         of reduction {expr_name}"
                                    ),
                                );
                            }
                        }
                    }

                    let extent_is_none = self.alloc_index();
                    push_annotated!(
                        Instruction::IsSome {
                            value: extent,
                            output: extent_is_none
                        },
                        format!("extent {extent} is some"),
                    );
                    push_annotated!(
                        Instruction::Not {
                            arg: extent_is_none.into(),
                            output: extent_is_none
                        },
                        format!("extent {extent} is none"),
                    );
                    let extent_is_zero = self.alloc_index();
                    push_annotated!(
                        Instruction::Equal {
                            lhs: extent,
                            rhs: 0usize.into(),
                            output: extent_is_zero
                        },
                        format!("extent {extent} is zero"),
                    );

                    self.free_index(extent_is_none);
                    self.free_index(extent_is_zero);
                    let can_skip_loop = self.alloc_index();
                    push_annotated!(
                        Instruction::Or {
                            lhs: extent_is_none.into(),
                            rhs: extent_is_zero.into(),
                            output: can_skip_loop
                        },
                        format!("loop over {extent} can be skipped"),
                    );

                    // Placeholder for the conditional jump to break
                    // out of the loop.  To be updated after the loop
                    // body is generated, when we know the destination
                    // index of the jump.
                    let jump_to_end_instruction_index = push_annotated!(
                        Instruction::NoOp,
                        format!("loop for {expr_name} terminated"),
                    );
                    self.free_index(can_skip_loop);

                    let loop_iter = self.alloc_index();
                    push_annotated!(
                        Instruction::Copy {
                            value: 0usize.into(),
                            output: loop_iter,
                        },
                        format!("initialize loop iterator for {expr_name}"),
                    );

                    let loop_start = self.instructions.current_index();

                    let reduction_index = match reduction {
                        &SymbolicValue::Result(op_index) => op_index,
                        _ => todo!(
                            "Better error message \
                             when SimpleReduce points to non-function"
                        ),
                    };

                    match &self.graph[reduction_index].kind {
                        ExprKind::Function { params, output } => {
                            if params.len() != 2 {
                                todo!("Better error message for invalid reduction function");
                            }
                            let accumulator = match params[0] {
                                SymbolicValue::Result(op_index) => op_index,
                                _ => todo!("Better error message for ill-formed function"),
                            };
                            let index = match params[1] {
                                SymbolicValue::Result(op_index) => op_index,
                                _ => todo!("Better error message for ill-formed function"),
                            };

                            self.currently_stored
                                .insert(accumulator, op_output.into());
                            self.currently_stored
                                .insert(index, loop_iter.into());

                            handle_scope!(
                                output,
                                op_output,
                                Scope::Function(reduction_index),
                            );

                            self.currently_stored.remove(&accumulator);
                            self.currently_stored.remove(&index);
                        }
                        ExprKind::NativeFunction(_) => {
                            let native_func_index = self
                                .native_function_lookup
                                .get(&reduction_index)
                                .expect(
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

                            push_annotated!(
                                Instruction::NativeFunctionCall {
                                    index: *native_func_index,
                                    args: vec![
                                        op_output.into(),
                                        loop_iter.into(),
                                    ],
                                    output: func_output,
                                },
                                format!(
                                    "native call to reduce into {expr_name}"
                                ),
                            );
                        }
                        _ => todo!(
                            "Better error message \
                             when SimpleReduce points to non-function"
                        ),
                    }

                    push_annotated!(
                        Instruction::Add {
                            lhs: loop_iter.into(),
                            rhs: 1usize.into(),
                            output: loop_iter,
                        },
                        format!("increment loop iterator for {expr_name}"),
                    );

                    let loop_condition = self.alloc_index();
                    push_annotated!(
                        Instruction::LessThan {
                            lhs: loop_iter.into(),
                            rhs: extent.into(),
                            output: loop_condition,
                        },
                        format!("loop condition for {expr_name}"),
                    );

                    push_annotated!(
                        Instruction::ConditionalJump {
                            cond: loop_condition.into(),
                            dest: loop_start,
                        },
                        format!("jump to beginning of {expr_name}"),
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

                    self.currently_stored.insert(op_index, op_output.into());
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
                    push_annotated!(
                        Instruction::IsSome {
                            value,
                            output: op_output,
                        },
                        format!("evaluate {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }

                ExprKind::IfElse {
                    condition,
                    if_branch,
                    else_branch,
                } => {
                    let condition = self.value_to_arg(op_index, condition)?;

                    let op_output = self.get_output_index(op_index);
                    let mut cached_currently_stored =
                        self.currently_stored.clone();
                    let mut cached_dead_indices = self.dead_indices.clone();
                    let mut cached_previously_consumed =
                        self.previously_consumed.clone();

                    let jump_to_if_branch_index = push_annotated!(
                        Instruction::NoOp,
                        format!("jump to if branch of {expr_name}"),
                    );

                    handle_scope!(
                        else_branch,
                        op_output,
                        Scope::ElseBranch(op_index),
                    );

                    std::mem::swap(
                        self.currently_stored,
                        &mut cached_currently_stored,
                    );
                    std::mem::swap(self.dead_indices, &mut cached_dead_indices);
                    std::mem::swap(
                        &mut self.previously_consumed,
                        &mut cached_previously_consumed,
                    );

                    let jump_to_branch_end_index = push_annotated!(
                        Instruction::NoOp,
                        format!(
                            "after else branch {expr_name}, \
                             skip the if branch"
                        ),
                    );

                    self.instructions.update(
                        jump_to_if_branch_index,
                        Instruction::ConditionalJump {
                            cond: condition,
                            dest: self.instructions.current_index(),
                        },
                    );

                    handle_scope!(
                        if_branch,
                        op_output,
                        Scope::IfBranch(op_index),
                    );

                    self.instructions.update(
                        jump_to_branch_end_index,
                        Instruction::ConditionalJump {
                            cond: true.into(),
                            dest: self.instructions.current_index(),
                        },
                    );

                    // After the conditional, dead indices marked as dead
                    // while following either branch are considered
                    // dead.
                    *self.dead_indices = cached_dead_indices
                        .into_iter()
                        .chain(self.dead_indices.iter().cloned())
                        .unique()
                        .collect();

                    // Likewise, only values that are currently stored
                    // at the end of both branches have a known
                    // storage location after the conditional.
                    *self.currently_stored = cached_currently_stored
                        .into_iter()
                        .filter(|(op_index, _)| {
                            self.currently_stored.contains_key(op_index)
                        })
                        .collect();

                    self.currently_stored.insert(op_index, op_output.into());
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
                    push_annotated!(
                        Instruction::PrimCast {
                            value,
                            prim_type: *prim_type,
                            output: op_output,
                        },
                        format!("eval {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::PhysicalDowncast { obj, ty } => {
                    let obj = self.value_to_arg(op_index, obj)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::Downcast {
                            obj,
                            subtype: *ty,
                            output: op_output,
                        },
                        format!("eval {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::ReadValue { ptr, prim_type } => {
                    let ptr = self.value_to_arg(op_index, ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::Read {
                            ptr,
                            prim_type: *prim_type,
                            output: op_output,
                        },
                        format!("eval {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::ReadString { ptr } => {
                    let ptr = self.value_to_arg(op_index, ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::ReadString {
                            ptr,
                            output: op_output,
                        },
                        format!("eval {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::PointerCast { ptr, .. } => {
                    let ptr = self.value_to_arg(op_index, ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::Copy {
                            value: ptr,
                            output: op_output,
                        },
                        format!("eval {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
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
}
