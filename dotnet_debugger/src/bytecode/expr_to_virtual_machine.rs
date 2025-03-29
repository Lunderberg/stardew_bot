use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap, HashSet},
};

use itertools::Itertools as _;

use crate::{
    bytecode::expr::Scope, Error, ExprKind, OpIndex, RuntimePrimValue,
    SymbolicValue,
};

use super::{
    virtual_machine::{FunctionIndex, StackIndex, VirtualMachineBuilder},
    ExposedNativeFunction, Instruction, SymbolicGraph, VMArg, VirtualMachine,
};

#[derive(Debug, Clone, Copy)]
struct LastUsage {
    /// The expression in which the expression is used.
    usage_point: OpIndex,

    /// The expression that was used.
    expr_used: OpIndex,
}

struct LastUsageCollector<'a> {
    graph: &'a SymbolicGraph,
    operations_by_scope: &'a HashMap<Scope, Vec<OpIndex>>,
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

    num_outputs: usize,
    next_free_index: &'a mut usize,
    dead_indices: &'a mut BinaryHeap<Reverse<usize>>,
    native_functions: &'a mut Vec<ExposedNativeFunction>,
    native_function_lookup: &'a mut HashMap<OpIndex, FunctionIndex>,

    reserved_outputs: &'a mut HashMap<OpIndex, StackIndex>,

    currently_stored: &'a mut HashMap<OpIndex, VMArg>,

    previously_consumed: HashSet<OpIndex>,

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

        let outputs = match output {
            SymbolicValue::Result(op_index) => match &self[op_index].kind {
                ExprKind::Tuple(values) => values.clone(),
                _ => vec![output],
            },
            _ => vec![output],
        };
        let num_outputs = outputs.len();

        let mut next_free_index = num_outputs;
        let mut native_functions: Vec<ExposedNativeFunction> = Vec::new();
        let mut native_function_lookup =
            HashMap::<OpIndex, FunctionIndex>::new();

        let mut output_lookup: HashMap<OpIndex, StackIndex> = {
            let mut output_lookup = HashMap::new();
            for (i, output) in outputs.iter().cloned().enumerate() {
                let stack_index = StackIndex(i);
                match output {
                    SymbolicValue::Result(op_index) => {
                        output_lookup.insert(op_index, stack_index);
                    }
                    SymbolicValue::Bool(value) => {
                        let value = RuntimePrimValue::Bool(value).into();
                        builder.push(Instruction::Copy {
                            value,
                            output: stack_index,
                        });
                    }
                    SymbolicValue::Int(value) => {
                        let value = RuntimePrimValue::NativeUInt(value).into();
                        builder.push(Instruction::Copy {
                            value,
                            output: stack_index,
                        });
                    }
                    SymbolicValue::Ptr(ptr) => {
                        let value = RuntimePrimValue::Ptr(ptr).into();
                        builder.push(Instruction::Copy {
                            value,
                            output: stack_index,
                        });
                    }
                }
            }
            output_lookup
        };

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
            num_outputs,
            next_free_index: &mut next_free_index,
            dead_indices: &mut dead_indices,
            native_functions: &mut native_functions,
            native_function_lookup: &mut native_function_lookup,
            reserved_outputs: &mut output_lookup,
            currently_stored: &mut currently_stored,
            previously_consumed: HashSet::new(),
            show_steps,
        };
        translater.translate(iter_op_indices)?;

        builder = builder.num_outputs(outputs.len());
        for native_func in native_functions.into_iter() {
            builder = builder.with_raw_native_function(native_func);
        }

        Ok(builder.build())
    }

    fn last_usage(&self) -> Vec<LastUsage> {
        let all_scopes = self.operation_scope();

        let operations_by_scope: HashMap<Scope, Vec<OpIndex>> = all_scopes
            .iter()
            .cloned()
            .enumerate()
            .rev()
            .map(|(i, scope)| (scope, OpIndex::new(i)))
            .into_group_map();

        let collector = LastUsageCollector {
            graph: self,
            operations_by_scope: &operations_by_scope,
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
        self.operations_by_scope
            .get(&scope)
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

                ExprKind::IfElse { condition, .. } => {
                    mark_value!(encountered, *condition);

                    let mut encountered_if: HashSet<OpIndex> =
                        encountered.clone();
                    self.walk_tree(
                        &mut encountered_if,
                        last_usage,
                        self.iter_scope(Scope::IfBranch(visiting)),
                    );

                    let mut encountered_else: HashSet<OpIndex> =
                        encountered.clone();
                    self.walk_tree(
                        &mut encountered_else,
                        last_usage,
                        self.iter_scope(Scope::ElseBranch(visiting)),
                    );

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
    fn value_to_arg(&self, value: &SymbolicValue) -> Result<VMArg, Error> {
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
                if self.previously_consumed.contains(&op_index) =>
            {
                Err(Error::AttemptedUseOfConsumedValue)
            }
            SymbolicValue::Result(op_index) => Ok(self
                .currently_stored
                .get(&op_index)
                .unwrap_or_else(|| {
                    panic!(
                        "Internal error, \
                         {op_index} was not previously translated.  \
                         This op is expression {}",
                        self.graph[*op_index].kind
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
                    let lhs = self.value_to_arg($lhs)?;
                    let rhs = self.value_to_arg($rhs)?;
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
                            vm_args.push(self.value_to_arg(arg)?);
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
                            self.previously_consumed.insert(first_arg_op);

                            let op_output = self.get_output_index(op_index);
                            if op_output.0 < self.num_outputs {
                                push_annotated!(
                                    Instruction::Swap(first_arg_loc, op_output),
                                    format!(
                                        "swap {expr_name} \
                                         to mandatory output loc {op_output}"
                                    ),
                                );
                                self.currently_stored
                                    .insert(op_index, op_output.into());
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

                ExprKind::Range { .. } => panic!(
                    "All Range expressions should be simplified \
                     to ExprKind::SimpleReduce"
                ),
                ExprKind::Map { .. } => panic!(
                    "All Map expressions should be simplified \
                     to ExprKind::SimpleReduce"
                ),
                ExprKind::Reduce { .. } => panic!(
                    "All Reduce expressions should be simplified \
                     to ExprKind::SimpleReduce"
                ),
                ExprKind::SimpleReduce {
                    initial,
                    extent,
                    reduction,
                } => {
                    let initial = self.value_to_arg(initial)?;
                    let extent = self.value_to_arg(extent)?;

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
                            push_annotated!(
                                Instruction::Swap(stack_index, op_output,),
                                format!(
                                    "move initial value \
                                     of reduction {expr_name}"
                                ),
                            );
                        }
                    }

                    let loop_iter = self.alloc_index();
                    push_annotated!(
                        Instruction::Copy {
                            value: 0usize.into(),
                            output: loop_iter,
                        },
                        format!("initialize loop iterator for {expr_name}"),
                    );

                    let loop_start = self.instructions.current_index();

                    let stop_loop_condition = self.alloc_index();
                    push_annotated!(
                        Instruction::GreaterThanOrEqual {
                            lhs: loop_iter.into(),
                            rhs: extent,
                            output: stop_loop_condition,
                        },
                        format!("loop condition of {expr_name}"),
                    );

                    // Placeholder for the conditional jump to break
                    // out of the loop.  To be updated after the loop
                    // body is generated, when we know the destination
                    // index of the jump.
                    let jump_to_end_instruction_index = push_annotated!(
                        Instruction::NoOp,
                        format!("loop for {expr_name} terminated"),
                    );

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

                            let iter_body = self
                                .instructions_by_scope
                                .get(&Scope::Function(reduction_index))
                                .into_iter()
                                .flatten()
                                .cloned()
                                .filter(|&op| {
                                    !matches!(
                                        self.graph[op].kind,
                                        ExprKind::Function { .. }
                                    )
                                });
                            let iter_body: Box<dyn Iterator<Item = OpIndex>> =
                                Box::new(iter_body);

                            self.translate(iter_body)?;

                            match self.value_to_arg(output)? {
                                VMArg::Const(value) => {
                                    push_annotated!(
                                        Instruction::Copy {
                                            value: value.into(),
                                            output: op_output,
                                        },
                                        format!("copy output of {expr_name}"),
                                    );
                                }
                                VMArg::SavedValue(body_output) => {
                                    push_annotated!(
                                        Instruction::Swap(
                                            body_output,
                                            op_output,
                                        ),
                                        format!("move output of {expr_name}"),
                                    );
                                }
                            }

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

                    push_annotated!(
                        Instruction::ConditionalJump {
                            cond: RuntimePrimValue::Bool(true).into(),
                            dest: loop_start,
                        },
                        format!("jump to beginning of {expr_name}"),
                    );

                    let after_loop = self.instructions.current_index();
                    self.instructions.update(
                        jump_to_end_instruction_index,
                        Instruction::ConditionalJump {
                            cond: stop_loop_condition.into(),
                            dest: after_loop,
                        },
                    );

                    self.free_dead_indices(op_index);

                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::NativeFunction(func) => {
                    let func_index = FunctionIndex(self.native_functions.len());
                    self.native_functions.push(func.clone());
                    self.native_function_lookup.insert(op_index, func_index);
                }

                ExprKind::IsSome(value) => {
                    let value = self.value_to_arg(value)?;
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
                    let condition = self.value_to_arg(condition)?;

                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    let mut cached_currently_stored =
                        self.currently_stored.clone();

                    let jump_to_if_branch_index = push_annotated!(
                        Instruction::NoOp,
                        format!("jump to if branch of {expr_name}"),
                    );

                    match else_branch {
                        &SymbolicValue::Result(else_index) => {
                            if let Some(existing) =
                                self.currently_stored.get(&else_index)
                            {
                                push_annotated!(
                                    Instruction::Copy {
                                        value: *existing,
                                        output: op_output
                                    },
                                    "move existing else-branch of {expr_name} \
                                     to output of if/else"
                                );
                            } else {
                                self.reserved_outputs
                                    .insert(else_index, op_output);
                                self.translate(
                                    self.instructions_by_scope
                                        .get(&Scope::ElseBranch(op_index))
                                        .into_iter()
                                        .flatten()
                                        .cloned(),
                                )?;
                                self.reserved_outputs.remove(&else_index);
                                std::mem::swap(
                                    self.currently_stored,
                                    &mut cached_currently_stored,
                                );
                            }
                        }
                        other => {
                            push_annotated!(
                                Instruction::Copy {
                                    value: other
                                        .as_prim_value()
                                        .expect(
                                            "Only SymbolicValue::Result \
                                             should return None",
                                        )
                                        .into(),
                                    output: op_output,
                                },
                                format!(
                                    "copy else-branch output \
                                     to {expr_name}"
                                ),
                            );
                        }
                    }

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

                    match if_branch {
                        &SymbolicValue::Result(if_index) => {
                            if let Some(existing) =
                                self.currently_stored.get(&if_index)
                            {
                                push_annotated!(
                                    Instruction::Copy {
                                        value: *existing,
                                        output: op_output
                                    },
                                    "move existing if-branch of {expr_name} \
                                     to output of if/else"
                                );
                            } else {
                                self.reserved_outputs
                                    .insert(if_index, op_output);
                                self.translate(
                                    self.instructions_by_scope
                                        .get(&Scope::IfBranch(op_index))
                                        .into_iter()
                                        .flatten()
                                        .cloned(),
                                )?;
                                self.reserved_outputs.remove(&if_index);
                            }
                        }
                        other => {
                            push_annotated!(
                                Instruction::Copy {
                                    value: other
                                        .as_prim_value()
                                        .expect(
                                            "Only SymbolicValue::Result \
                                             should return None",
                                        )
                                        .into(),
                                    output: op_output,
                                },
                                format!(
                                    "copy if-branch output \
                                     to {expr_name}"
                                ),
                            );
                        }
                    }

                    self.instructions.update(
                        jump_to_branch_end_index,
                        Instruction::ConditionalJump {
                            cond: true.into(),
                            dest: self.instructions.current_index(),
                        },
                    );

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
                    let value = self.value_to_arg(value)?;
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
                    let obj = self.value_to_arg(obj)?;
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
                    let ptr = self.value_to_arg(ptr)?;
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
                    let ptr = self.value_to_arg(ptr)?;
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
                    let ptr = self.value_to_arg(ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::Copy {
                            value: ptr,
                            output: op_output,
                        },
                        format!("eval {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, ptr);
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
