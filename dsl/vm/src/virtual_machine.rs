use std::{collections::HashMap, fmt::Display, mem::MaybeUninit};

use arrayvec::ArrayVec;
use derive_more::derive::From;
use dsl_runtime::{Runtime, RuntimeFunc};
use itertools::Either;

use dotnet_debugger::{MethodTable, RuntimeString};
use memory_reader::{OwnedBytes, Pointer, TypedPointer};

use dsl_ir::{
    ExposedNativeFunction, ExposedNativeObject, NativeFunction,
    RuntimePrimType, RuntimePrimValue, StackValue, WrappedNativeFunction,
};

use crate::{Error, StackIndex, VMResults};

pub struct VirtualMachineBuilder {
    instructions: Vec<Instruction>,
    native_functions: Vec<ExposedNativeFunction>,
    entry_points: HashMap<String, InstructionIndex>,
    annotations: HashMap<AnnotationLocation, String>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum AnnotationLocation {
    Before(InstructionIndex),
    At(InstructionIndex),
    After(InstructionIndex),
}

#[derive(Debug)]
pub struct VirtualMachine {
    /// The instructions to execute in the virtual machine.
    instructions: Vec<Instruction>,

    /// Functions in Rust that may be called from the bytecode.
    native_functions: Vec<ExposedNativeFunction>,

    /// A lookup table from the name of a function to its entry point.
    entry_points: HashMap<String, InstructionIndex>,

    /// The stack size required to execute the VM
    stack_size: usize,

    /// Annotations of each instruction
    annotations: HashMap<AnnotationLocation, String>,
}

pub struct VMEvaluator<'a> {
    vm: &'a VirtualMachine,
    current_instruction: InstructionIndex,
    reader: Box<dyn dsl_runtime::Reader + 'a>,
    values: VMResults,
}

#[derive(Debug, Clone, Copy, PartialEq, From)]
pub enum VMArg {
    Const(RuntimePrimValue),
    SavedValue(StackIndex),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstructionIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionIndex(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    /// Do nothing.
    NoOp,

    /// Clear the specified location.
    Clear { loc: StackIndex },

    /// Copy the value to the specified location.  Can be used to copy
    /// a value on the stack, or to store a constant value onto the
    /// stack.
    Copy { value: VMArg, output: StackIndex },

    /// Swap the values stored in two stack locations.
    Swap(StackIndex, StackIndex),

    /// If the register contains true, jump to the specified
    /// instruction.  Otherwise, continue to the next instruction.
    ConditionalJump { cond: VMArg, dest: InstructionIndex },

    /// Call a native function
    NativeFunctionCall {
        index: FunctionIndex,
        args: Vec<VMArg>,
        output: Option<StackIndex>,
    },

    /// Cast a value to the specified primitive type.
    PrimCast {
        value: VMArg,
        prim_type: RuntimePrimType,
        output: StackIndex,
    },

    /// Check if a location contains a non-None value.
    IsSome { value: VMArg, output: StackIndex },

    /// Compute the AND of two boolean expressions.
    And {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Compute the OR of two boolean expressions.
    Or {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Negate a boolean expression
    Not { arg: VMArg, output: StackIndex },

    /// Check if the two operands are equal to each other, storing the
    /// resulting boolean to the output index.
    Equal {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Check if the two operands are not equal to each other, storing
    /// the resulting boolean to the output index.
    NotEqual {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Check if the left-hand operand is less than the right-hand
    /// operand, storing the resulting boolean to the output index.
    LessThan {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Check if the left-hand operand is greater than the right-hand
    /// operand, storing the resulting boolean to the output index.
    GreaterThan {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Check if the left-hand operand is less than or equal to the
    /// right-hand operand, storing the resulting boolean to the
    /// output index.
    LessThanOrEqual {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Check if the left-hand operand is greater than or equal to the
    /// right-hand operand, storing the resulting boolean to the
    /// output index.
    GreaterThanOrEqual {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Add the two operands together, storing the result to the
    /// output index.
    Add {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Subtract the right-hand operand from the left-hand operand,
    /// storing the result to the output index.
    Sub {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Multiply the two operands together, storing the result to the
    /// output index.
    Mul {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Divide the left-hand operand by the right-hand operand,
    /// storing the result to the output index.
    Div {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Divide the left-hand operand by the right-hand operand,
    /// storing the remainder to the output index.
    Mod {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    /// Check if an object's method table indicates that it is an
    /// instance of a given type, or a subclass of the given type.
    IsSubclassOf {
        method_table_ptr: VMArg,
        base_type: TypedPointer<MethodTable>,
        output: StackIndex,
    },

    /// Read an array of bytes
    ReadBytes {
        regions: Vec<VMByteRange>,
        output: StackIndex,
    },

    /// Cast from bytes into a primitive value.
    CastBytes {
        bytes: VMArg,
        offset: VMArg,
        prim_type: RuntimePrimType,
        output: StackIndex,
    },

    /// Given a pointer to a .NET string, read the contents of the
    /// string, storing the result into a native Rust `String` located
    /// at the output index.
    ReadString { ptr: VMArg, output: StackIndex },

    /// End execution of the VM, and return to the parent scope.
    Return { outputs: Vec<VMArg> },
}

#[derive(Debug, Clone, PartialEq)]
pub struct VMByteRange {
    pub ptr: VMArg,
    pub num_bytes: VMArg,
}

pub trait NormalizeStackIndex {
    fn normalize_stack_index(self) -> usize;
}
impl NormalizeStackIndex for StackIndex {
    fn normalize_stack_index(self) -> usize {
        self.0
    }
}
impl NormalizeStackIndex for usize {
    fn normalize_stack_index(self) -> usize {
        self
    }
}

trait RuntimeOutputExt {
    fn collect_native_function_args_impl<'a>(
        &'a mut self,
        vm_args: &[VMArg],
        inline_consts: &'a mut [Option<StackValue>],
        collected_args: &mut [MaybeUninit<&'a mut Option<StackValue>>],
    );

    fn collect_native_function_args<'a>(
        &'a mut self,
        args: &[VMArg],
        inline_consts: &'a mut [Option<StackValue>],
    ) -> Vec<&'a mut Option<StackValue>>;
}
impl RuntimeOutputExt for VMResults {
    fn collect_native_function_args_impl<'a>(
        &'a mut self,
        vm_args: &[VMArg],
        inline_consts: &'a mut [Option<StackValue>],
        collected_args: &mut [MaybeUninit<&'a mut Option<StackValue>>],
    ) {
        assert_eq!(vm_args.len(), inline_consts.len());
        assert_eq!(vm_args.len(), collected_args.len());

        vm_args
            .iter()
            .zip(inline_consts.iter_mut())
            .filter_map(|(vm_arg, scratch)| match vm_arg {
                VMArg::Const(prim) => Some((*prim, scratch)),
                _ => None,
            })
            .for_each(|(prim, inline_const)| {
                *inline_const = Some(StackValue::Prim(prim));
            });

        inline_consts
            .iter_mut()
            .zip(collected_args.iter_mut())
            .for_each(|(opt_const, collected_arg)| {
                if opt_const.is_some() {
                    *collected_arg = MaybeUninit::new(opt_const);
                }
            });

        const MAX_STACK_ARGS: usize = 32;
        struct ArgInfo {
            arg_index: usize,
            stack_index: usize,
        }
        let mut arg_info_array = ArrayVec::<ArgInfo, MAX_STACK_ARGS>::new();
        vm_args.iter().enumerate().for_each(|(i, vm_arg)| {
            if let VMArg::SavedValue(stack_index) = vm_arg {
                arg_info_array.push(ArgInfo {
                    arg_index: i,
                    stack_index: stack_index.0,
                });
            }
        });
        arg_info_array.sort_by_key(|info| info.stack_index);

        let mut prev_stack_index = None;
        let mut remaining_values = &mut self[..];
        for info in arg_info_array {
            let split_index = match prev_stack_index {
                Some(prev) => info.stack_index - prev,
                None => info.stack_index + 1,
            };
            let (left, right) = remaining_values.split_at_mut(split_index);
            collected_args[info.arg_index] =
                MaybeUninit::new(left.last_mut().unwrap());
            remaining_values = right;
            prev_stack_index = Some(info.stack_index);
        }
    }

    fn collect_native_function_args<'a>(
        &'a mut self,
        args: &[VMArg],
        inline_consts: &'a mut [Option<StackValue>],
    ) -> Vec<&'a mut Option<StackValue>> {
        let mut references: Vec<&mut Option<StackValue>> =
            Vec::with_capacity(args.len());
        self.collect_native_function_args_impl(
            args,
            inline_consts,
            references.spare_capacity_mut(),
        );
        unsafe {
            references.set_len(args.len());
        }
        references
    }
}

impl VirtualMachineBuilder {
    pub fn mark_entry_point(
        &mut self,
        name: impl Into<String>,
    ) -> Result<(), Error> {
        let name = name.into();
        if self.entry_points.contains_key(&name) {
            Err(Error::DuplicateFunctionName(name).into())
        } else {
            self.entry_points.insert(name, self.current_index());
            Ok(())
        }
    }

    pub fn push(&mut self, inst: Instruction) -> InstructionIndex {
        let index = InstructionIndex(self.instructions.len());
        self.instructions.push(inst);
        index
    }

    pub fn update(&mut self, index: InstructionIndex, inst: Instruction) {
        self.instructions[index.0] = inst;
    }

    pub fn current_index(&self) -> InstructionIndex {
        InstructionIndex(self.instructions.len())
    }

    pub fn annotate(
        &mut self,
        inst: impl Into<AnnotationLocation>,
        annot: impl Display,
    ) {
        self.annotations
            .entry(inst.into())
            .and_modify(|prev| {
                *prev = format!("{prev}\n{annot}");
            })
            .or_insert_with(|| format!("{annot}"));
    }

    pub fn push_raw_native_function<T>(&mut self, func: T) -> FunctionIndex
    where
        T: 'static,
        T: Into<ExposedNativeFunction>,
    {
        let index = FunctionIndex(self.native_functions.len());
        self.native_functions.push(func.into());
        index
    }

    pub fn push_native_function<Func, ArgList>(
        &mut self,
        func: Func,
    ) -> FunctionIndex
    where
        WrappedNativeFunction<Func, ArgList>: NativeFunction,
        WrappedNativeFunction<Func, ArgList>: 'static,
    {
        let index = FunctionIndex(self.native_functions.len());
        let wrapped = WrappedNativeFunction::new(func);
        self.native_functions.push(wrapped.into());
        index
    }

    pub fn with_entry_point(
        mut self,
        name: impl Into<String>,
    ) -> Result<Self, Error> {
        self.mark_entry_point(name)?;
        Ok(self)
    }

    pub fn with_instructions(self, instructions: Vec<Instruction>) -> Self {
        Self {
            instructions,
            ..self
        }
    }

    pub fn with_raw_native_function<T>(mut self, func: T) -> Self
    where
        T: 'static,
        T: Into<ExposedNativeFunction>,
    {
        self.native_functions.push(func.into());
        self
    }

    pub fn with_native_function<Func, ArgList>(mut self, func: Func) -> Self
    where
        WrappedNativeFunction<Func, ArgList>: NativeFunction,
        WrappedNativeFunction<Func, ArgList>: 'static,
    {
        let wrapped = WrappedNativeFunction::new(func);
        self.native_functions.push(wrapped.into());
        self
    }

    pub fn build(self) -> VirtualMachine {
        let num_values = self
            .instructions
            .iter()
            .flat_map(|instruction| {
                instruction
                    .input_indices()
                    .chain(instruction.output_indices())
            })
            .map(|index| index.0 + 1)
            .max()
            .unwrap_or(0);

        VirtualMachine {
            instructions: self.instructions,
            native_functions: self.native_functions,
            entry_points: self.entry_points,
            stack_size: num_values,
            annotations: self.annotations,
        }
    }
}

impl VirtualMachine {
    pub fn builder() -> VirtualMachineBuilder {
        VirtualMachineBuilder {
            instructions: Vec::new(),
            native_functions: Vec::new(),
            entry_points: HashMap::new(),
            annotations: HashMap::new(),
        }
    }

    pub fn num_instructions(&self) -> usize {
        self.instructions.len()
    }

    pub fn stack_size(&self) -> usize {
        self.stack_size
    }

    pub fn get_function<'a>(
        &'a self,
        name: &str,
    ) -> Result<VMEvaluator<'a>, Error> {
        let entry_point = *self
            .entry_points
            .get(name)
            .ok_or_else(|| Error::NoSuchFunction(name.into()))?;

        let values = VMResults::new(self.stack_size);

        Ok(VMEvaluator {
            vm: self,
            current_instruction: entry_point,
            reader: Box::new(dsl_runtime::DummyReader),
            values,
        })
    }

    /// Evaluate the virtual machine, raising an error if any
    /// instructions attempt to read from the remote process.
    pub fn local_eval(&self) -> Result<VMResults, Error> {
        self.get_function("main")?.evaluate()
    }

    /// Evaluate the virtual machine, reading from the remote process
    /// as necessary.
    pub fn evaluate(
        &self,
        reader: impl dsl_runtime::Reader,
    ) -> Result<VMResults, Error> {
        self.get_function("main")?.with_reader(reader).evaluate()
    }
}

impl Runtime for VirtualMachine {
    type Error = Error;

    type Func<'a>
        = VMEvaluator<'a>
    where
        Self: 'a;

    fn get_function<'a>(
        &'a self,
        name: &str,
    ) -> Result<Self::Func<'a>, Self::Error> {
        self.get_function(name)
    }
}

impl<'a> RuntimeFunc<'a> for VMEvaluator<'a> {
    type Error = Error;

    fn with_reader(self, reader: impl dsl_runtime::Reader + 'a) -> Self {
        self.with_reader(reader)
    }

    fn evaluate(self) -> Result<VMResults, Self::Error> {
        self.evaluate()
    }
}

macro_rules! define_binary_op {
    ($func_name:ident,
     $(
         ($lhs_ty:ident($lhs_var:ident) ,
          $rhs_ty:ident($rhs_var:ident) $(,)?
         ) => $result:expr
     ),* $(,)?
    ) => {
        fn $func_name(
            &mut self,
            lhs: VMArg,
            rhs: VMArg,
            output: StackIndex,
        ) -> Result<(), Error> {
            let op_name =
                self.vm.instructions[self.current_instruction.0].op_name();

            let opt_lhs = self.arg_to_prim(lhs)?;
            let opt_rhs = self.arg_to_prim(rhs)?;

            self.values[output] = match (opt_lhs, opt_rhs) {
                (Some(lhs), Some(rhs)) => {
                    let res: RuntimePrimValue = match (lhs, rhs) {
                        $(
                            (
                                RuntimePrimValue::$lhs_ty($lhs_var),
                                RuntimePrimValue::$rhs_ty($rhs_var),
                            ) => Ok($result.into()),
                        )*
                            (lhs, rhs) => Err(Error::InvalidOperandsForBinaryOp {
                                op: op_name,
                                lhs: lhs.runtime_type().into(),
                                rhs: rhs.runtime_type().into(),
                            }),
                    }?;

                    let res: StackValue = res.into();

                    Some(res)
                }
                _ => None,
            };

            Ok(())
        }
    };
}

macro_rules! define_comparison_op {
    ($func_name:ident, $cmp:ident) => {
        define_binary_op! {
            $func_name,
            ( NativeUInt(a), NativeUInt(b) ) => a.$cmp(&b),
            ( I32(a), I32(b) ) => a.$cmp(&b),
            ( F32(a), F32(b) ) => a.$cmp(&b),
            ( F64(a), F64(b) ) => a.$cmp(&b),

            ( NativeUInt(a), U32(b) ) => a.$cmp(&(b as usize)),
            ( U32(a), NativeUInt(b) ) => (a as usize).$cmp(&b),

            ( NativeUInt(a), F32(b) ) => (a as f32).$cmp(&b),
            ( F32(a), NativeUInt(b) ) => a.$cmp(&(b as f32)),

            ( NativeUInt(a), F64(b) ) => (a as f64).$cmp(&b),
            ( F64(a), NativeUInt(b) ) => a.$cmp(&(b as f64)),
        }
    };
}

impl<'a> VMEvaluator<'a> {
    pub fn with_reader(
        self,
        reader: impl dsl_runtime::Reader + 'a,
    ) -> VMEvaluator<'a> {
        VMEvaluator {
            reader: Box::new(reader.with_cache()),
            ..self
        }
    }

    pub fn evaluate(mut self) -> Result<VMResults, Error> {
        while self.current_instruction.0 < self.vm.instructions.len() {
            let instruction = &self.vm.instructions[self.current_instruction.0];
            let mut next_instruction =
                InstructionIndex(self.current_instruction.0 + 1);
            self.values.increment_eval_counter();

            match instruction {
                Instruction::NoOp => {}
                &Instruction::Clear { loc } => self.eval_clear(loc)?,
                &Instruction::Copy { value, output } => {
                    self.eval_copy(value, output)?
                }
                &Instruction::Swap(lhs, rhs) => self.eval_swap(lhs, rhs)?,

                &Instruction::ConditionalJump { cond, dest } => {
                    self.eval_conditional_jump(
                        cond,
                        dest,
                        &mut next_instruction,
                    )?;
                }

                Instruction::NativeFunctionCall {
                    index,
                    args,
                    output,
                } => {
                    self.eval_native_function_call(*index, args, *output)?;
                }

                &Instruction::PrimCast {
                    value,
                    prim_type,
                    output,
                } => self.eval_prim_cast(value, prim_type, output)?,

                &Instruction::IsSome { value, output } => {
                    self.eval_is_some(value, output)?
                }

                &Instruction::Add { lhs, rhs, output } => {
                    self.eval_add(lhs, rhs, output)?
                }
                &Instruction::Sub { lhs, rhs, output } => {
                    self.eval_sub(lhs, rhs, output)?
                }
                &Instruction::Mul { lhs, rhs, output } => {
                    self.eval_mul(lhs, rhs, output)?
                }
                &Instruction::Div { lhs, rhs, output } => {
                    self.eval_div(lhs, rhs, output)?
                }
                &Instruction::Mod { lhs, rhs, output } => {
                    self.eval_mod(lhs, rhs, output)?
                }

                &Instruction::And { lhs, rhs, output } => {
                    self.eval_and(lhs, rhs, output)?
                }

                &Instruction::Or { lhs, rhs, output } => {
                    self.eval_or(lhs, rhs, output)?
                }

                &Instruction::Not { arg, output } => {
                    self.eval_not(arg, output)?
                }

                &Instruction::Equal { lhs, rhs, output } => {
                    self.eval_eq(lhs, rhs, output)?
                }
                &Instruction::NotEqual { lhs, rhs, output } => {
                    self.eval_ne(lhs, rhs, output)?
                }
                &Instruction::LessThan { lhs, rhs, output } => {
                    self.eval_lt(lhs, rhs, output)?
                }
                &Instruction::LessThanOrEqual { lhs, rhs, output } => {
                    self.eval_le(lhs, rhs, output)?
                }
                &Instruction::GreaterThan { lhs, rhs, output } => {
                    self.eval_gt(lhs, rhs, output)?
                }
                &Instruction::GreaterThanOrEqual { lhs, rhs, output } => {
                    self.eval_ge(lhs, rhs, output)?
                }

                &Instruction::IsSubclassOf {
                    method_table_ptr,
                    base_type,
                    output,
                } => self.eval_is_subclass_of(
                    method_table_ptr,
                    base_type,
                    output,
                )?,

                Instruction::ReadBytes { regions, output } => {
                    self.eval_read_bytes(regions, *output)?
                }

                &Instruction::CastBytes {
                    bytes,
                    offset,
                    prim_type,
                    output,
                } => self.eval_cast_bytes(bytes, offset, prim_type, output)?,

                &Instruction::ReadString { ptr, output } => {
                    self.eval_read_string(ptr, output)?
                }

                Instruction::Return { outputs } => {
                    let mut inline_consts = Vec::with_capacity(outputs.len());
                    inline_consts.resize_with(outputs.len(), Default::default);
                    let mut collected_outputs =
                        self.values.collect_native_function_args(
                            outputs,
                            &mut inline_consts,
                        );
                    let outputs = collected_outputs
                        .iter_mut()
                        .map(|opt_mut| opt_mut.take())
                        .collect();
                    return Ok(self.values.replace(outputs));
                }
            }

            self.current_instruction = next_instruction;
        }

        Err(Error::ReachedEndWithoutReturnInstruction.into())
    }

    fn arg_to_prim(
        &self,
        arg: VMArg,
    ) -> Result<Option<RuntimePrimValue>, Error> {
        match arg {
            VMArg::Const(value) => Ok(Some(value)),
            VMArg::SavedValue(index) => match &self.values[index] {
                Some(StackValue::Prim(prim)) => Ok(Some(*prim)),
                Some(other) => Err(Error::OperatorExpectsPrimitiveArgument {
                    operator: self.vm.instructions[self.current_instruction.0]
                        .op_name(),
                    index: self.current_instruction,
                    arg_type: other.runtime_type(),
                }
                .into()),
                None => Ok(None),
            },
        }
    }

    fn arg_to_byte_array(&self, arg: VMArg) -> Result<Option<&[u8]>, Error> {
        match arg {
            VMArg::Const(value) => Err(Error::OperatorExpectsByteArray {
                operator: self.vm.instructions[self.current_instruction.0]
                    .op_name(),
                index: self.current_instruction,
                arg_type: value.runtime_type().into(),
            }
            .into()),
            VMArg::SavedValue(index) => match &self.values[index] {
                Some(stack_value) => {
                    let bytes =
                        stack_value.as_byte_array().ok_or_else(|| {
                            Error::OperatorExpectsByteArray {
                                operator: self.vm.instructions
                                    [self.current_instruction.0]
                                    .op_name(),
                                index: self.current_instruction,
                                arg_type: stack_value.runtime_type(),
                            }
                        })?;
                    Ok(Some(bytes))
                }
                None => Ok(None),
            },
        }
    }

    fn eval_clear(&mut self, loc: StackIndex) -> Result<(), Error> {
        self.values[loc] = None;
        Ok(())
    }

    fn eval_copy(
        &mut self,
        value: VMArg,
        output: StackIndex,
    ) -> Result<(), Error> {
        let opt_value = self.arg_to_prim(value)?;

        self.values[output] = opt_value.map(Into::into);
        Ok(())
    }

    fn eval_swap(
        &mut self,
        lhs: StackIndex,
        rhs: StackIndex,
    ) -> Result<(), Error> {
        if lhs != rhs {
            let (a, b) = if lhs.0 < rhs.0 {
                (lhs, rhs)
            } else {
                (rhs, lhs)
            };

            let (a_slice, b_slice) = self.values[a..].split_at_mut(b - a);
            let a_mut = a_slice.first_mut().unwrap();
            let b_mut = b_slice.first_mut().unwrap();
            std::mem::swap(a_mut, b_mut);
        }
        Ok(())
    }

    fn eval_conditional_jump(
        &mut self,
        cond: VMArg,
        dest: InstructionIndex,
        next_instruction: &mut InstructionIndex,
    ) -> Result<(), Error> {
        let opt_cond = self.arg_to_prim(cond)?;

        let should_jump = opt_cond
            .map(|val| match val {
                RuntimePrimValue::Bool(val) => Ok(val),
                other => Err(Error::InvalidOperandForConditionalJump(
                    other.runtime_type(),
                )),
            })
            .transpose()?
            .unwrap_or(false);

        if should_jump {
            *next_instruction = dest;
        }

        Ok(())
    }

    fn eval_native_function_call(
        &mut self,
        func_index: FunctionIndex,
        vm_args: &[VMArg],
        output: Option<StackIndex>,
    ) -> Result<(), Error> {
        const MAX_STACK_ARGS: usize = 16;
        let result = if vm_args.len() <= MAX_STACK_ARGS {
            let mut inline_consts: [Option<StackValue>; MAX_STACK_ARGS] =
                Default::default();
            let inline_consts = &mut inline_consts[..vm_args.len()];

            let mut collected_args =
                [const { MaybeUninit::<&mut Option<StackValue>>::uninit() };
                    MAX_STACK_ARGS];
            let arg_slice = &mut collected_args[..vm_args.len()];

            self.values.collect_native_function_args_impl(
                vm_args,
                inline_consts,
                arg_slice,
            );
            let arg_slice = unsafe {
                std::mem::transmute::<
                    &mut [MaybeUninit<&mut Option<StackValue>>],
                    &mut [&mut Option<StackValue>],
                >(arg_slice)
            };

            self.vm.native_functions[func_index.0].apply(arg_slice)?
        } else {
            let mut inline_consts = Vec::with_capacity(vm_args.len());
            inline_consts.resize_with(vm_args.len(), Default::default);

            let mut collected_args: Vec<&mut Option<StackValue>> =
                Vec::with_capacity(vm_args.len());
            self.values.collect_native_function_args_impl(
                vm_args,
                &mut inline_consts,
                collected_args.spare_capacity_mut(),
            );
            unsafe {
                collected_args.set_len(vm_args.len());
            }

            self.vm.native_functions[func_index.0].apply(&mut collected_args)?
        };

        if let Some(output) = output {
            self.values[output] = result;
        }
        Ok(())
    }

    fn eval_prim_cast(
        &mut self,
        value: VMArg,
        prim_type: RuntimePrimType,
        output: StackIndex,
    ) -> Result<(), Error> {
        let opt_value = self.arg_to_prim(value)?;
        self.values[output] = opt_value
            .map(|val| val.prim_cast(prim_type))
            .transpose()?
            .map(Into::into);
        Ok(())
    }

    fn eval_is_some(
        &mut self,
        value: VMArg,
        output: StackIndex,
    ) -> Result<(), Error> {
        let is_some: bool = match value {
            VMArg::Const(_) => true,
            VMArg::SavedValue(stack_index) => {
                self.values[stack_index].is_some()
            }
        };
        self.values[output] = Some(RuntimePrimValue::Bool(is_some).into());
        Ok(())
    }

    define_binary_op! {
        eval_add,
        (NativeUInt(a), NativeUInt(b)) => a + b,
        (Ptr(a), NativeUInt(b)) => a.try_add(b)?,
        (NativeUInt(a), Ptr(b)) => b.try_add(a)?,
        (I32(a), I32(b)) => a + b,
        (F32(a), F32(b)) => a + b,
        (F64(a), F64(b)) => a + b,
    }

    define_binary_op! {
        eval_sub,
        (NativeUInt(a), NativeUInt(b)) => a - b,
        (Ptr(a), NativeUInt(b)) => a - b,
        (Ptr(a), Ptr(b)) => a - b,
        (I32(a), I32(b)) => a - b,
        (F32(a), F32(b)) => a - b,
        (F64(a), F64(b)) => a - b,
    }

    define_binary_op! {
        eval_mul,
        (NativeUInt(a), NativeUInt(b)) => a*b,
        (F32(a), F32(b)) => a*b,
        (F64(a), F64(b)) => a*b,
    }

    define_binary_op! {
        eval_div,
        (NativeUInt(a),NativeUInt(b)) => a.div_euclid(b),
        (I32(a),NativeUInt(b)) => a.div_euclid(b as i32),
        (F32(a),NativeUInt(b)) => a / (b as f32),
        (F64(a),NativeUInt(b)) => a / (b as f64),
    }

    define_binary_op! {
        eval_mod,
        (NativeUInt(a),NativeUInt(b)) => a.rem_euclid(b)
    }

    fn eval_and(
        &mut self,
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    ) -> Result<(), Error> {
        let op_name =
            self.vm.instructions[self.current_instruction.0].op_name();

        let opt_lhs = self.arg_to_prim(lhs)?;
        let opt_rhs = self.arg_to_prim(rhs)?;

        self.values[output] = match (opt_lhs, opt_rhs) {
            (None, Some(RuntimePrimValue::Bool(false)))
            | (Some(RuntimePrimValue::Bool(false)), None) => {
                Some(RuntimePrimValue::Bool(false).into())
            }
            (Some(lhs), Some(rhs)) => {
                let res = match (lhs, rhs) {
                    (RuntimePrimValue::Bool(a), RuntimePrimValue::Bool(b)) => {
                        Ok(RuntimePrimValue::Bool(a && b))
                    }
                    _ => Err(Error::InvalidOperandsForBinaryOp {
                        op: op_name,
                        lhs: lhs.runtime_type().into(),
                        rhs: rhs.runtime_type().into(),
                    }),
                }?;
                Some(res.into())
            }
            _ => None,
        };

        Ok(())
    }

    fn eval_or(
        &mut self,
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    ) -> Result<(), Error> {
        let op_name =
            self.vm.instructions[self.current_instruction.0].op_name();

        let opt_lhs = self.arg_to_prim(lhs)?;
        let opt_rhs = self.arg_to_prim(rhs)?;

        self.values[output] = match (opt_lhs, opt_rhs) {
            (None, Some(RuntimePrimValue::Bool(true)))
            | (Some(RuntimePrimValue::Bool(true)), None) => {
                Some(RuntimePrimValue::Bool(true).into())
            }
            (Some(lhs), Some(rhs)) => {
                let res = match (lhs, rhs) {
                    (RuntimePrimValue::Bool(a), RuntimePrimValue::Bool(b)) => {
                        Ok(RuntimePrimValue::Bool(a || b))
                    }
                    _ => Err(Error::InvalidOperandsForBinaryOp {
                        op: op_name,
                        lhs: lhs.runtime_type().into(),
                        rhs: rhs.runtime_type().into(),
                    }),
                }?;
                Some(res.into())
            }
            _ => None,
        };

        Ok(())
    }

    fn eval_not(
        &mut self,
        arg: VMArg,
        output: StackIndex,
    ) -> Result<(), Error> {
        let op_name =
            self.vm.instructions[self.current_instruction.0].op_name();

        let opt_arg = self.arg_to_prim(arg)?;

        self.values[output] = match opt_arg {
            Some(RuntimePrimValue::Bool(b)) => {
                Ok(Some(RuntimePrimValue::Bool(!b).into()))
            }
            Some(other) => Err(Error::InvalidOperandForUnaryOp {
                op: op_name,
                arg: other.runtime_type().into(),
            }),
            None => Ok(None),
        }?;

        Ok(())
    }

    define_comparison_op! {eval_eq, eq}
    define_comparison_op! {eval_ne, ne}
    define_comparison_op! {eval_lt, lt}
    define_comparison_op! {eval_le, le}
    define_comparison_op! {eval_gt, gt}
    define_comparison_op! {eval_ge, ge}

    fn eval_is_subclass_of(
        &mut self,
        method_table_ptr: VMArg,
        base_type: TypedPointer<MethodTable>,
        output: StackIndex,
    ) -> Result<(), Error> {
        let method_table_ptr = self.arg_to_prim(method_table_ptr)?;

        self.values[output] = match method_table_ptr {
            None => Ok(None),
            Some(RuntimePrimValue::Ptr(ptr)) => {
                let is_subclass = self
                    .reader
                    .is_dotnet_base_class_of(base_type, ptr.into())?;

                Ok(Some(RuntimePrimValue::Bool(is_subclass).into()))
            }
            Some(other) => Err(Error::InvalidArgumentForSubclassCheck(other)),
        }?;

        Ok(())
    }

    fn eval_read_bytes(
        &mut self,
        regions: &[VMByteRange],
        output: StackIndex,
    ) -> Result<(), Error> {
        let mut num_bytes = 0usize;
        for region in regions.iter() {
            let Some(region_bytes) = self.arg_to_prim(region.num_bytes)? else {
                self.values[output] = None;
                return Ok(());
            };
            let region_bytes: usize =
                region_bytes.try_into().map_err(|_| {
                    Error::ByteCountNotConvertibleToInt {
                        operator: self.vm.instructions
                            [self.current_instruction.0]
                            .op_name(),
                        index: self.current_instruction,
                        value: region_bytes,
                    }
                })?;
            num_bytes += region_bytes;
        }

        let mut new_stack_value = if num_bytes
            <= StackValue::SMALL_BYTE_ARRAY_SIZE
        {
            StackValue::SmallByteArray([0u8; StackValue::SMALL_BYTE_ARRAY_SIZE])
        } else {
            StackValue::ByteArray(vec![0u8; num_bytes])
        };

        {
            let mut bytes = new_stack_value
                .as_mut_byte_array()
                .expect("Just constructed as byte array");

            const MAX_BATCHED_RANGES: usize = 16;
            let mut to_read =
                ArrayVec::<(Pointer, &mut [u8]), MAX_BATCHED_RANGES>::new();

            macro_rules! flush {
                () => {
                    match to_read.len() {
                        0 => {}
                        1 => {
                            let (ptr, region_bytes) = to_read.pop().unwrap();
                            if !ptr.is_null() {
                                self.reader.read_bytes(ptr, region_bytes)?;
                            }
                        }
                        _ => {
                            self.reader.read_byte_regions(&mut to_read)?;
                        }
                    }
                    to_read.clear();
                };
            }

            for region in regions.iter() {
                let ptr: Pointer = self
                    .arg_to_prim(region.ptr)?
                    .map(TryInto::try_into)
                    .transpose()?
                    .unwrap_or_else(Pointer::null);

                let region_bytes: usize = self
                    .arg_to_prim(region.num_bytes)
                    .expect("No error from first check")
                    .expect("Was Some(_) in first check")
                    .try_into()
                    .expect("Was convertible to usize in first check");

                let (left, right) = bytes.split_at_mut(region_bytes);
                bytes = right;
                if !ptr.is_null() {
                    to_read.push((ptr, left));
                    if to_read.len() == to_read.capacity() {
                        flush!();
                    }
                }
            }
            flush!();
        }

        self.values[output] = Some(new_stack_value);

        Ok(())
    }

    fn eval_cast_bytes(
        &mut self,
        bytes: VMArg,
        offset: VMArg,
        prim_type: RuntimePrimType,
        output: StackIndex,
    ) -> Result<(), Error> {
        let opt_offset = self.arg_to_prim(offset)?;
        let opt_bytes = self.arg_to_byte_array(bytes)?;

        self.values[output] = match (opt_bytes, opt_offset) {
            (None, _) | (_, None) => None,
            (Some(bytes), Some(offset)) => {
                let offset: usize = offset.try_into()?;
                let (_, bytes) =
                    bytes.split_at_checked(offset).ok_or_else(|| {
                        Error::OutOfBoundsByteIndex {
                            operator: self.vm.instructions
                                [self.current_instruction.0]
                                .op_name(),
                            index: self.current_instruction,
                            byte_index: offset,
                            array_size: bytes.len(),
                        }
                    })?;

                let prim_value = prim_type.parse(bytes)?;

                match prim_value {
                    RuntimePrimValue::Ptr(ptr) if ptr.is_null() => None,
                    other => Some(other.into()),
                }
            }
        };

        Ok(())
    }

    fn eval_read_string(
        &mut self,
        ptr: VMArg,
        output: StackIndex,
    ) -> Result<(), Error> {
        let opt_ptr = self.arg_to_prim(ptr)?;

        self.values[output] = opt_ptr
            .map(|prim| -> Result<_, Error> {
                let ptr: Pointer = prim.try_into()?;
                let runtime_string = RuntimeString::read_string(
                    ptr,
                    |byte_range: std::ops::Range<Pointer>|
                               -> Result<OwnedBytes, Error> {
                        let num_bytes = byte_range.end - byte_range.start;
                        let mut bytes = vec![0; num_bytes];
                        self.reader.read_bytes(byte_range.start, &mut bytes)?;
                        Ok(OwnedBytes::new(byte_range.start, bytes))
                    },
                )?;
                let string: String = runtime_string.into();
                Ok(ExposedNativeObject::new(string).into())
            })
            .transpose()?;

        Ok(())
    }
}

impl Instruction {
    fn input_indices(&self) -> impl Iterator<Item = StackIndex> + '_ {
        let (lhs, rhs, dyn_args) = match self {
            // Nullary instructions
            Instruction::NoOp | Instruction::Clear { .. } => (None, None, None),

            // Unary instructions
            Instruction::Copy { value: arg, .. }
            | Instruction::ConditionalJump { cond: arg, .. }
            | Instruction::PrimCast { value: arg, .. }
            | Instruction::IsSubclassOf {
                method_table_ptr: arg,
                ..
            }
            | Instruction::ReadString { ptr: arg, .. }
            | Instruction::IsSome { value: arg, .. }
            | Instruction::Not { arg, .. } => (Some(*arg), None, None),

            // Binary instructions
            Instruction::And { lhs, rhs, .. }
            | Instruction::Or { lhs, rhs, .. }
            | Instruction::Equal { lhs, rhs, .. }
            | Instruction::NotEqual { lhs, rhs, .. }
            | Instruction::LessThan { lhs, rhs, .. }
            | Instruction::GreaterThan { lhs, rhs, .. }
            | Instruction::LessThanOrEqual { lhs, rhs, .. }
            | Instruction::GreaterThanOrEqual { lhs, rhs, .. }
            | Instruction::Add { lhs, rhs, .. }
            | Instruction::Sub { lhs, rhs, .. }
            | Instruction::Mul { lhs, rhs, .. }
            | Instruction::Div { lhs, rhs, .. }
            | Instruction::Mod { lhs, rhs, .. } => {
                (Some(*lhs), Some(*rhs), None)
            }

            Instruction::CastBytes { bytes, offset, .. } => {
                (Some(*bytes), Some(*offset), None)
            }

            Instruction::Swap(lhs, rhs) => (
                Some(VMArg::SavedValue(*lhs)),
                Some(VMArg::SavedValue(*rhs)),
                None,
            ),

            // Arbitrary arity instructions
            Instruction::NativeFunctionCall { args: values, .. }
            | Instruction::Return { outputs: values } => {
                (None, None, Some(Either::Left(values.iter().cloned())))
            }
            Instruction::ReadBytes { regions, .. } => (
                None,
                None,
                Some(Either::Right(
                    regions
                        .iter()
                        .flat_map(|region| [region.ptr, region.num_bytes]),
                )),
            ),
        };

        std::iter::empty()
            .chain(lhs)
            .chain(rhs)
            .chain(dyn_args.into_iter().flatten())
            .filter_map(|arg| match arg {
                VMArg::SavedValue(stack_index) => Some(stack_index),
                VMArg::Const(_) => None,
            })
    }

    fn output_indices(&self) -> impl Iterator<Item = StackIndex> {
        let (opt_a, opt_b) = match self {
            Instruction::NoOp
            | Instruction::NativeFunctionCall { output: None, .. }
            | Instruction::ConditionalJump { .. }
            | Instruction::Return { .. } => (None, None),

            Instruction::Clear { loc: output }
            | Instruction::Copy { output, .. }
            | Instruction::NativeFunctionCall {
                output: Some(output),
                ..
            }
            | Instruction::PrimCast { output, .. }
            | Instruction::And { output, .. }
            | Instruction::Or { output, .. }
            | Instruction::Not { output, .. }
            | Instruction::Equal { output, .. }
            | Instruction::NotEqual { output, .. }
            | Instruction::LessThan { output, .. }
            | Instruction::GreaterThan { output, .. }
            | Instruction::LessThanOrEqual { output, .. }
            | Instruction::GreaterThanOrEqual { output, .. }
            | Instruction::Add { output, .. }
            | Instruction::Sub { output, .. }
            | Instruction::Mul { output, .. }
            | Instruction::Div { output, .. }
            | Instruction::Mod { output, .. }
            | Instruction::IsSubclassOf { output, .. }
            | Instruction::ReadBytes { output, .. }
            | Instruction::CastBytes { output, .. }
            | Instruction::ReadString { output, .. }
            | Instruction::IsSome { output, .. } => (Some(*output), None),

            Instruction::Swap(lhs, rhs) => (Some(*lhs), Some(*rhs)),
        };

        std::iter::empty().chain(opt_a).chain(opt_b)
    }

    fn op_name(&self) -> &'static str {
        match self {
            Instruction::NoOp => "NoOp",
            Instruction::Clear { .. } => "Clear",
            Instruction::Copy { .. } => "Copy",
            Instruction::Swap { .. } => "Swap",
            Instruction::ConditionalJump { .. } => "ConditionalJump",
            Instruction::NativeFunctionCall { .. } => "NativeFunctionCall",
            Instruction::PrimCast { .. } => "PrimCast",
            Instruction::IsSome { .. } => "IsSome",
            Instruction::And { .. } => "And",
            Instruction::Or { .. } => "Or",
            Instruction::Not { .. } => "Not",
            Instruction::Equal { .. } => "Equal ",
            Instruction::NotEqual { .. } => "NotEqual",
            Instruction::LessThan { .. } => "LessThan",
            Instruction::GreaterThan { .. } => "GreaterThan",
            Instruction::LessThanOrEqual { .. } => "LessThanOrEqual",
            Instruction::GreaterThanOrEqual { .. } => "GreaterThanOrEqual",
            Instruction::Add { .. } => "Add",
            Instruction::Sub { .. } => "Sub",
            Instruction::Mul { .. } => "Mul",
            Instruction::Div { .. } => "Div",
            Instruction::Mod { .. } => "Mod",
            Instruction::IsSubclassOf { .. } => "IsSubclassOf",
            Instruction::ReadBytes { .. } => "ReadBytes",
            Instruction::CastBytes { .. } => "CastBytes",
            Instruction::ReadString { .. } => "ReadString",
            Instruction::Return { .. } => "Return",
        }
    }
}

impl From<InstructionIndex> for AnnotationLocation {
    fn from(instruction: InstructionIndex) -> Self {
        Self::At(instruction)
    }
}

impl From<usize> for VMArg {
    fn from(value: usize) -> Self {
        VMArg::Const(RuntimePrimValue::NativeUInt(value))
    }
}
impl From<bool> for VMArg {
    fn from(value: bool) -> Self {
        VMArg::Const(RuntimePrimValue::Bool(value))
    }
}

impl Display for VirtualMachine {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{} instructions, stack size of {}",
            self.instructions.len(),
            self.stack_size,
        )?;
        for (i, instruction) in self.instructions.iter().enumerate() {
            let index = InstructionIndex(i);

            if let Some(annot) =
                self.annotations.get(&AnnotationLocation::Before(index))
            {
                for line in annot.split('\n') {
                    writeln!(f, "// {line}")?;
                }
            }

            write!(f, "{i}: {instruction}")?;
            if let Some(annot) =
                self.annotations.get(&AnnotationLocation::At(index))
            {
                write!(f, " // {annot}")?;
            }
            writeln!(f)?;

            if let Some(annot) =
                self.annotations.get(&AnnotationLocation::After(index))
            {
                for line in annot.split('\n') {
                    writeln!(f, "// {line}")?;
                }
            }
        }
        Ok(())
    }
}

impl Display for VMArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VMArg::Const(prim) => write!(f, "{prim}"),
            VMArg::SavedValue(index) => write!(f, "{index}"),
        }
    }
}
impl Display for InstructionIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "op_{}", self.0)
    }
}
impl Display for FunctionIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "native_functions[{}]", self.0)
    }
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::NoOp => {
                write!(f, "{{}}")
            }

            Instruction::Clear { loc } => {
                write!(f, "{loc} = None;")
            }

            Instruction::ConditionalJump { cond, dest } => {
                write!(f, "if {cond} {{ goto {dest} }}")
            }
            Instruction::Copy { value, output } => {
                write!(f, "{output} = {value}")
            }
            Instruction::Swap(lhs, rhs) => {
                write!(f, "({rhs}, {lhs}) = ({lhs}, {rhs})")
            }
            Instruction::NativeFunctionCall {
                index,
                args,
                output,
            } => {
                if let Some(output) = output {
                    write!(f, "{output} = ")?;
                }
                write!(f, "{index}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{arg}")?;
                }
                write!(f, ")")
            }
            Instruction::PrimCast {
                value,
                prim_type,
                output,
            } => write!(f, "{output} = {value}.prim_cast({prim_type})"),

            Instruction::IsSome { value, output } => {
                write!(f, "{output} = {value}.is_some()")
            }

            Instruction::And { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} && {rhs}")
            }

            Instruction::Or { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} || {rhs}")
            }

            Instruction::Not { arg, output } => {
                write!(f, "{output} = !{arg}")
            }

            Instruction::Equal { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} == {rhs}")
            }
            Instruction::NotEqual { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} != {rhs}")
            }
            Instruction::LessThan { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} < {rhs}")
            }
            Instruction::GreaterThan { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} > {rhs}")
            }
            Instruction::LessThanOrEqual { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} <= {rhs}")
            }
            Instruction::GreaterThanOrEqual { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} >= {rhs}")
            }

            Instruction::Add { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} + {rhs}")
            }
            Instruction::Sub { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} - {rhs}")
            }
            Instruction::Mul { lhs, rhs, output } => {
                write!(f, "{output} = {lhs}*{rhs}")
            }
            Instruction::Div { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} / {rhs}")
            }
            Instruction::Mod { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} % {rhs}")
            }

            Instruction::IsSubclassOf {
                method_table_ptr,
                base_type,
                output,
            } => write!(
                f,
                "{output} = {method_table_ptr}.is_subclass_of::<{base_type}>()"
            ),

            Instruction::ReadBytes { regions, output }
                if regions.len() == 1 =>
            {
                let ptr = &regions[0].ptr;
                let num_bytes = &regions[0].num_bytes;
                write!(f, "{output} = {ptr}.read_bytes({num_bytes})")
            }

            Instruction::ReadBytes { regions, output } => {
                write!(f, "{output} = read_bytes(")?;
                regions.iter().enumerate().try_for_each(|(i, region)| {
                    write!(f, "{}, {}", region.ptr, region.num_bytes)?;
                    if i + 1 < regions.len() {
                        write!(f, ", ")?;
                    }
                    Ok(())
                })?;
                write!(f, ")")?;
                Ok(())
            }

            Instruction::CastBytes {
                bytes,
                offset,
                prim_type,
                output,
            } => write!(
                f,
                "{output} = {bytes}.cast_bytes::<{prim_type}>({offset})"
            ),

            Instruction::ReadString { ptr, output } => {
                write!(f, "{output} = {ptr}.read_string()")
            }

            Instruction::Return { outputs } => match outputs.len() {
                0 => write!(f, "return"),
                1 => write!(f, "return {}", outputs[0]),
                _ => {
                    write!(f, "return (")?;
                    outputs.iter().enumerate().try_for_each(
                        |(i, output)| {
                            if i > 0 {
                                write!(f, ", ")?;
                            }
                            write!(f, "{output}")
                        },
                    )?;
                    write!(f, ")")?;
                    Ok(())
                }
            },
        }
    }
}
