use std::{
    any::Any,
    collections::HashMap,
    fmt::Display,
    ops::{Mul, Range, RangeFrom},
};

use derive_more::derive::From;
use memory_reader::{OwnedBytes, Pointer};
use thiserror::Error;

use crate::{
    runtime_type::{RuntimePrimType, RustType},
    CachedReader, Error, MethodTable, RuntimePrimValue, RuntimeString,
    RuntimeType, TypedPointer,
};

use super::{
    native_function::{NativeFunction, WrappedNativeFunction},
    ExposedNativeFunction, ExposedNativeObject, RustNativeObject,
};

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
    entry_point: InstructionIndex,
    reader: Box<dyn VMReader + 'a>,
}

#[derive(Debug, From)]
pub enum StackValue {
    Prim(RuntimePrimValue),
    Native(ExposedNativeObject),
}

pub struct VMResults {
    values: Vec<Option<StackValue>>,
    num_instructions_evaluated: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, From)]
pub enum VMArg {
    Const(RuntimePrimValue),
    SavedValue(StackIndex),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstructionIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct StackIndex(pub usize);

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

    /// Downcast a type.  Assumes the register contains a pointer to an
    /// object.  If the object is of type `ty`, then the register is
    /// unchanged.  If the object is not of type `ty`, then the
    /// register is replaced with `None`.
    ///
    /// TODO: If/when conditionals are implemented, express this as a
    /// Read(RuntimeType::Ptr), followed by a boolean check IsBaseOf.
    /// That way, multiple type checks on the same object can share the
    /// same Read of the method table pointer.
    Downcast {
        obj: VMArg,
        subtype: TypedPointer<MethodTable>,
        output: StackIndex,
    },

    /// Read a value.
    ///
    /// Assumes the register contains a pointer.  Reads a value of the
    /// specified type from the register's location into the register.
    ///
    /// TODO: Change Read to produce a byte array, followed by a later
    /// Cast operator the operates on a subset of those bytes.  This
    /// would allow access of multiple fields of an object to share the
    /// same read.
    Read {
        ptr: VMArg,
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

#[derive(Error)]
pub enum VMExecutionError {
    #[error("Virtual machine did not contain a function named '{0}'")]
    NoSuchFunction(String),

    #[error("Cannot define function {0} multiple times")]
    DuplicateFunctionName(String),

    #[error(
        "Offset must be applied to Ptr, \
         but was instead applied to {0}."
    )]
    OffsetAppliedToNonPointer(RuntimePrimType),

    #[error(
        "DynamicOffset requires the index \
         to have been previously computed, \
         but location {0} was empty."
    )]
    DynamicOffsetWasEmpty(usize),

    #[error(
        "DynamicScale requires the index \
         to have been previously computed, \
         but location {0} was empty."
    )]
    DynamicScaleWasEmpty(usize),

    #[error(
        "DynamicOffset requires the index \
         to be convertible to a usize, \
         but index instead contained {0}."
    )]
    DynamicOffsetNotConvertibleToIndex(RuntimePrimValue),

    #[error(
        "Downcast requires the register to contain a pointer, \
         but instead register contained {0}."
    )]
    DowncastAppliedToNonPointer(RuntimePrimValue),

    #[error(
        "Downcast requires the register to contain a pointer, \
         but instead register contained {0}."
    )]
    ReadAppliedToNonPointer(RuntimePrimValue),

    #[error("Local evaluation of VM may not perform any reads.")]
    ReadOccurredDuringLocalVMEvaluation,

    #[error(
        "ConditionalJump requires the register \
         to contain a boolean value.  \
         However, it contained an instance of type {0}."
    )]
    InvalidOperandForConditionalJump(RuntimePrimType),

    #[error(
        "Native function expecting {expected} arguments \
         was provided with {provided} arguments."
    )]
    InvalidNumberOfOperandsForNativeFunction {
        expected: usize,
        provided: usize,
    },

    #[error(
        "Operator {operator} in instruction {index} \
         does not support operands \
         of rust-native type {arg_type:?}"
    )]
    OperatorExpectsPrimitiveArgument {
        operator: &'static str,
        index: InstructionIndex,
        arg_type: RuntimeType,
    },

    #[error(
        "Rust-native function expected argument of type {expected}, \
         but received argument of type {actual}."
    )]
    InvalidArgumentForNativeFunction {
        expected: RuntimeType,
        actual: RuntimeType,
    },

    #[error("Cannot initialize vector with element type {0}")]
    IllegalVectorElementType(RuntimeType),

    #[error(
        "Vector was expected to be type {expected}, \
         but was instead {actual}."
    )]
    IncorrectVectorType {
        expected: RuntimeType,
        actual: RuntimeType,
    },

    #[error(
        "Item of type {item_type} cannot be pushed \
         into Vec<{element_type}>."
    )]
    IncorrectVectorElementType {
        element_type: RuntimeType,
        item_type: RuntimeType,
    },

    #[error(
        "Expected vector into which to accumulate, \
         but received None."
    )]
    ExpectedVectorToAccumulateInto,

    #[error(
        "When pushing into vector, \
         pushed element must not be None."
    )]
    MissingElementTypeInVectorAccumulation,

    #[error(
        "Attempted to read output as {attempted}, \
         but the output was of type {actual}."
    )]
    IncorrectOutputType {
        attempted: RuntimeType,
        actual: RuntimeType,
    },

    #[error(
        "Reached the last instruction \
         without encountering a Return."
    )]
    ReachedEndWithoutReturnInstruction,
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

impl VMResults {
    pub fn get(&self, index: impl NormalizeStackIndex) -> Option<&StackValue> {
        let index = index.normalize_stack_index();
        self[index].as_ref()
    }

    pub fn get_as<'a, T>(
        &'a self,
        index: impl NormalizeStackIndex,
    ) -> Result<Option<T>, Error>
    where
        &'a StackValue: TryInto<T, Error = Error>,
    {
        let index = index.normalize_stack_index();
        self[index]
            .as_ref()
            .map(|value| value.try_into())
            .transpose()
    }

    pub fn get_any<'a>(
        &'a self,
        index: impl NormalizeStackIndex,
    ) -> Result<Option<&'a dyn Any>, Error> {
        self.get_as(index)
    }

    pub fn take_obj<T: RustNativeObject>(
        &mut self,
        index: impl NormalizeStackIndex,
    ) -> Result<Option<T>, Error> {
        let index = index.normalize_stack_index();

        Ok(self[index]
            .take()
            .map(|value| match value {
                StackValue::Native(native) => {
                    native.downcast::<T>().map_err(|native| {
                        VMExecutionError::IncorrectOutputType {
                            attempted: RustType::new::<T>().into(),
                            actual: native.runtime_type(),
                        }
                    })
                }
                StackValue::Prim(value) => {
                    Err(VMExecutionError::IncorrectOutputType {
                        attempted: RustType::new::<T>().into(),
                        actual: RuntimeType::Prim(value.runtime_type()),
                    })
                }
            })
            .transpose()?
            .map(|boxed| *boxed))
    }

    pub fn get_obj<'a, T: RustNativeObject>(
        &'a self,
        index: impl NormalizeStackIndex,
    ) -> Result<Option<&'a T>, Error> {
        let index = index.normalize_stack_index();
        let opt_value = self[index].as_ref();

        let opt_obj = opt_value
            .map(|value| match value {
                StackValue::Native(native) => native
                    .downcast_ref::<T>()
                    .ok_or_else(|| VMExecutionError::IncorrectOutputType {
                        attempted: RustType::new::<T>().into(),
                        actual: native.runtime_type(),
                    }),
                StackValue::Prim(value) => {
                    Err(VMExecutionError::IncorrectOutputType {
                        attempted: RustType::new::<T>().into(),
                        actual: RuntimeType::Prim(value.runtime_type()),
                    })
                }
            })
            .transpose()?;

        Ok(opt_obj)
    }

    fn collect_native_function_args<'a>(
        &'a mut self,
        args: &[VMArg],
        scratch: &'a mut Vec<Option<StackValue>>,
    ) -> Vec<&'a mut Option<StackValue>> {
        let mut opt_references: Vec<_> =
            (0..args.len()).map(|_| None).collect();

        struct ArgInfo {
            arg_index: usize,
            stack_index: StackIndex,
        }

        let mut indices: Vec<ArgInfo> = Vec::new();

        let initial_scratch_size = scratch.len();

        args.iter().cloned().enumerate().for_each(
            |(arg_index, arg)| match arg {
                VMArg::Const(prim) => {
                    scratch.push(Some(StackValue::Prim(prim)));
                }
                VMArg::SavedValue(stack_index) => {
                    indices.push(ArgInfo {
                        arg_index,
                        stack_index,
                    });
                }
            },
        );

        {
            // The scratch space now contains a copy of each constant.
            // The list of references may now be populated with
            // references to those constants.  After this point, no
            // further elements may be pushed onto the scratch vector.
            let mut remaining_scratch = &mut scratch[initial_scratch_size..];
            for (i, arg) in args.iter().enumerate() {
                if matches!(arg, VMArg::Const(_)) {
                    let (scratch_arg, rest) = remaining_scratch
                        .split_first_mut()
                        .expect("Each VMArg::Const exists in the scratch Vec");
                    opt_references[i] = Some(scratch_arg);
                    remaining_scratch = rest;
                }
            }
        }

        let mut sort_indices: Vec<_> = (0..indices.len()).collect();
        sort_indices.sort_by_key(|i| indices[*i].stack_index.0);

        let mut remaining = &mut self.values[..];
        let mut prev_index: usize = 0;

        for sort_index in sort_indices {
            let stack_index = indices[sort_index].stack_index;
            let rel_index = (stack_index.0 + 1)
                .checked_sub(prev_index)
                .expect("Internal error, indices should be sorted");

            // The `.split_at_mut_checked` method is the trick
            // required to turn one mutable reference into several.
            // Normally, the expression `&mut slice[index]` would
            // borrow the entire `slice` for the duration of the
            // element's borrow.  However, `.split_at_mut_checked`
            // returns two independent borrows, one for each side of
            // the split.
            //
            // This function requires the indices to be unique, such
            // that each mutable reference is unique.  If the indices
            // are not unique, then `rel_index` will be zero for the
            // repeated index.  When `rel_index` is zero,
            // `lhs.last_mut()` will return `None`, preventing the
            // duplicate mutable reference from being created.  If
            // this error occurs at runtime, it means that the
            // compilation produced an invalid
            // Instruction::NativeFunctionCall that contained repeated
            // indices in the argument list.
            let (lhs, rhs) = remaining
                .split_at_mut_checked(rel_index)
                .expect("StackIndex must be in-bound");
            let ref_at_index =
                lhs.last_mut().expect("Indices should be unique");
            let arg_index = indices[sort_index].arg_index;
            opt_references[arg_index] = Some(ref_at_index);
            remaining = rhs;
            prev_index = stack_index.0 + 1;
        }

        // Now that all `Option<&mut StackValue>` elements have been
        // populated, they can be unwrapped to produce the
        // NativeFunction arguments.
        let references: Vec<_> = opt_references
            .into_iter()
            .map(|opt| {
                opt.expect("All references should be filled at this point")
            })
            .collect();

        references
    }

    pub fn num_instructions_evaluated(&self) -> usize {
        self.num_instructions_evaluated
    }
}

impl StackValue {
    pub(crate) fn runtime_type(&self) -> RuntimeType {
        match self {
            StackValue::Prim(prim) => prim.runtime_type().into(),
            StackValue::Native(native) => native.ty.clone(),
        }
    }

    pub fn as_prim(&self) -> Option<RuntimePrimValue> {
        match self {
            StackValue::Prim(prim) => Some(*prim),
            StackValue::Native(_) => None,
        }
    }

    pub fn as_native<T: RustNativeObject>(&self) -> Option<&T> {
        match self {
            StackValue::Native(native) => native.downcast_ref(),
            StackValue::Prim(_) => None,
        }
    }

    pub fn read_string_ptr(
        &self,
        reader: &memory_reader::MemoryReader,
    ) -> Result<String, Error> {
        match self {
            StackValue::Prim(prim) => prim.read_string_ptr(reader),
            StackValue::Native(obj) => Err(
                Error::AttemptedReadOfNativeObjectAsStringPtr(obj.type_id()),
            ),
        }
    }
}

impl VirtualMachineBuilder {
    pub(crate) fn mark_entry_point(
        &mut self,
        name: impl Into<String>,
    ) -> Result<(), Error> {
        let name = name.into();
        if self.entry_points.contains_key(&name) {
            Err(VMExecutionError::DuplicateFunctionName(name).into())
        } else {
            self.entry_points.insert(name, self.current_index());
            Ok(())
        }
    }

    pub(crate) fn push(&mut self, inst: Instruction) -> InstructionIndex {
        let index = InstructionIndex(self.instructions.len());
        self.instructions.push(inst);
        index
    }

    pub(crate) fn update(
        &mut self,
        index: InstructionIndex,
        inst: Instruction,
    ) {
        self.instructions[index.0] = inst;
    }

    pub(crate) fn current_index(&self) -> InstructionIndex {
        InstructionIndex(self.instructions.len())
    }

    pub(crate) fn annotate(
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

pub trait VMReader {
    fn read_bytes(
        &mut self,
        loc: Pointer,
        output: &mut [u8],
    ) -> Result<(), Error>;

    // TODO: Remove the need for this method.  Would be better for
    // everything to be implemented/implementable in terms of
    // `read_bytes`.
    fn is_dotnet_base_class_of(
        &mut self,
        parent_class_ptr: TypedPointer<MethodTable>,
        child_class_ptr: TypedPointer<MethodTable>,
    ) -> Result<bool, Error>;
}

impl VMReader for CachedReader<'_> {
    fn read_bytes(
        &mut self,
        loc: Pointer,
        output: &mut [u8],
    ) -> Result<(), Error> {
        let reader: &memory_reader::MemoryReader = self.as_ref();
        Ok(reader.read_exact(loc, output)?)
    }

    fn is_dotnet_base_class_of(
        &mut self,
        parent_class_ptr: TypedPointer<MethodTable>,
        child_class_ptr: TypedPointer<MethodTable>,
    ) -> Result<bool, Error> {
        self.is_base_of(parent_class_ptr, child_class_ptr)
    }
}

struct DummyReader;
impl VMReader for DummyReader {
    fn read_bytes(
        &mut self,
        _loc: Pointer,
        _output: &mut [u8],
    ) -> Result<(), Error> {
        Err(Error::VMExecutionError(
            VMExecutionError::ReadOccurredDuringLocalVMEvaluation,
        ))
    }

    fn is_dotnet_base_class_of(
        &mut self,
        _parent_class_ptr: TypedPointer<MethodTable>,
        _child_class_ptr: TypedPointer<MethodTable>,
    ) -> Result<bool, Error> {
        Err(Error::VMExecutionError(
            VMExecutionError::ReadOccurredDuringLocalVMEvaluation,
        ))
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
        let entry_point = self
            .entry_points
            .get(name)
            .ok_or_else(|| VMExecutionError::NoSuchFunction(name.into()))?
            .clone();

        Ok(VMEvaluator {
            vm: self,
            entry_point,
            reader: Box::new(DummyReader),
        })
    }

    /// Evaluate the virtual machine, raising an error if any
    /// instructions attempt to read from the remote process.
    pub fn local_eval(&self) -> Result<VMResults, Error> {
        self.get_function("main")?.evaluate()
    }

    /// Evaluate the virtual machine, reading from the remote process
    /// as necessary.
    pub fn evaluate(&self, reader: impl VMReader) -> Result<VMResults, Error> {
        self.get_function("main")?.with_reader(reader).evaluate()
    }
}

impl<'a> VMEvaluator<'a> {
    pub fn with_reader(self, reader: impl VMReader + 'a) -> VMEvaluator<'a> {
        VMEvaluator {
            reader: Box::new(reader),
            ..self
        }
    }

    pub fn evaluate(mut self) -> Result<VMResults, Error> {
        let mut values = {
            let stack = (0..self.vm.stack_size).map(|_| None).collect();
            VMResults {
                values: stack,
                num_instructions_evaluated: 0,
            }
        };

        let mut current_instruction = self.entry_point;
        while current_instruction.0 < self.vm.instructions.len() {
            let instruction = &self.vm.instructions[current_instruction.0];
            let mut next_instruction =
                InstructionIndex(current_instruction.0 + 1);
            values.num_instructions_evaluated += 1;

            macro_rules! arg_to_prim {
                ($arg:expr, $operator:expr) => {{
                    let arg: &VMArg = $arg;
                    let prim:Option<RuntimePrimValue> = match arg {
                        VMArg::Const(value) => Ok(Some(*value)),
                        VMArg::SavedValue(index) => {
                            match &values[*index] {
                                Some(StackValue::Prim(prim)) => Ok(Some(*prim)),
                                Some(StackValue::Native(native)) => Err(
                                    VMExecutionError
                                        ::OperatorExpectsPrimitiveArgument {
                                            operator: $operator,
                                            index: current_instruction,
                                            arg_type: native.ty.clone(),
                                    },
                                ),
                                None => Ok(None),
                            }
                        }
                    }?;
                    prim
                }};
            }

            macro_rules! comparison_op {
                ($lhs:expr, $rhs:expr, $output:expr, $name:literal, $cmp:ident) => {{
                    let lhs = $lhs;
                    let rhs = $rhs;
                    let output = $output;

                    let opt_lhs = arg_to_prim!(lhs, $name);
                    let opt_rhs = arg_to_prim!(rhs, $name);

                    values[*output] = match (opt_lhs, opt_rhs) {
                        (Some(lhs), Some(rhs)) => {
                            let res = match (lhs, rhs) {
                                (
                                    RuntimePrimValue::NativeUInt(a),
                                    RuntimePrimValue::NativeUInt(b),
                                ) => Ok(RuntimePrimValue::Bool(a.$cmp(&b))),
                                _ => Err(
                                    Error::InvalidOperandsForNumericComparison {
                                        lhs: lhs.runtime_type().into(),
                                        rhs: rhs.runtime_type().into(),
                                    },
                                ),
                            }?;
                            Some(res.into())
                        }
                        _ => None,
                    };
                }};
            }

            macro_rules! integer_op {
                ($lhs:expr, $rhs:expr, $output:expr, $name:literal, $op:ident) => {{
                    let lhs = $lhs;
                    let rhs = $rhs;
                    let output = $output;
                    let opt_lhs = arg_to_prim!(lhs, $name);
                    let opt_rhs = arg_to_prim!(rhs, $name);
                    values[*output] = match (opt_lhs, opt_rhs) {
                        (Some(lhs), Some(rhs)) => {
                            let res = match (lhs, rhs) {
                                (
                                    RuntimePrimValue::NativeUInt(a),
                                    RuntimePrimValue::NativeUInt(b),
                                ) => Ok(RuntimePrimValue::NativeUInt(a.$op(b))),
                                (lhs, rhs) => {
                                    Err(Error::InvalidOperandsForBinaryOp {
                                        op: instruction.op_name(),
                                        lhs: lhs.runtime_type().into(),
                                        rhs: rhs.runtime_type().into(),
                                    })
                                }
                            }?;
                            Some(res.into())
                        }
                        _ => None,
                    };
                }};
            }

            match instruction {
                Instruction::NoOp => {}
                Instruction::Clear { loc } => {
                    values[*loc] = None;
                }
                Instruction::Copy { value, output } => {
                    let value = arg_to_prim!(value, "Copy");
                    values[*output] = value.map(Into::into);
                }
                &Instruction::Swap(lhs, rhs) => {
                    if lhs != rhs {
                        let (a, b) = if lhs.0 < rhs.0 {
                            (lhs, rhs)
                        } else {
                            (rhs, lhs)
                        };

                        let (a_slice, b_slice) =
                            values[a..].split_at_mut(b - a);
                        let a_mut = a_slice.first_mut().unwrap();
                        let b_mut = b_slice.first_mut().unwrap();
                        std::mem::swap(a_mut, b_mut);
                    }
                }

                Instruction::ConditionalJump { cond, dest } => {
                    let cond = arg_to_prim!(cond, "ConditionalJump");
                    let should_jump = cond
                        .map(|val| match val {
                            RuntimePrimValue::Bool(val) => Ok(val),
                            other => Err(
                                VMExecutionError::InvalidOperandForConditionalJump(
                                    other.runtime_type()
                                ),
                            ),
                        })
                        .transpose()?
                        .unwrap_or(false);
                    if should_jump {
                        next_instruction = *dest;
                    }
                }

                Instruction::NativeFunctionCall {
                    index,
                    args,
                    output,
                } => {
                    let mut inline_consts = Vec::new();
                    let mut args = values
                        .collect_native_function_args(args, &mut inline_consts);
                    let result =
                        self.vm.native_functions[index.0].apply(&mut args)?;
                    if let Some(output) = output {
                        values[*output] = result;
                    }
                }

                Instruction::PrimCast {
                    value,
                    prim_type,
                    output,
                } => {
                    let value = arg_to_prim!(value, "PrimCast");
                    values[*output] = value
                        .map(|val| val.prim_cast(*prim_type))
                        .transpose()?
                        .map(Into::into);
                }

                Instruction::IsSome { value, output } => {
                    let is_some: bool = match value {
                        VMArg::Const(_) => true,
                        VMArg::SavedValue(stack_index) => {
                            values[*stack_index].is_some()
                        }
                    };
                    values[*output] =
                        Some(RuntimePrimValue::Bool(is_some).into());
                }

                Instruction::Add { lhs, rhs, output } => {
                    let opt_lhs = arg_to_prim!(lhs, "Add");
                    let opt_rhs = arg_to_prim!(rhs, "Add");
                    values[*output] = match (opt_lhs, opt_rhs) {
                        (Some(lhs), Some(rhs)) => {
                            let res = match (lhs, rhs) {
                                (
                                    RuntimePrimValue::NativeUInt(a),
                                    RuntimePrimValue::NativeUInt(b),
                                ) => Ok(RuntimePrimValue::NativeUInt(a + b)),
                                (
                                    RuntimePrimValue::Ptr(a),
                                    RuntimePrimValue::NativeUInt(b),
                                ) => Ok(RuntimePrimValue::Ptr(a + b)),
                                (
                                    RuntimePrimValue::NativeUInt(a),
                                    RuntimePrimValue::Ptr(b),
                                ) => Ok(RuntimePrimValue::Ptr(b + a)),
                                (lhs, rhs) => {
                                    Err(Error::InvalidOperandsForBinaryOp {
                                        op: instruction.op_name(),
                                        lhs: lhs.runtime_type().into(),
                                        rhs: rhs.runtime_type().into(),
                                    })
                                }
                            }?;
                            Some(res.into())
                        }
                        _ => None,
                    };
                }
                Instruction::Sub { lhs, rhs, output } => {
                    let opt_lhs = arg_to_prim!(lhs, "Sub");
                    let opt_rhs = arg_to_prim!(rhs, "Sub");
                    values[*output] = match (opt_lhs, opt_rhs) {
                        (Some(lhs), Some(rhs)) => {
                            let res = match (lhs, rhs) {
                                (
                                    RuntimePrimValue::NativeUInt(a),
                                    RuntimePrimValue::NativeUInt(b),
                                ) => Ok(RuntimePrimValue::NativeUInt(a - b)),
                                (
                                    RuntimePrimValue::Ptr(a),
                                    RuntimePrimValue::NativeUInt(b),
                                ) => Ok(RuntimePrimValue::Ptr(a - b)),
                                (lhs, rhs) => {
                                    Err(Error::InvalidOperandsForBinaryOp {
                                        op: instruction.op_name(),
                                        lhs: lhs.runtime_type().into(),
                                        rhs: rhs.runtime_type().into(),
                                    })
                                }
                            }?;
                            Some(res.into())
                        }
                        _ => None,
                    };
                }
                Instruction::Mul { lhs, rhs, output } => {
                    integer_op! {lhs, rhs, output, "Mul", mul}
                }
                Instruction::Div { lhs, rhs, output } => {
                    integer_op! {lhs, rhs, output, "Div", div_euclid}
                }
                Instruction::Mod { lhs, rhs, output } => {
                    integer_op! {lhs, rhs, output, "Mod", rem_euclid}
                }

                Instruction::And { lhs, rhs, output } => {
                    let opt_lhs = arg_to_prim!(lhs, "And");
                    let opt_rhs = arg_to_prim!(rhs, "And");
                    values[*output] = match (opt_lhs, opt_rhs) {
                        (None, Some(RuntimePrimValue::Bool(false)))
                        | (Some(RuntimePrimValue::Bool(false)), None) => {
                            Some(RuntimePrimValue::Bool(false).into())
                        }
                        (Some(lhs), Some(rhs)) => {
                            let res = match (lhs, rhs) {
                                (
                                    RuntimePrimValue::Bool(a),
                                    RuntimePrimValue::Bool(b),
                                ) => Ok(RuntimePrimValue::Bool(a && b)),
                                _ => Err(Error::InvalidOperandsForBinaryOp {
                                    op: instruction.op_name(),
                                    lhs: lhs.runtime_type().into(),
                                    rhs: rhs.runtime_type().into(),
                                }),
                            }?;
                            Some(res.into())
                        }
                        _ => None,
                    };
                }

                Instruction::Or { lhs, rhs, output } => {
                    let opt_lhs = arg_to_prim!(lhs, "Or");
                    let opt_rhs = arg_to_prim!(rhs, "Or");
                    values[*output] = match (opt_lhs, opt_rhs) {
                        (None, Some(RuntimePrimValue::Bool(true)))
                        | (Some(RuntimePrimValue::Bool(true)), None) => {
                            Some(RuntimePrimValue::Bool(true).into())
                        }
                        (Some(lhs), Some(rhs)) => {
                            let res = match (lhs, rhs) {
                                (
                                    RuntimePrimValue::Bool(a),
                                    RuntimePrimValue::Bool(b),
                                ) => Ok(RuntimePrimValue::Bool(a || b)),
                                _ => Err(Error::InvalidOperandsForBinaryOp {
                                    op: instruction.op_name(),
                                    lhs: lhs.runtime_type().into(),
                                    rhs: rhs.runtime_type().into(),
                                }),
                            }?;
                            Some(res.into())
                        }
                        _ => None,
                    };
                }

                Instruction::Not { arg, output } => {
                    let opt_arg = arg_to_prim!(arg, "Not");
                    values[*output] = match opt_arg {
                        Some(RuntimePrimValue::Bool(b)) => {
                            Ok(Some(RuntimePrimValue::Bool(!b).into()))
                        }
                        Some(other) => Err(Error::InvalidOperandForUnaryOp {
                            op: instruction.op_name(),
                            arg: other.runtime_type().into(),
                        }),
                        None => Ok(None),
                    }?;
                }

                Instruction::Equal { lhs, rhs, output } => {
                    comparison_op! {lhs, rhs, output, "Equal", eq}
                }
                Instruction::NotEqual { lhs, rhs, output } => {
                    comparison_op! {lhs, rhs, output, "NotEqual", ne}
                }
                Instruction::LessThan { lhs, rhs, output } => {
                    comparison_op! {lhs, rhs, output, "LessThan", lt}
                }
                Instruction::LessThanOrEqual { lhs, rhs, output } => {
                    comparison_op! {lhs, rhs, output, "LessThanOrEqual", le}
                }
                Instruction::GreaterThan { lhs, rhs, output } => {
                    comparison_op! {lhs, rhs, output, "GreaterThan", gt}
                }
                Instruction::GreaterThanOrEqual { lhs, rhs, output } => {
                    comparison_op! {lhs, rhs, output, "GreaterThanOrEqual", ge}
                }

                Instruction::Downcast {
                    obj,
                    subtype,
                    output,
                } => {
                    let obj = arg_to_prim!(obj, "Downcast");
                    values[*output] = match obj {
                        None => None,
                        Some(RuntimePrimValue::Ptr(ptr)) => {
                            let actual_type_ptr: Pointer = {
                                let mut arr = [0; Pointer::SIZE];
                                self.reader.read_bytes(ptr, &mut arr)?;
                                arr.into()
                            };
                            self.reader
                                .is_dotnet_base_class_of(
                                    *subtype,
                                    actual_type_ptr.into(),
                                )?
                                .then(|| RuntimePrimValue::Ptr(ptr).into())
                        }
                        Some(other) => {
                            return Err(
                                VMExecutionError::DowncastAppliedToNonPointer(
                                    other,
                                )
                                .into(),
                            );
                        }
                    };
                }
                Instruction::Read {
                    ptr,
                    prim_type,
                    output,
                } => {
                    let ptr = arg_to_prim!(ptr, "Read");
                    values[*output] = match ptr {
                        None => None,
                        Some(RuntimePrimValue::Ptr(ptr)) => {
                            let bytes = {
                                let mut vec = vec![0; prim_type.size_bytes()];
                                self.reader.read_bytes(ptr, &mut vec)?;
                                vec
                            };
                            let prim_value = prim_type.parse(&bytes)?;
                            match prim_value {
                                RuntimePrimValue::Ptr(ptr) if ptr.is_null() => {
                                    None
                                }
                                other => Some(other.into()),
                            }
                        }
                        Some(other) => {
                            return Err(
                                VMExecutionError::ReadAppliedToNonPointer(
                                    other,
                                )
                                .into(),
                            );
                        }
                    };
                }
                Instruction::ReadString { ptr, output } => {
                    let opt_prim = arg_to_prim!(ptr, "ReadString");
                    values[*output] = opt_prim
                        .map(|prim| -> Result<_, Error> {
                            let ptr: Pointer = prim.try_into()?;
                            let runtime_string = RuntimeString::read_string(
                                ptr,
                                |byte_range| -> Result<OwnedBytes, Error> {
                                    let num_bytes =
                                        byte_range.end - byte_range.start;
                                    let mut bytes = vec![0; num_bytes];
                                    self.reader.read_bytes(
                                        byte_range.start,
                                        &mut bytes,
                                    )?;
                                    Ok(OwnedBytes::new(byte_range.start, bytes))
                                },
                            )?;
                            let string: String = runtime_string.into();
                            Ok(ExposedNativeObject::new(string).into())
                        })
                        .transpose()?;
                }

                Instruction::Return { outputs } => {
                    let mut inline_consts = Vec::new();
                    let mut collected_outputs = values
                        .collect_native_function_args(
                            outputs,
                            &mut inline_consts,
                        );
                    let outputs = collected_outputs
                        .iter_mut()
                        .map(|opt_mut| opt_mut.take())
                        .collect();
                    return Ok(VMResults {
                        values: outputs,
                        ..values
                    });
                }
            }

            current_instruction = next_instruction;
        }

        Err(VMExecutionError::ReachedEndWithoutReturnInstruction.into())
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
            | Instruction::Downcast { obj: arg, .. }
            | Instruction::Read { ptr: arg, .. }
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

            Instruction::Swap(lhs, rhs) => (
                Some(VMArg::SavedValue(*lhs)),
                Some(VMArg::SavedValue(*rhs)),
                None,
            ),

            // Arbitrary arity instructions
            Instruction::NativeFunctionCall { args: values, .. }
            | Instruction::Return { outputs: values } => {
                (None, None, Some(values.iter().cloned()))
            }
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
            | Instruction::Downcast { output, .. }
            | Instruction::Read { output, .. }
            | Instruction::ReadString { output, .. }
            | Instruction::IsSome { output, .. } => (Some(*output), None),

            Instruction::Swap(lhs, rhs) => (Some(*lhs), Some(*rhs)),
        };

        std::iter::empty()
            .chain(opt_a.into_iter())
            .chain(opt_b.into_iter())
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
            Instruction::Downcast { .. } => "Downcast",
            Instruction::Read { .. } => "Read",
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

impl std::fmt::Debug for VMExecutionError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
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
impl Display for StackValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackValue::Prim(prim) => write!(f, "{prim}"),
            StackValue::Native(any) => {
                write!(f, "[rust-native object of type {:?}]", any.type_id())
            }
        }
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
impl Display for StackIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "stack[pc + {}]", self.0)
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

            Instruction::Downcast {
                obj,
                subtype,
                output,
            } => write!(f, "{output} = {obj}.downcast::<{subtype}>()"),
            Instruction::Read {
                ptr,
                prim_type,
                output,
            } => write!(f, "{output} = {ptr}.read({prim_type})"),
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

impl IntoIterator for VMResults {
    type Item = <Vec<Option<StackValue>> as IntoIterator>::Item;

    type IntoIter = <Vec<Option<StackValue>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.values.into_iter()
    }
}

impl std::ops::Deref for VMResults {
    type Target = Vec<Option<StackValue>>;

    fn deref(&self) -> &Self::Target {
        &self.values
    }
}
impl std::ops::DerefMut for VMResults {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.values
    }
}

impl std::ops::Index<usize> for VMResults {
    type Output = Option<StackValue>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.values[index]
    }
}
impl std::ops::IndexMut<usize> for VMResults {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.values[index]
    }
}

impl std::ops::Index<StackIndex> for VMResults {
    type Output = Option<StackValue>;

    fn index(&self, index: StackIndex) -> &Self::Output {
        &self.values[index.0]
    }
}
impl std::ops::IndexMut<StackIndex> for VMResults {
    fn index_mut(&mut self, index: StackIndex) -> &mut Self::Output {
        &mut self.values[index.0]
    }
}
impl std::ops::Index<Range<StackIndex>> for VMResults {
    type Output = [Option<StackValue>];

    fn index(&self, index: std::ops::Range<StackIndex>) -> &Self::Output {
        &self.values[index.start.0..index.end.0]
    }
}
impl std::ops::IndexMut<Range<StackIndex>> for VMResults {
    fn index_mut(&mut self, index: Range<StackIndex>) -> &mut Self::Output {
        &mut self.values[index.start.0..index.end.0]
    }
}
impl std::ops::Index<RangeFrom<StackIndex>> for VMResults {
    type Output = [Option<StackValue>];

    fn index(&self, index: std::ops::RangeFrom<StackIndex>) -> &Self::Output {
        &self.values[index.start.0..]
    }
}
impl std::ops::IndexMut<RangeFrom<StackIndex>> for VMResults {
    fn index_mut(&mut self, index: RangeFrom<StackIndex>) -> &mut Self::Output {
        &mut self.values[index.start.0..]
    }
}

impl std::ops::Add<usize> for StackIndex {
    type Output = StackIndex;

    fn add(self, rhs: usize) -> Self::Output {
        StackIndex(self.0 + rhs)
    }
}
impl std::ops::Sub for StackIndex {
    type Output = usize;

    fn sub(self, rhs: Self) -> Self::Output {
        self.0 - rhs.0
    }
}

impl TryInto<RuntimePrimValue> for StackValue {
    type Error = Error;

    fn try_into(self) -> Result<RuntimePrimValue, Self::Error> {
        match self {
            StackValue::Prim(value) => Ok(value),
            StackValue::Native(any) => {
                Err(Error::AttemptedConversionOfNativeObject(any.type_id()))
            }
        }
    }
}
impl TryInto<RuntimePrimValue> for &'_ StackValue {
    type Error = Error;

    fn try_into(self) -> Result<RuntimePrimValue, Self::Error> {
        match self {
            StackValue::Prim(value) => Ok(*value),
            StackValue::Native(any) => {
                Err(Error::AttemptedConversionOfNativeObject(any.type_id()))
            }
        }
    }
}
impl<'a> TryInto<&'a dyn Any> for &'a StackValue {
    type Error = Error;

    fn try_into(self) -> Result<&'a dyn Any, Self::Error> {
        match self {
            StackValue::Native(native) => Ok(native.as_ref()),
            StackValue::Prim(value) => {
                Err(Error::AttemptedConversionOfPrimitiveToNativeObject(
                    value.runtime_type(),
                ))
            }
        }
    }
}
impl TryInto<Box<dyn Any>> for StackValue {
    type Error = Error;

    fn try_into(self) -> Result<Box<dyn Any>, Self::Error> {
        match self {
            StackValue::Native(native) => Ok(native.into()),
            StackValue::Prim(value) => {
                Err(Error::AttemptedConversionOfPrimitiveToNativeObject(
                    value.runtime_type(),
                ))
            }
        }
    }
}

macro_rules! stack_value_to_prim {
    ($prim:ty) => {
        impl TryInto<$prim> for StackValue {
            type Error = Error;

            fn try_into(self) -> Result<$prim, Self::Error> {
                match self {
                    StackValue::Prim(value) => value.try_into(),
                    StackValue::Native(native) => {
                        Err(Error::AttemptedConversionOfNativeObject(
                            native.type_id(),
                        ))
                    }
                }
            }
        }

        impl TryInto<$prim> for &'_ StackValue {
            type Error = Error;

            fn try_into(self) -> Result<$prim, Self::Error> {
                match self {
                    StackValue::Prim(value) => (*value).try_into(),
                    StackValue::Native(native) => {
                        Err(Error::AttemptedConversionOfNativeObject(
                            native.type_id(),
                        ))
                    }
                }
            }
        }

        impl TryInto<$prim> for VMResults {
            type Error = Error;

            fn try_into(mut self) -> Result<$prim, Self::Error> {
                let num_elements = self.values.len();
                if num_elements == 1 {
                    self.values[0]
                        .take()
                        .ok_or(Error::AttemptedConversionOfMissingValue)?
                        .try_into()
                } else {
                    Err(Error::IncorrectNumberOfResults {
                        expected: 1,
                        actual: num_elements,
                    })
                }
            }
        }
    };
}

stack_value_to_prim!(bool);
stack_value_to_prim!(u8);
stack_value_to_prim!(u16);
stack_value_to_prim!(u32);
stack_value_to_prim!(u64);
stack_value_to_prim!(usize);
stack_value_to_prim!(i8);
stack_value_to_prim!(i16);
stack_value_to_prim!(i32);
stack_value_to_prim!(i64);
stack_value_to_prim!(isize);
stack_value_to_prim!(f32);
stack_value_to_prim!(f64);
stack_value_to_prim!(Pointer);

impl TryInto<Box<dyn Any>> for VMResults {
    type Error = Error;

    fn try_into(mut self) -> Result<Box<dyn Any>, Self::Error> {
        let num_elements = self.values.len();
        if num_elements == 1 {
            self.values[0]
                .take()
                .ok_or(Error::AttemptedConversionOfMissingValue)?
                .try_into()
        } else {
            Err(Error::IncorrectNumberOfResults {
                expected: 1,
                actual: num_elements,
            })
        }
    }
}
