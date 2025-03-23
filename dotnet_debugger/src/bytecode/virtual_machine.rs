use std::{
    any::Any,
    collections::HashMap,
    fmt::Display,
    ops::{Range, RangeFrom},
};

use derive_more::derive::From;
use memory_reader::{OwnedBytes, Pointer};
use thiserror::Error;

use crate::{
    runtime_type::RuntimePrimType, CachedReader, Error, MethodTable,
    RuntimePrimValue, RuntimeString, RuntimeType, TypedPointer,
};

use super::{
    native_function::{NativeFunction, WrappedNativeFunction},
    ExposedNativeFunction,
};

pub struct VirtualMachineBuilder {
    instructions: Vec<Instruction>,
    native_functions: Vec<ExposedNativeFunction>,
    num_outputs: usize,
}

#[derive(Debug)]
pub struct VirtualMachine {
    /// The instructions to execute in the virtual machine.
    instructions: Vec<Instruction>,

    /// Functions in Rust that may be called from the bytecode.
    native_functions: Vec<ExposedNativeFunction>,

    /// The number of output values to produce.
    num_outputs: usize,

    /// The number of additional temporary values to allocate.
    num_temporaries: usize,
}

#[derive(Debug, From)]
pub enum StackValue {
    Prim(RuntimePrimValue),
    Any(Box<dyn Any>),
}

pub struct VMResults(Vec<Option<StackValue>>);

#[derive(Debug, Clone, Copy, PartialEq, From)]
pub enum VMArg {
    Const(RuntimePrimValue),
    SavedValue(StackIndex),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InstructionIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StackIndex(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FunctionIndex(pub usize);

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    // Do nothing.
    NoOp,

    // Returns a copy of the specified value.  Can be used to copy a
    // value on the stack, or to store a constant value onto the
    // stack.
    Copy {
        value: VMArg,
        output: StackIndex,
    },

    // Swap the values stored in two stack locations
    Swap(StackIndex, StackIndex),

    // If the register contains true, jump to the specified
    // instruction.  Otherwise, continue to the next instruction.
    ConditionalJump {
        cond: VMArg,
        dest: InstructionIndex,
    },

    // Call a native function
    NativeFunctionCall {
        index: FunctionIndex,
        args: Vec<VMArg>,
        output: Option<StackIndex>,
    },

    // Cast the value in the register to the specified type.
    PrimCast {
        value: VMArg,
        prim_type: RuntimePrimType,
        output: StackIndex,
    },

    // Check if a location contains a non-None value.
    IsSome {
        value: VMArg,
        output: StackIndex,
    },

    // Increment the register by the value specified in the argument,
    // storing the result back to the register.
    Add {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    // Decrement the register by the value specified in the argument,
    // storing the result back to the register.
    Sub {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    // Multiply the register by the value specified in the argument,
    // storing the result back to the register.
    Mul {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    // Check if the value in the register is equal to the specified
    // argument, storing the resulting boolean back to the register.
    Equal {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    // Check if the value in the register is greater than the
    // specified argument, storing the resulting boolean back to the
    // register.
    LessThan {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    // Check if the value in the register is greater than the
    // specified argument, storing the resulting boolean back to the
    // register.
    GreaterThan {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    GreaterThanOrEqual {
        lhs: VMArg,
        rhs: VMArg,
        output: StackIndex,
    },

    // Downcast a type.  Assumes the register contains a pointer to an
    // object.  If the object is of type `ty`, then the register is
    // unchanged.  If the object is not of type `ty`, then the
    // register is replaced with `None`.
    //
    // TODO: If/when conditionals are implemented, express this as a
    // Read(RuntimeType::Ptr), followed by a boolean check IsBaseOf.
    // That way, multiple type checks on the same object can share the
    // same Read of the method table pointer.
    Downcast {
        obj: VMArg,
        subtype: TypedPointer<MethodTable>,
        output: StackIndex,
    },

    // Read a value.
    //
    // Assumes the register contains a pointer.  Reads a value of the
    // specified type from the register's location into the register.
    //
    // TODO: Change Read to produce a byte array, followed by a later
    // Cast operator the operates on a subset of those bytes.  This
    // would allow access of multiple fields of an object to share the
    // same read.
    Read {
        ptr: VMArg,
        prim_type: RuntimePrimType,
        output: StackIndex,
    },

    ReadString {
        ptr: VMArg,
        output: StackIndex,
    },
}

#[derive(Error)]
pub enum VMExecutionError {
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
        "Operator {operator} does not support operands \
         of rust-native type {arg_type:?}"
    )]
    OperatorExpectsPrimitiveArgument {
        operator: &'static str,
        arg_type: std::any::TypeId,
    },

    #[error(
        "Rust-native function expected argument of type {expected:?}, \
         but received argument of type {actual}."
    )]
    InvalidArgumentForNativeFunction {
        expected: RuntimeType,
        actual: RuntimeType,
    },
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

        let mut remaining = &mut self.0[..];
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
}

impl StackValue {
    pub fn runtime_type(&self) -> RuntimeType {
        match self {
            StackValue::Prim(prim) => prim.runtime_type().into(),
            StackValue::Any(any) => any.type_id().into(),
        }
    }

    pub fn as_prim(&self) -> Option<RuntimePrimValue> {
        match self {
            StackValue::Prim(prim) => Some(*prim),
            StackValue::Any(_) => None,
        }
    }

    pub fn read_string_ptr(
        &self,
        reader: &memory_reader::MemoryReader,
    ) -> Result<String, Error> {
        match self {
            StackValue::Prim(prim) => prim.read_string_ptr(reader),
            StackValue::Any(obj) => Err(
                Error::AttemptedReadOfNativeObjectAsStringPtr(obj.type_id()),
            ),
        }
    }
}

impl VirtualMachineBuilder {
    pub fn num_outputs(self, num_outputs: usize) -> Self {
        Self {
            num_outputs,
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

        assert!(
            num_values >= self.num_outputs,
            "Virtual machine has {}, \
             but only found instructions for writing to {num_values}",
            self.num_outputs
        );

        VirtualMachine {
            instructions: self.instructions,
            native_functions: self.native_functions,
            num_outputs: self.num_outputs,
            num_temporaries: num_values - self.num_outputs,
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
    pub fn builder(instructions: Vec<Instruction>) -> VirtualMachineBuilder {
        VirtualMachineBuilder {
            instructions,
            native_functions: Vec::new(),
            num_outputs: 1,
        }
    }

    pub fn new(instructions: Vec<Instruction>, num_outputs: usize) -> Self {
        Self::builder(instructions).num_outputs(num_outputs).build()
    }

    pub fn num_instructions(&self) -> usize {
        self.instructions.len()
    }

    pub fn simplify(self) -> Self {
        self.remap_temporary_indices()
    }

    pub(crate) fn remap_temporary_indices(self) -> Self {
        let mut remap: HashMap<StackIndex, StackIndex> = (0..self.num_outputs)
            .map(StackIndex)
            .map(|output| (output, output))
            .collect();

        // TODO: After the last usage of an index, allow it to be
        // reused for a different temporary value.
        //
        // TODO: If all occurrences of a temporary value are before
        // the write of an output index, allow the output index to be
        // used as a temporary.
        let mut do_remap = |index: StackIndex| -> StackIndex {
            if let Some(new_index) = remap.get(&index) {
                *new_index
            } else {
                let new_index = StackIndex(remap.len());
                remap.insert(index, new_index);
                new_index
            }
        };

        let instructions = self
            .instructions
            .into_iter()
            .map(|mut inst| {
                inst.visit_indices(|index| {
                    *index = do_remap(*index);
                });
                inst
            })
            .collect();

        Self {
            instructions,
            num_temporaries: remap.len() - self.num_outputs,
            ..self
        }
    }

    pub fn assert_equal(&self, other: &VirtualMachine) {
        assert_eq!(self.num_outputs, other.num_outputs);
        assert_eq!(self.num_temporaries, other.num_temporaries);
        assert_eq!(self.native_functions.len(), other.native_functions.len());
        self.native_functions
            .iter()
            .zip(other.native_functions.iter())
            .for_each(|(self_func, other_func)| {
                assert_eq!(self_func, other_func);
            });

        assert_eq!(self.instructions.len(), other.instructions.len());

        self.instructions
            .iter()
            .zip(other.instructions.iter())
            .for_each(|(self_instruction, other_instruction)| {
                assert_eq!(self_instruction, other_instruction);
            });
    }

    /// Evaluate the virtual machine, raising an error if any
    /// instructions attempt to read from the remote process.
    pub fn local_eval(&self) -> Result<VMResults, Error> {
        self.evaluate(DummyReader)
    }

    pub fn evaluate(
        &self,
        mut reader: impl VMReader,
    ) -> Result<VMResults, Error> {
        let mut values = {
            let stack_size = self.num_outputs + self.num_temporaries;
            let stack = (0..stack_size).map(|_| None).collect();
            VMResults(stack)
        };

        macro_rules! arg_to_prim {
            ($arg:expr, $operator:literal) => {{
                let arg: &VMArg = $arg;
                let prim:Option<RuntimePrimValue> = match arg {
                    VMArg::Const(value) => Ok(Some(*value)),
                    VMArg::SavedValue(index) => {
                        match &values[*index] {
                            Some(StackValue::Prim(prim)) => Ok(Some(*prim)),
                            Some(StackValue::Any(obj)) => Err(
                                VMExecutionError::OperatorExpectsPrimitiveArgument {
                                    operator: $operator,
                                    arg_type: obj.type_id(),
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

        let mut current_instruction = InstructionIndex(0);
        while current_instruction.0 < self.instructions.len() {
            let instruction = &self.instructions[current_instruction.0];
            current_instruction.0 += 1;

            match instruction {
                Instruction::NoOp => {}
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
                        current_instruction = *dest;
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
                        self.native_functions[index.0].apply(&mut args)?;
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
                                    Err(Error::InvalidOperandsForAddition {
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
                                    Err(Error::InvalidOperandsForAddition {
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
                    let opt_lhs = arg_to_prim!(lhs, "Mul");
                    let opt_rhs = arg_to_prim!(rhs, "Mul");
                    values[*output] = match (opt_lhs, opt_rhs) {
                        (Some(lhs), Some(rhs)) => {
                            let res = match (lhs, rhs) {
                                (
                                    RuntimePrimValue::NativeUInt(a),
                                    RuntimePrimValue::NativeUInt(b),
                                ) => Ok(RuntimePrimValue::NativeUInt(a * b)),
                                (lhs, rhs) => Err(
                                    Error::InvalidOperandsForMultiplication {
                                        lhs: lhs.runtime_type().into(),
                                        rhs: rhs.runtime_type().into(),
                                    },
                                ),
                            }?;
                            Some(res.into())
                        }
                        _ => None,
                    };
                }
                Instruction::Equal { lhs, rhs, output } => {
                    comparison_op! {lhs, rhs, output, "Equal", eq}
                }
                Instruction::LessThan { lhs, rhs, output } => {
                    comparison_op! {lhs, rhs, output, "LessThan", lt}
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
                                reader.read_bytes(ptr, &mut arr)?;
                                arr.into()
                            };
                            reader
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
                                reader.read_bytes(ptr, &mut vec)?;
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
                                    reader.read_bytes(
                                        byte_range.start,
                                        &mut bytes,
                                    )?;
                                    Ok(OwnedBytes::new(byte_range.start, bytes))
                                },
                            )?;
                            let string: String = runtime_string.into();
                            Ok(StackValue::Any(Box::new(string)))
                        })
                        .transpose()?;
                }
            }
        }

        values.0.truncate(self.num_outputs);

        Ok(values)
    }
}

impl Instruction {
    fn input_indices(&self) -> impl Iterator<Item = StackIndex> + '_ {
        let (lhs, rhs, dyn_args) = match self {
            // Nullary instructions
            Instruction::NoOp => (None, None, None),

            // Unary instructions
            Instruction::Copy { value: arg, .. }
            | Instruction::ConditionalJump { cond: arg, .. }
            | Instruction::PrimCast { value: arg, .. }
            | Instruction::Downcast { obj: arg, .. }
            | Instruction::Read { ptr: arg, .. }
            | Instruction::ReadString { ptr: arg, .. }
            | Instruction::IsSome { value: arg, .. } => {
                (Some(*arg), None, None)
            }

            // Binary instructions
            Instruction::Add { lhs, rhs, .. }
            | Instruction::Sub { lhs, rhs, .. }
            | Instruction::Mul { lhs, rhs, .. }
            | Instruction::Equal { lhs, rhs, .. }
            | Instruction::LessThan { lhs, rhs, .. }
            | Instruction::GreaterThan { lhs, rhs, .. }
            | Instruction::GreaterThanOrEqual { lhs, rhs, .. } => {
                (Some(*lhs), Some(*rhs), None)
            }

            Instruction::Swap(lhs, rhs) => (
                Some(VMArg::SavedValue(*lhs)),
                Some(VMArg::SavedValue(*rhs)),
                None,
            ),

            // Arbitrary arity instructions
            Instruction::NativeFunctionCall { args, .. } => {
                (None, None, Some(args.iter().cloned()))
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
            | Instruction::ConditionalJump { .. } => (None, None),

            Instruction::Copy { output, .. }
            | Instruction::NativeFunctionCall {
                output: Some(output),
                ..
            }
            | Instruction::PrimCast { output, .. }
            | Instruction::Add { output, .. }
            | Instruction::Sub { output, .. }
            | Instruction::Mul { output, .. }
            | Instruction::Equal { output, .. }
            | Instruction::LessThan { output, .. }
            | Instruction::GreaterThan { output, .. }
            | Instruction::GreaterThanOrEqual { output, .. }
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

    fn visit_indices(&mut self, mut callback: impl FnMut(&mut StackIndex)) {
        match self {
            Instruction::NoOp => {}

            Instruction::IsSome { value: arg, output }
            | Instruction::Copy { value: arg, output }
            | Instruction::PrimCast {
                value: arg, output, ..
            }
            | Instruction::Downcast {
                obj: arg, output, ..
            }
            | Instruction::Read {
                ptr: arg, output, ..
            }
            | Instruction::ReadString { ptr: arg, output } => {
                if let VMArg::SavedValue(index) = arg {
                    callback(index);
                }
                callback(output);
            }
            Instruction::Swap(lhs, rhs) => {
                callback(lhs);
                callback(rhs);
            }
            Instruction::ConditionalJump { cond: arg, .. } => {
                if let VMArg::SavedValue(index) = arg {
                    callback(index);
                }
            }
            Instruction::NativeFunctionCall { args, output, .. } => {
                args.iter_mut()
                    .filter_map(|arg| match arg {
                        VMArg::SavedValue(index) => Some(index),
                        VMArg::Const(_) => None,
                    })
                    .for_each(|arg| callback(arg));
                if let Some(output) = output {
                    callback(output);
                }
            }

            Instruction::Add { lhs, rhs, output }
            | Instruction::Sub { lhs, rhs, output }
            | Instruction::Mul { lhs, rhs, output }
            | Instruction::Equal { lhs, rhs, output }
            | Instruction::LessThan { lhs, rhs, output }
            | Instruction::GreaterThan { lhs, rhs, output }
            | Instruction::GreaterThanOrEqual { lhs, rhs, output } => {
                if let VMArg::SavedValue(index) = lhs {
                    callback(index);
                }
                if let VMArg::SavedValue(index) = rhs {
                    callback(index);
                }
                callback(output);
            }
        }
    }
}

impl From<usize> for VMArg {
    fn from(value: usize) -> Self {
        VMArg::Const(RuntimePrimValue::NativeUInt(value))
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
            "{} instructions, {} outputs, {} temporaries",
            self.instructions.len(),
            self.num_outputs,
            self.num_temporaries
        )?;
        for (i, instruction) in self.instructions.iter().enumerate() {
            writeln!(f, "{i}: {instruction}")?;
        }
        Ok(())
    }
}
impl Display for StackValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            StackValue::Prim(prim) => write!(f, "{prim}"),
            StackValue::Any(any) => {
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

            Instruction::Add { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} + {rhs}")
            }
            Instruction::Sub { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} - {rhs}")
            }
            Instruction::Mul { lhs, rhs, output } => {
                write!(f, "{output} = {lhs}*{rhs}")
            }
            Instruction::Equal { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} == {rhs}")
            }
            Instruction::LessThan { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} < {rhs}")
            }
            Instruction::GreaterThan { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} > {rhs}")
            }
            Instruction::GreaterThanOrEqual { lhs, rhs, output } => {
                write!(f, "{output} = {lhs} >= {rhs}")
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
        }
    }
}

impl IntoIterator for VMResults {
    type Item = <Vec<Option<StackValue>> as IntoIterator>::Item;

    type IntoIter = <Vec<Option<StackValue>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl std::ops::Deref for VMResults {
    type Target = Vec<Option<StackValue>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}
impl std::ops::DerefMut for VMResults {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl std::ops::Index<usize> for VMResults {
    type Output = Option<StackValue>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl std::ops::Index<StackIndex> for VMResults {
    type Output = Option<StackValue>;

    fn index(&self, index: StackIndex) -> &Self::Output {
        &self.0[index.0]
    }
}
impl std::ops::IndexMut<StackIndex> for VMResults {
    fn index_mut(&mut self, index: StackIndex) -> &mut Self::Output {
        &mut self.0[index.0]
    }
}
impl std::ops::Index<Range<StackIndex>> for VMResults {
    type Output = [Option<StackValue>];

    fn index(&self, index: std::ops::Range<StackIndex>) -> &Self::Output {
        &self.0[index.start.0..index.end.0]
    }
}
impl std::ops::IndexMut<Range<StackIndex>> for VMResults {
    fn index_mut(&mut self, index: Range<StackIndex>) -> &mut Self::Output {
        &mut self.0[index.start.0..index.end.0]
    }
}
impl std::ops::Index<RangeFrom<StackIndex>> for VMResults {
    type Output = [Option<StackValue>];

    fn index(&self, index: std::ops::RangeFrom<StackIndex>) -> &Self::Output {
        &self.0[index.start.0..]
    }
}
impl std::ops::IndexMut<RangeFrom<StackIndex>> for VMResults {
    fn index_mut(&mut self, index: RangeFrom<StackIndex>) -> &mut Self::Output {
        &mut self.0[index.start.0..]
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
            StackValue::Any(any) => {
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
            StackValue::Any(any) => {
                Err(Error::AttemptedConversionOfNativeObject(any.type_id()))
            }
        }
    }
}
impl<'a> TryInto<&'a dyn Any> for &'a StackValue {
    type Error = Error;

    fn try_into(self) -> Result<&'a dyn Any, Self::Error> {
        match self {
            StackValue::Any(any) => Ok(any.as_ref()),
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
                    StackValue::Any(any) => Err(
                        Error::AttemptedConversionOfNativeObject(any.type_id()),
                    ),
                }
            }
        }

        impl TryInto<$prim> for &'_ StackValue {
            type Error = Error;

            fn try_into(self) -> Result<$prim, Self::Error> {
                match self {
                    StackValue::Prim(value) => (*value).try_into(),
                    StackValue::Any(any) => Err(
                        Error::AttemptedConversionOfNativeObject(any.type_id()),
                    ),
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
