use std::{
    any::Any,
    collections::{BTreeMap, HashMap},
    fmt::Display,
    ops::{Range, RangeFrom},
};

use derive_more::derive::From;
use itertools::{Either, Itertools as _};
use lru::LruCache;
use memory_reader::{OwnedBytes, Pointer};
use paste::paste;
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
    current_instruction: InstructionIndex,
    reader: Box<dyn VMReader + 'a>,
    values: VMResults,
}

#[derive(Debug, From)]
pub enum StackValue {
    Prim(RuntimePrimValue),
    ByteArray(Vec<u8>),
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
        "Operator {operator} in instruction {index} \
         expected a byte array, \
         but received argument of type {arg_type:?}"
    )]
    OperatorExpectsByteArray {
        operator: &'static str,
        index: InstructionIndex,
        arg_type: RuntimeType,
    },

    #[error(
        "Operator {operator} in instruction {index} \
         attention to access byte {byte_index} \
         in byte array of size {array_size}."
    )]
    OutOfBoundsByteIndex {
        operator: &'static str,
        index: InstructionIndex,
        byte_index: usize,
        array_size: usize,
    },

    #[error(
        "Operator {operator} in instruction {index} \
         attempted to use {value} as an integer number of bytes."
    )]
    ByteCountNotConvertibleToInt {
        operator: &'static str,
        index: InstructionIndex,
        value: RuntimePrimValue,
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
                StackValue::ByteArray(_) => {
                    Err(VMExecutionError::IncorrectOutputType {
                        attempted: RustType::new::<T>().into(),
                        actual: RuntimeType::ByteArray,
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
                StackValue::ByteArray(_) => {
                    Err(VMExecutionError::IncorrectOutputType {
                        attempted: RustType::new::<T>().into(),
                        actual: RuntimeType::ByteArray,
                    })
                }
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
            StackValue::ByteArray(_) => RuntimeType::ByteArray,
            StackValue::Native(native) => native.ty.clone(),
        }
    }

    pub fn as_prim(&self) -> Option<RuntimePrimValue> {
        match self {
            StackValue::Prim(prim) => Some(*prim),
            StackValue::Native(_) | StackValue::ByteArray(_) => None,
        }
    }

    pub fn as_native<T: RustNativeObject>(&self) -> Option<&T> {
        match self {
            StackValue::Native(native) => native.downcast_ref(),
            StackValue::Prim(_) | StackValue::ByteArray(_) => None,
        }
    }

    pub fn as_byte_array(&self) -> Option<&[u8]> {
        match self {
            StackValue::ByteArray(arr) => Some(arr),
            StackValue::Prim(_) | StackValue::Native(_) => None,
        }
    }

    pub fn read_string_ptr(
        &self,
        reader: &memory_reader::MemoryReader,
    ) -> Result<String, Error> {
        match self {
            StackValue::Prim(prim) => prim.read_string_ptr(reader),
            StackValue::ByteArray(_) => {
                Err(Error::AttemptedReadOfByteArrayAsStringPtr)
            }
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

    fn read_byte_regions(
        &mut self,
        regions: &mut [(Pointer, &mut [u8])],
    ) -> Result<(), Error> {
        for (ptr, buf) in regions.into_iter() {
            self.read_bytes(*ptr, *buf)?;
        }
        Ok(())
    }

    /// From the specified location, how many bytes are safe to read?
    /// Used for caching, when reading more bytes than requested may
    /// be beneficial, in case they can be returned for future
    /// requests.
    fn safe_read_extent(&self, loc: Pointer) -> Range<Pointer> {
        loc..loc
    }

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

    fn read_byte_regions(
        &mut self,
        regions: &mut [(Pointer, &mut [u8])],
    ) -> Result<(), Error> {
        let reader: &memory_reader::MemoryReader = self.as_ref();
        Ok(reader.read_regions(regions)?)
    }

    fn safe_read_extent(&self, loc: Pointer) -> Range<Pointer> {
        let reader: &memory_reader::MemoryReader = self.as_ref();
        reader.find_containing_region_range(loc).unwrap_or(loc..loc)
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

struct CachedVMReader<Inner> {
    inner: Inner,
    cache: BTreeMap<Pointer, Vec<u8>>,
    lru_tracking: LruCache<Pointer, ()>,
    lru_capacity: usize,
}

impl<Inner> CachedVMReader<Inner> {
    fn new(inner: Inner) -> Self {
        Self {
            inner,
            cache: BTreeMap::new(),
            lru_tracking: LruCache::unbounded(),
            lru_capacity: 100,
        }
    }

    fn from_cache(&mut self, range: Range<Pointer>) -> Option<&[u8]> {
        let start = range.start;
        let end = range.end;
        let num_bytes = end - start;

        self.cache
            .range(..end)
            .rev()
            // In the majority of cases, we only need to check the
            // first cache entry where (cache.start <= region.end).
            // However, if the cache contains two nearby reads, such
            // that the a cache entry starts partway through the
            // requested remote region, then preceding cache entry may
            // also be the correct one for the region.
            .take(2)
            .find(|(cache_loc, cache_data)| {
                let cache_start = **cache_loc;
                let cache_end = cache_start + cache_data.len();
                cache_start <= start && end <= cache_end
            })
            .map(|(cache_loc, cache_data)| {
                self.lru_tracking.promote(cache_loc);
                let index_start = start - *cache_loc;
                let index_end = index_start + num_bytes;
                &cache_data[index_start..index_end]
            })
    }

    fn choose_cache_range(
        &self,
        requested_range: Range<Pointer>,
    ) -> Range<Pointer>
    where
        Inner: VMReader,
    {
        // Ideally, we're going to read whichever pages contain the
        // pointer-range that we're reading out.
        const PAGE_SIZE: usize = 4096;
        let page_start = requested_range.start.prev_multiple_of(PAGE_SIZE);
        let page_end = requested_range.end.next_multiple_of(PAGE_SIZE);
        let desired_range = page_start..page_end;

        // However, it may not be safe to do so in all cases.  If the
        // requested range is close to the beginning or end of a
        // memmap region, then the expanded size of the read may be
        // unsafe, even if a read of the requested region would have
        // succeeded.
        let safe_range = self.inner.safe_read_extent(requested_range.start);

        // Since the `safe_read_extent` function may not identify a
        // safe extent, we can still assume that the requested range
        // is safe to read.  This ensures that a read always populates
        // the cache for the requested byte range, even if it can't
        // read anything more.
        let safe_range = safe_range.start.min(requested_range.start)
            ..safe_range.end.max(requested_range.end);

        // The actual region to be read out will be the intersection
        // of the region that we'd like to read, with the region that
        // is safe to read.
        let cache_range = desired_range.start.max(safe_range.start)
            ..desired_range.end.min(safe_range.end);

        cache_range
    }

    fn add_to_cache(&mut self, cached_entries: Vec<(Pointer, Vec<u8>)>) {
        for (ptr, new_cache) in &cached_entries {
            if let Some(cached) = self.cache.get(ptr) {
                panic!(
                    "Ptr at {ptr} is already cached.  \
                     Previously, cached with {} bytes, \
                     now cached with {} bytes",
                    cached.len(),
                    new_cache.len(),
                );
            }
        }
        cached_entries
            .iter()
            .map(|(ptr, _)| *ptr)
            .counts()
            .into_iter()
            .for_each(|(ptr, counts)| {
                assert_eq!(
                    counts, 1,
                    "Cache update had {counts} entries for {ptr}"
                );
            });

        // Normally, the eviction would be automatically handled by
        // `self.lru_tracking.push`.  However, if a batched read
        // produces more cache entries than are configured to be in
        // the cache, the cache size should temporarily increase to
        // contain the cached entries.
        //
        // That is, after calling `add_to_cache`, all entries provided
        // must be in the cache.
        let size_with_new_entries =
            self.lru_tracking.len() + cached_entries.len();
        let cache_entries_to_pop = size_with_new_entries
            .saturating_sub(self.lru_capacity)
            .clamp(0, self.lru_tracking.len());
        for _ in 0..cache_entries_to_pop {
            let evicted = self
                .lru_tracking
                .pop_lru()
                .expect("Protected by earlier length check")
                .0;
            self.cache.remove(&evicted);
        }

        for (ptr, bytes) in cached_entries {
            if let Some((evicted, _)) = self.lru_tracking.push(ptr, ()) {
                assert!(self.cache.contains_key(&evicted));
                unreachable!(
                    "Automatic LRU evictions are disabled, \
                     to allow a batch of cache entries \
                     to temporarily exceed the specified LRU capacity.  \
                     However, found eviction of pointer {evicted}"
                );
            }
            self.cache.insert(ptr, bytes);
        }
    }
}

impl<Inner> VMReader for CachedVMReader<Inner>
where
    Inner: VMReader,
{
    fn read_byte_regions(
        &mut self,
        regions: &mut [(Pointer, &mut [u8])],
    ) -> Result<(), Error> {
        let mut to_read = Vec::<(Pointer, Vec<u8>)>::new();
        let mut update_after_read = Vec::<usize>::new();

        regions
            .iter_mut()
            .enumerate()
            .for_each(|(i, (loc, output))| {
                let loc = *loc;
                let remote_range = loc..loc + output.len();
                if let Some(cached) = self.from_cache(remote_range.clone()) {
                    output.copy_from_slice(cached);
                } else {
                    // A single batch of reads will usually be avoided
                    // by CSE at the IR level.  However, it's also
                    // possible that two distinct expressions will
                    // evaluate to the same pointer, resulting in two
                    // reads being issued to the same location.
                    let is_new_read = to_read.iter().all(|queued_read| {
                        let queued_range =
                            queued_read.0..queued_read.0 + queued_read.1.len();
                        queued_range.end < remote_range.start
                            || remote_range.end < queued_range.start
                    });
                    if is_new_read {
                        let cache_range =
                            self.choose_cache_range(remote_range.clone());
                        to_read.push((
                            cache_range.start,
                            vec![0u8; cache_range.end - cache_range.start],
                        ));
                    }
                    update_after_read.push(i);
                }
            });

        if to_read.is_empty() {
            // Early bail-out for the case where all reads were
            // present in the cache.
            return Ok(());
        }

        {
            let mut regions = Vec::<(Pointer, &mut [u8])>::new();
            let mut remaining = &mut to_read[..];
            while !remaining.is_empty() {
                let (left, right) = remaining.split_at_mut(1);
                let (start, slice) = &mut left[0];
                regions.push((*start, slice));
                remaining = right;
            }
            self.inner.read_byte_regions(&mut regions)?;
        }
        self.add_to_cache(to_read);

        update_after_read.into_iter().for_each(|i| {
            let (loc, output) = &mut regions[i];
            let loc = *loc;
            let remote_range = loc..loc + output.len();
            if let Some(cached) = self.from_cache(remote_range.clone()) {
                output.copy_from_slice(cached);
            } else {
                unreachable!("Cache should be populated by add_to_cache()");
            }
        });

        Ok(())
    }

    fn read_bytes(
        &mut self,
        loc: Pointer,
        output: &mut [u8],
    ) -> Result<(), Error> {
        let remote_range = loc..loc + output.len();
        if let Some(cached) = self.from_cache(remote_range.clone()) {
            output.copy_from_slice(cached);
            return Ok(());
        }

        let cache_range = self.choose_cache_range(remote_range.clone());

        let mut cache_bytes = vec![0u8; cache_range.end - cache_range.start];
        self.inner.read_bytes(cache_range.start, &mut cache_bytes)?;

        self.add_to_cache(vec![(cache_range.start, cache_bytes)]);
        if let Some(cached) = self.from_cache(remote_range.clone()) {
            output.copy_from_slice(cached);
        } else {
            unreachable!("Cache should be populated after add_to_cache().");
        }

        Ok(())
    }

    fn is_dotnet_base_class_of(
        &mut self,
        parent_class_ptr: TypedPointer<MethodTable>,
        child_class_ptr: TypedPointer<MethodTable>,
    ) -> Result<bool, Error> {
        self.inner
            .is_dotnet_base_class_of(parent_class_ptr, child_class_ptr)
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

        let values = {
            let stack = (0..self.stack_size).map(|_| None).collect();
            VMResults {
                values: stack,
                num_instructions_evaluated: 0,
            }
        };

        Ok(VMEvaluator {
            vm: self,
            current_instruction: entry_point,
            reader: Box::new(DummyReader),
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
    pub fn evaluate(&self, reader: impl VMReader) -> Result<VMResults, Error> {
        self.get_function("main")?.with_reader(reader).evaluate()
    }
}

macro_rules! define_comparison_op {
    ($cmp:ident) => {paste!{
        fn [< eval_ $cmp >](
            &mut self,
            lhs: VMArg,
            rhs: VMArg,
            output: StackIndex,
        ) -> Result<(), Error> {
            let opt_lhs = self.arg_to_prim(lhs)?;
            let opt_rhs = self.arg_to_prim(rhs)?;

            self.values[output] = match (opt_lhs, opt_rhs) {
                (Some(lhs), Some(rhs)) => {
                    let res = match (lhs, rhs) {
                        (
                            RuntimePrimValue::NativeUInt(a),
                            RuntimePrimValue::NativeUInt(b),
                        ) => Ok(RuntimePrimValue::Bool(a.$cmp(&b))),
                        _ => Err(Error::InvalidOperandsForNumericComparison {
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
    }};
}

macro_rules! define_binary_integer_op {
    ($func_name:ident, $op_func:ident) => {
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
                    let res = match (lhs, rhs) {
                        (
                            RuntimePrimValue::NativeUInt(a),
                            RuntimePrimValue::NativeUInt(b),
                        ) => Ok(RuntimePrimValue::NativeUInt(a.$op_func(b))),
                        (lhs, rhs) => Err(Error::InvalidOperandsForBinaryOp {
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
    };
}

impl<'a> VMEvaluator<'a> {
    pub fn with_reader(self, reader: impl VMReader + 'a) -> VMEvaluator<'a> {
        let reader = CachedVMReader::new(reader);
        VMEvaluator {
            reader: Box::new(reader),
            ..self
        }
    }

    pub fn evaluate(mut self) -> Result<VMResults, Error> {
        while self.current_instruction.0 < self.vm.instructions.len() {
            let instruction = &self.vm.instructions[self.current_instruction.0];
            let mut next_instruction =
                InstructionIndex(self.current_instruction.0 + 1);
            self.values.num_instructions_evaluated += 1;

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

                &Instruction::Downcast {
                    obj,
                    subtype,
                    output,
                } => self.eval_downcast(obj, subtype, output)?,

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
                    let mut inline_consts = Vec::new();
                    let mut collected_outputs =
                        self.values.collect_native_function_args(
                            outputs,
                            &mut inline_consts,
                        );
                    let outputs = collected_outputs
                        .iter_mut()
                        .map(|opt_mut| opt_mut.take())
                        .collect();
                    return Ok(VMResults {
                        values: outputs,
                        ..self.values
                    });
                }
            }

            self.current_instruction = next_instruction;
        }

        Err(VMExecutionError::ReachedEndWithoutReturnInstruction.into())
    }

    fn arg_to_prim(
        &self,
        arg: VMArg,
    ) -> Result<Option<RuntimePrimValue>, Error> {
        match arg {
            VMArg::Const(value) => Ok(Some(value)),
            VMArg::SavedValue(index) => match &self.values[index] {
                Some(StackValue::Prim(prim)) => Ok(Some(*prim)),
                Some(other) => {
                    Err(VMExecutionError::OperatorExpectsPrimitiveArgument {
                        operator: self.vm.instructions
                            [self.current_instruction.0]
                            .op_name(),
                        index: self.current_instruction,
                        arg_type: other.runtime_type(),
                    }
                    .into())
                }
                None => Ok(None),
            },
        }
    }

    fn arg_to_byte_array(&self, arg: VMArg) -> Result<Option<&[u8]>, Error> {
        match arg {
            VMArg::Const(value) => {
                Err(VMExecutionError::OperatorExpectsByteArray {
                    operator: self.vm.instructions[self.current_instruction.0]
                        .op_name(),
                    index: self.current_instruction,
                    arg_type: value.runtime_type().into(),
                }
                .into())
            }
            VMArg::SavedValue(index) => match &self.values[index] {
                Some(StackValue::ByteArray(bytes)) => Ok(Some(bytes)),
                Some(other) => {
                    Err(VMExecutionError::OperatorExpectsByteArray {
                        operator: self.vm.instructions
                            [self.current_instruction.0]
                            .op_name(),
                        index: self.current_instruction,
                        arg_type: other.runtime_type(),
                    }
                    .into())
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
                other => {
                    Err(VMExecutionError::InvalidOperandForConditionalJump(
                        other.runtime_type(),
                    ))
                }
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
        args: &[VMArg],
        output: Option<StackIndex>,
    ) -> Result<(), Error> {
        let mut inline_consts = Vec::new();
        let mut args = self
            .values
            .collect_native_function_args(args, &mut inline_consts);
        let result = self.vm.native_functions[func_index.0].apply(&mut args)?;
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

    fn eval_add(
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
                    (lhs, rhs) => Err(Error::InvalidOperandsForBinaryOp {
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

    fn eval_sub(
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
                let res = match (lhs, rhs) {
                    (
                        RuntimePrimValue::NativeUInt(a),
                        RuntimePrimValue::NativeUInt(b),
                    ) => Ok(RuntimePrimValue::NativeUInt(a - b)),
                    (
                        RuntimePrimValue::Ptr(a),
                        RuntimePrimValue::NativeUInt(b),
                    ) => Ok(RuntimePrimValue::Ptr(a - b)),
                    (lhs, rhs) => Err(Error::InvalidOperandsForBinaryOp {
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

    fn eval_mul(
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
                let res = match (lhs, rhs) {
                    (
                        RuntimePrimValue::NativeUInt(a),
                        RuntimePrimValue::NativeUInt(b),
                    ) => Ok(RuntimePrimValue::NativeUInt(a * b)),
                    (RuntimePrimValue::F32(a), RuntimePrimValue::F32(b)) => {
                        Ok(RuntimePrimValue::F32(a * b))
                    }
                    (lhs, rhs) => Err(Error::InvalidOperandsForBinaryOp {
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

    define_binary_integer_op! {eval_div, div_euclid}
    define_binary_integer_op! {eval_mod, rem_euclid}

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

    define_comparison_op! {eq}
    define_comparison_op! {ne}
    define_comparison_op! {lt}
    define_comparison_op! {le}
    define_comparison_op! {gt}
    define_comparison_op! {ge}

    fn eval_downcast(
        &mut self,
        obj: VMArg,
        subtype: TypedPointer<MethodTable>,
        output: StackIndex,
    ) -> Result<(), Error> {
        let obj = self.arg_to_prim(obj)?;

        self.values[output] = match obj {
            None => Ok(None),
            Some(RuntimePrimValue::Ptr(ptr)) => {
                let actual_type_ptr: Pointer = {
                    let mut arr = [0; Pointer::SIZE];
                    self.reader.read_bytes(ptr, &mut arr)?;
                    arr.into()
                };
                let is_valid_cast = self
                    .reader
                    .is_dotnet_base_class_of(subtype, actual_type_ptr.into())?;

                Ok(is_valid_cast.then(|| RuntimePrimValue::Ptr(ptr).into()))
            }
            Some(other) => {
                Err(VMExecutionError::DowncastAppliedToNonPointer(other))
            }
        }?;

        Ok(())
    }

    fn eval_read_bytes(
        &mut self,
        regions: &[VMByteRange],
        output: StackIndex,
    ) -> Result<(), Error> {
        let mut remote_ranges = Vec::<Range<Pointer>>::new();
        for region in regions.iter() {
            let opt_ptr = self.arg_to_prim(region.ptr)?;
            let opt_num_bytes = self.arg_to_prim(region.num_bytes)?;

            if opt_num_bytes.is_none() {
                self.values[output] = None;
                return Ok(());
            }

            let ptr: Pointer = opt_ptr
                .map(TryInto::try_into)
                .transpose()?
                .unwrap_or_else(Pointer::null);
            let num_bytes: RuntimePrimValue = opt_num_bytes.unwrap();
            let num_bytes: usize = num_bytes.try_into().map_err(|_| {
                VMExecutionError::ByteCountNotConvertibleToInt {
                    operator: self.vm.instructions[self.current_instruction.0]
                        .op_name(),
                    index: self.current_instruction,
                    value: num_bytes,
                }
            })?;
            remote_ranges.push(ptr..ptr + num_bytes);
        }

        let num_bytes = remote_ranges
            .iter()
            .map(|range| range.end - range.start)
            .sum::<usize>();
        let mut bytes = vec![0u8; num_bytes];

        let mut regions: Vec<(Pointer, &mut [u8])> = Vec::new();
        {
            let mut remaining = &mut bytes[..];
            for range in remote_ranges {
                let num_bytes = range.end - range.start;
                let (left, right) = remaining.split_at_mut(num_bytes);
                if !range.start.is_null() {
                    regions.push((range.start, left));
                }
                remaining = right;
            }
        }
        self.reader.read_byte_regions(&mut regions)?;

        self.values[output] = Some(StackValue::ByteArray(bytes));

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
                        VMExecutionError::OutOfBoundsByteIndex {
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
                    |byte_range| -> Result<OwnedBytes, Error> {
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
            | Instruction::Downcast { obj: arg, .. }
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
            | Instruction::Downcast { output, .. }
            | Instruction::ReadBytes { output, .. }
            | Instruction::CastBytes { output, .. }
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
            StackValue::ByteArray(_) => write!(f, "ByteArray"),
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
            other => Err(Error::IllegalConversionToPrimitiveValue(
                other.runtime_type(),
            )),
        }
    }
}
impl TryInto<RuntimePrimValue> for &'_ StackValue {
    type Error = Error;

    fn try_into(self) -> Result<RuntimePrimValue, Self::Error> {
        match self {
            StackValue::Prim(value) => Ok(*value),
            other => Err(Error::IllegalConversionToPrimitiveValue(
                other.runtime_type(),
            )),
        }
    }
}
impl<'a> TryInto<&'a dyn Any> for &'a StackValue {
    type Error = Error;

    fn try_into(self) -> Result<&'a dyn Any, Self::Error> {
        match self {
            StackValue::Native(native) => Ok(native.as_ref()),
            other => Err(Error::IllegalConversionToNativeObject(
                other.runtime_type(),
            )),
        }
    }
}
impl TryInto<Box<dyn Any>> for StackValue {
    type Error = Error;

    fn try_into(self) -> Result<Box<dyn Any>, Self::Error> {
        match self {
            StackValue::Native(native) => Ok(native.into()),
            other => Err(Error::IllegalConversionToNativeObject(
                other.runtime_type(),
            )),
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
                    other => Err(Error::IllegalConversionToPrimitiveValue(
                        other.runtime_type(),
                    )),
                }
            }
        }

        impl TryInto<$prim> for &'_ StackValue {
            type Error = Error;

            fn try_into(self) -> Result<$prim, Self::Error> {
                match self {
                    StackValue::Prim(value) => (*value).try_into(),
                    other => Err(Error::IllegalConversionToPrimitiveValue(
                        other.runtime_type(),
                    )),
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
