use std::{
    any::Any,
    collections::HashMap,
    fmt::Display,
    mem::MaybeUninit,
    num::NonZeroUsize,
    ops::{Range, RangeFrom},
};

use arrayvec::ArrayVec;
use derive_more::derive::From;
use itertools::{Either, Itertools};
use lru::LruCache;
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
    current_instruction: InstructionIndex,
    reader: Box<dyn VMReader + 'a>,
    values: VMResults,
}

const SMALL_BYTE_ARRAY_SIZE: usize = 8;

#[derive(Debug, From)]
pub enum StackValue {
    Prim(RuntimePrimValue),
    ByteArray(Vec<u8>),
    SmallByteArray([u8; SMALL_BYTE_ARRAY_SIZE]),
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

    /// Check if an object's method table indicates that it is an
    /// instance of a given type, or a subclass of the given type.
    IsSubclassOf {
        method_table_ptr: VMArg,
        base_type: TypedPointer<MethodTable>,
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
        "IsSubclassOf requires the argument to be a Pointer, \
         but instead received {0}."
    )]
    InvalidArgumentForSubclassCheck(RuntimePrimValue),

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
                other => Err(VMExecutionError::IncorrectOutputType {
                    attempted: RustType::new::<T>().into(),
                    actual: other.runtime_type(),
                }),
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
                other => Err(VMExecutionError::IncorrectOutputType {
                    attempted: RustType::new::<T>().into(),
                    actual: other.runtime_type(),
                }),
            })
            .transpose()?;

        Ok(opt_obj)
    }

    fn collect_native_function_args_impl<'a, 'b>(
        &'a mut self,
        vm_args: &[VMArg],
        inline_consts: &'a mut [Option<StackValue>],
        collected_args: &'b mut [MaybeUninit<&'a mut Option<StackValue>>],
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
        let mut remaining_values = &mut self.values[..];
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

    pub fn num_instructions_evaluated(&self) -> usize {
        self.num_instructions_evaluated
    }
}

impl StackValue {
    pub(crate) fn runtime_type(&self) -> RuntimeType {
        match self {
            StackValue::Prim(prim) => prim.runtime_type().into(),
            StackValue::ByteArray(_) | StackValue::SmallByteArray(_) => {
                RuntimeType::ByteArray
            }
            StackValue::Native(native) => native.ty.clone(),
        }
    }

    pub fn as_prim(&self) -> Option<RuntimePrimValue> {
        match self {
            StackValue::Prim(prim) => Some(*prim),
            StackValue::Native(_)
            | StackValue::ByteArray(_)
            | StackValue::SmallByteArray(_) => None,
        }
    }

    pub fn as_native<T: RustNativeObject>(&self) -> Option<&T> {
        match self {
            StackValue::Native(native) => native.downcast_ref(),
            StackValue::Prim(_)
            | StackValue::ByteArray(_)
            | StackValue::SmallByteArray(_) => None,
        }
    }

    pub fn as_byte_array(&self) -> Option<&[u8]> {
        match self {
            StackValue::ByteArray(arr) => Some(arr),
            StackValue::SmallByteArray(arr) => Some(arr),
            StackValue::Prim(_) | StackValue::Native(_) => None,
        }
    }

    pub fn as_mut_byte_array(&mut self) -> Option<&mut [u8]> {
        match self {
            StackValue::ByteArray(arr) => Some(arr),
            StackValue::SmallByteArray(arr) => Some(arr),
            StackValue::Prim(_) | StackValue::Native(_) => None,
        }
    }

    pub fn read_string_ptr(
        &self,
        reader: &memory_reader::MemoryReader,
    ) -> Result<String, Error> {
        match self {
            StackValue::Prim(prim) => prim.read_string_ptr(reader),
            other => {
                Err(Error::InvalidOperandForDotnetString(other.runtime_type())
                    .into())
            }
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

const PAGE_SIZE: usize = 4096;
const NUM_CACHE_PAGES: usize = 1024;

struct CachedVMReader<Inner> {
    inner: Inner,

    /// The cached data
    cache: Vec<u8>,

    /// Lookup from remote address to index within the cache
    lru_cache: LruCache<Pointer, usize>,
}

impl<Inner> CachedVMReader<Inner> {
    fn new(inner: Inner) -> Self {
        let cache = vec![0; NUM_CACHE_PAGES * PAGE_SIZE];

        // Initialize the cache with dummy pointers, and valid indices.
        let mut lru_cache =
            LruCache::new(NonZeroUsize::new(NUM_CACHE_PAGES).unwrap());
        for i in 0..NUM_CACHE_PAGES {
            lru_cache.push(Pointer::null() + 1 + i, i * PAGE_SIZE);
        }

        Self {
            inner,
            cache,
            lru_cache,
        }
    }

    fn from_cache<'a>(
        lru_cache: &mut LruCache<Pointer, usize>,
        cache: &'a [u8],
        range: Range<Pointer>,
    ) -> Option<(&'a [u8], &'a [u8])> {
        let page_start = range.start.prev_multiple_of(PAGE_SIZE);
        let page_end = range.end.next_multiple_of(PAGE_SIZE);

        let num_pages = (page_end - page_start) / PAGE_SIZE;

        let empty_slice = &[0u8; 0];

        match num_pages {
            0 => Some((empty_slice, empty_slice)),
            1 => {
                let page_index = lru_cache.get(&page_start)?;
                let cache_index = page_index + (range.start - page_start);
                let num_bytes = range.end - range.start;
                let from_cache = &cache[cache_index..cache_index + num_bytes];
                Some((from_cache, empty_slice))
            }
            2 => {
                let page_boundary = page_start + PAGE_SIZE;
                let num_bytes_page_0 = page_boundary - range.start;
                let num_bytes_page_1 = range.end - page_boundary;

                let page_0_start_index = *lru_cache.get(&page_start)?;
                let page_0_end_index = page_0_start_index + PAGE_SIZE;
                let cache_range_0 =
                    page_0_end_index - num_bytes_page_0..page_0_end_index;

                let page_1_start_index = *lru_cache.get(&page_boundary)?;
                let cache_range_1 =
                    page_1_start_index..page_1_start_index + num_bytes_page_1;

                let slice_0 = &cache[cache_range_0];
                let slice_1 = &cache[cache_range_1];

                Some((slice_0, slice_1))
            }
            _ => panic!(
                "Region in cached read may cross at most one page boundary, \
                 but pointer range from {} to {} consists of {} bytes, \
                 and would require reading from {} cached pages.",
                range.start,
                range.end,
                range.end - range.start,
                num_pages,
            ),
        }
    }

    fn choose_cache_range(requested_range: Range<Pointer>) -> Range<Pointer> {
        let page_start = requested_range.start.prev_multiple_of(PAGE_SIZE);
        let page_end = requested_range.end.next_multiple_of(PAGE_SIZE);
        page_start..page_end
    }

    fn iter_pages(region: Range<Pointer>) -> impl Iterator<Item = Pointer> {
        let start = region.start.prev_multiple_of(PAGE_SIZE);
        let end = region.end;
        (0..)
            .map(move |i| start + i * PAGE_SIZE)
            .take_while(move |ptr| *ptr < end)
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
        assert_eq!(self.lru_cache.len(), NUM_CACHE_PAGES);

        const MAX_PAGES_PER_READ: usize = 16;
        let mut to_read =
            ArrayVec::<(Pointer, usize), MAX_PAGES_PER_READ>::new();
        const MAX_DELAYED_UPDATES: usize = 64;
        let mut update_after_read =
            ArrayVec::<usize, MAX_DELAYED_UPDATES>::new();

        const _: () = assert!(
            MAX_PAGES_PER_READ < NUM_CACHE_PAGES,
            "Cannot read more pages in a batched read \
             than are contained in the cache.."
        );

        macro_rules! flush {
            () => {{
                to_read.sort_by_key(|(_, index)| *index);

                {
                    let mut subregions = ArrayVec::<
                        (Pointer, &mut [u8]),
                        MAX_PAGES_PER_READ,
                    >::new();

                    let mut prev_index = None;
                    let mut remaining_slice = &mut self.cache[..];
                    for (page, index) in to_read.iter().cloned() {
                        let split = match prev_index {
                            Some(prev_index) => index - prev_index,
                            None => index + PAGE_SIZE,
                        };
                        assert!(split >= PAGE_SIZE);
                        assert!(
                            split <= remaining_slice.len(),
                            "Split location {split}, \
                             derived from index={index} and prev_index={prev_index:?}, \
                             exceeds remaining length {}.  \
                             Initial length {} has been consumed at indices [{}].",
                            remaining_slice.len(),
                            100usize*PAGE_SIZE,
                            to_read.iter().map(|(_,index)| *index).format(", "),
                        );
                        let (left, right) = remaining_slice.split_at_mut(split);
                        let page_slice = &mut left[split-PAGE_SIZE..];
                        subregions.push((page, page_slice));
                        remaining_slice = right;
                        prev_index = Some(index);
                    }

                    self.inner.read_byte_regions(&mut subregions)?;
                }

                for (page, index) in to_read.iter().cloned() {
                    let evicted = self.lru_cache.push(page, index);
                    assert!(
                        evicted.is_none(),
                        "No evictions should be required, \
                         since this only contains pages not in cache, \
                         and LRU was already popped to decide the index.  \
                         Pages in current batch = [{}].",
                        to_read.iter().map(|(page,_)| *page)
                            .counts()
                            .into_iter()
                            .sorted_by_key(|(page,counts)| (std::cmp::Reverse(*counts),*page))
                            .map(|(page,counts)| format!("{page}: {counts}"))
                            .format(", ")
                    );
                }

                for i in update_after_read.iter().cloned() {
                    let loc = regions[i].0;
                    let output = &mut regions[i].1;
                    let remote_range = loc..loc + output.len();
                    let (cache_a, cache_b) = Self::from_cache(
                        &mut self.lru_cache,
                        &self.cache,
                        remote_range.clone(),
                    )
                    .expect("Cache entry should be populated");
                    output[..cache_a.len()].copy_from_slice(cache_a);
                    output[cache_a.len()..].copy_from_slice(cache_b);
                }

                to_read.clear();
                update_after_read.clear();
            }};
        }

        for i in 0..regions.len() {
            // Looping over indices rather than `regions.iter_mut()`,
            // because ownership of `regions` needs to be dropped
            // whenever flushing out the collected
            // `update_after_read`.
            let loc = regions[i].0;
            let output = &mut regions[i].1;
            let remote_range = loc..loc + output.len();
            if let Some((cache_a, cache_b)) = Self::from_cache(
                &mut self.lru_cache,
                &self.cache,
                remote_range.clone(),
            ) {
                output[..cache_a.len()].copy_from_slice(cache_a);
                output[cache_a.len()..].copy_from_slice(cache_b);
                continue;
            }

            for page in Self::iter_pages(remote_range.clone()) {
                let already_in_next_batch =
                    to_read.iter().any(|(queued_page, _)| *queued_page == page);
                if !already_in_next_batch {
                    let index = self
                        .lru_cache
                        .pop_lru()
                        .map(|(_, index)| index)
                        .expect("Cache should never be empty");
                    to_read.push((page, index));
                }
            }
            update_after_read.push(i);

            if to_read.len() + 1 >= to_read.capacity()
                || update_after_read.len() == update_after_read.capacity()
            {
                flush!();
                assert_eq!(self.lru_cache.len(), NUM_CACHE_PAGES);
            }
        }

        flush!();
        assert_eq!(self.lru_cache.len(), NUM_CACHE_PAGES);

        Ok(())
    }

    fn read_bytes(
        &mut self,
        loc: Pointer,
        output: &mut [u8],
    ) -> Result<(), Error> {
        let remote_range = loc..loc + output.len();
        let cache_range = Self::choose_cache_range(remote_range.clone());
        let num_pages = (cache_range.end - cache_range.start) / PAGE_SIZE;

        if num_pages > self.lru_cache.len().min(2) {
            // This single read would exceed the capacity of the
            // entire cache (e.g. a large string), or would exceed the
            // size of a single cached read.  For a read occupies
            // several entire page, it probably wouldn't be shared by
            // other objects, so we can just let it skip the cache
            // altogether.
            return self.inner.read_bytes(loc, output);
        }

        for page in Self::iter_pages(remote_range.clone()) {
            self.lru_cache.promote(&page);
        }

        assert!(
            num_pages <= self.lru_cache.len(),
            "Read of {} bytes would use {num_pages} pages, \
             more than are the {} pages in the cache.",
            output.len(),
            self.lru_cache.len(),
        );

        for page in Self::iter_pages(remote_range.clone()) {
            if self.lru_cache.contains(&page) {
                continue;
            }

            let index = self
                .lru_cache
                .pop_lru()
                .map(|(_, index)| index)
                .expect("Cache should never be empty");

            let res = self
                .inner
                .read_bytes(page, &mut self.cache[index..index + PAGE_SIZE]);

            if res.is_ok() {
                self.lru_cache.push(page, index);
            } else {
                self.lru_cache.push(page + 1, index);
                self.lru_cache.demote(&(page + 1));
            }

            res?;
        }

        let (cache_a, cache_b) = Self::from_cache(
            &mut self.lru_cache,
            &self.cache,
            remote_range.clone(),
        )
        .unwrap_or_else(|| {
            panic!(
                "Cache should be populated at this point, \
                     but doesn't contain {}-{}.",
                remote_range.start, remote_range.end,
            )
        });

        output[..cache_a.len()].copy_from_slice(cache_a);
        output[cache_a.len()..].copy_from_slice(cache_b);
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

                &Instruction::IsSubclassOf {
                    method_table_ptr,
                    base_type,
                    output,
                } => self.eval_is_subclass_of(
                    method_table_ptr,
                    base_type,
                    output,
                )?,

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
                Some(stack_value) => {
                    let bytes =
                        stack_value.as_byte_array().ok_or_else(|| {
                            VMExecutionError::OperatorExpectsByteArray {
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
        (Ptr(a), NativeUInt(b)) => a + b,
        (NativeUInt(a), Ptr(b)) => b + a,
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
            Some(other) => {
                Err(VMExecutionError::InvalidArgumentForSubclassCheck(other))
            }
        }?;

        Ok(())
    }

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
        let mut num_bytes = 0usize;
        for region in regions.iter() {
            let Some(region_bytes) = self.arg_to_prim(region.num_bytes)? else {
                self.values[output] = None;
                return Ok(());
            };
            let region_bytes: usize =
                region_bytes.try_into().map_err(|_| {
                    VMExecutionError::ByteCountNotConvertibleToInt {
                        operator: self.vm.instructions
                            [self.current_instruction.0]
                            .op_name(),
                        index: self.current_instruction,
                        value: region_bytes,
                    }
                })?;
            num_bytes += region_bytes;
        }

        let mut new_stack_value = if num_bytes <= SMALL_BYTE_ARRAY_SIZE {
            StackValue::SmallByteArray([0u8; SMALL_BYTE_ARRAY_SIZE])
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
            Instruction::IsSubclassOf { .. } => "IsSubclassOf",
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
            StackValue::SmallByteArray(_) => write!(f, "SmallByteArray"),
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

            Instruction::IsSubclassOf {
                method_table_ptr,
                base_type,
                output,
            } => write!(
                f,
                "{output} = {method_table_ptr}.is_subclass_of::<{base_type}>()"
            ),

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
