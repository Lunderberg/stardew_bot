use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use memory_reader::Pointer;
use thiserror::Error;

use crate::{
    runtime_type::RuntimePrimType, CachedReader, Error, MethodTable,
    RuntimePrimValue, TypedPointer, ValueToken,
};

pub struct VirtualMachine {
    /// The instructions to execute in the virtual machine.
    instructions: Vec<Instruction>,

    /// The number of output values to produce.
    num_outputs: usize,

    /// The number of additional temporary values to allocate.
    num_temporaries: usize,
}

pub struct VMResults(Vec<Option<RuntimePrimValue>>);

#[derive(Clone, Copy)]
pub enum VMArg {
    Const(RuntimePrimValue),
    SavedValue { index: usize },
}

#[derive(Clone)]
pub enum Instruction {
    // Write the current value of the register into vm.values[index]
    SaveValue { index: usize },

    // Load the constant, or previously saved value, into the
    // register.
    LoadToRegister(VMArg),

    // Cast the value in the register to the specified type.
    PrimCast(RuntimePrimType),

    // Increment the register by the value specified in the argument,
    // storing the result back to the register.
    Add(VMArg),

    // Multiply the register by the value specified in the argument,
    // storing the result back to the register.
    Mul(VMArg),

    // Downcast a type.  Assumes the register contains a pointer to an
    // object.  If the object is of type `ty`, then the register is
    // unchanged.  If the object is not of type `ty`, then the
    // register is replaced with `None`.
    //
    // TODO: If/when conditionals are implemented, express this as a
    // Read(RuntimeType::Ptr), followed by a boolean check IsBaseOf.
    // That way, multiple type checks on the same object can share the
    // same Read of the method table pointer.
    Downcast { ty: TypedPointer<MethodTable> },

    // Read a value.
    //
    // Assumes the register contains a pointer.  Reads a value of the
    // specified type from the register's location into the register.
    //
    // TODO: Change Read to produce a byte array, followed by a later
    // Cast operator the operates on a subset of those bytes.  This
    // would allow access of multiple fields of an object to share the
    // same read.
    Read { ty: RuntimePrimType },
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
        Err(Error::ReadOccurredDuringLocalVMEvaluation)
    }

    fn is_dotnet_base_class_of(
        &mut self,
        _parent_class_ptr: TypedPointer<MethodTable>,
        _child_class_ptr: TypedPointer<MethodTable>,
    ) -> Result<bool, Error> {
        Err(Error::ReadOccurredDuringLocalVMEvaluation)
    }
}

impl VirtualMachine {
    pub fn new(instructions: Vec<Instruction>, num_outputs: usize) -> Self {
        let num_values = instructions
            .iter()
            .flat_map(|instruction| {
                instruction
                    .input_indices()
                    .chain(instruction.output_indices())
            })
            .map(|index| index + 1)
            .max()
            .unwrap_or(0);

        assert!(
            num_values >= num_outputs,
            "Virtual machine has {num_outputs}, \
             but only found instructions for writing to {num_values}"
        );

        Self {
            instructions,
            num_outputs,
            num_temporaries: num_values - num_outputs,
        }
    }

    pub fn num_instructions(&self) -> usize {
        self.instructions.len()
    }

    pub(crate) fn remove_unnecessary_restore_value(self) -> Self {
        let mut instructions = Vec::new();
        let mut value_in_register: Option<usize> = None;

        for inst in self.instructions.into_iter() {
            match inst {
                Instruction::SaveValue { index } => {
                    value_in_register = Some(index);
                    instructions.push(inst);
                }
                Instruction::LoadToRegister(VMArg::SavedValue { index }) => {
                    if value_in_register != Some(index) {
                        instructions.push(inst);
                    }
                }
                _ => {
                    value_in_register = None;
                    instructions.push(inst);
                }
            }
        }

        Self {
            instructions,
            ..self
        }
    }

    pub(crate) fn remove_unnecessary_save_value(self) -> Self {
        let mut instructions = Vec::new();

        let used: HashSet<_> = self
            .instructions
            .iter()
            .flat_map(|inst| inst.input_indices())
            .collect();

        for inst in self.instructions.into_iter() {
            match inst {
                Instruction::SaveValue { index } => {
                    if used.contains(&index) || index < self.num_outputs {
                        instructions.push(inst);
                    }
                }
                _ => {
                    instructions.push(inst);
                }
            }
        }

        Self {
            instructions,
            ..self
        }
    }

    pub(crate) fn remap_temporary_indices(self) -> Self {
        let mut remap: HashMap<usize, usize> = HashMap::new();

        // TODO: After the last usage of an index, allow it to be
        // reused for a different temporary value.
        //
        // TODO: If all occurrences of a temporary value are before
        // the write of an output index, allow the output index to be
        // used as a temporary.
        let mut do_remap = |index: usize| -> usize {
            if index < self.num_outputs {
                index
            } else if let Some(new_index) = remap.get(&index) {
                *new_index
            } else {
                let new_index = self.num_outputs + remap.len();
                remap.insert(index, new_index);
                new_index
            }
        };

        let instructions = self
            .instructions
            .into_iter()
            .map(|inst| match inst {
                Instruction::SaveValue { index } => {
                    let index = do_remap(index);
                    Instruction::SaveValue { index }
                }
                Instruction::LoadToRegister(VMArg::SavedValue { index }) => {
                    let index = do_remap(index);
                    Instruction::LoadToRegister(VMArg::SavedValue { index })
                }
                Instruction::Add(VMArg::SavedValue { index }) => {
                    let index = do_remap(index);
                    Instruction::Add(VMArg::SavedValue { index })
                }
                Instruction::Mul(VMArg::SavedValue { index }) => {
                    let index = do_remap(index);
                    Instruction::Mul(VMArg::SavedValue { index })
                }
                other => other,
            })
            .collect();

        Self {
            instructions,
            num_temporaries: remap.len(),
            ..self
        }
    }

    /// Evaluate the virtual machine, raising an error if any instructions attempt to read to
    pub fn local_eval(&self) -> Result<VMResults, Error> {
        self.evaluate(DummyReader)
    }

    pub fn evaluate(
        &self,
        mut reader: impl VMReader,
    ) -> Result<VMResults, Error> {
        let mut register: Option<RuntimePrimValue> = None;
        let mut values = vec![None; self.num_outputs + self.num_temporaries];

        macro_rules! arg_to_value {
            ($arg:expr) => {
                match $arg {
                    VMArg::Const(prim) => Some(prim),
                    VMArg::SavedValue { index } => values[*index].as_ref(),
                }
            };
        }

        for instruction in &self.instructions {
            match instruction {
                Instruction::SaveValue { index } => {
                    values[*index] = register;
                }
                Instruction::LoadToRegister(value) => {
                    register = arg_to_value!(value).cloned();
                }
                Instruction::PrimCast(prim_type) => {
                    register = register
                        .map(|value| value.prim_cast(*prim_type))
                        .transpose()?;
                }
                Instruction::Add(value) => {
                    let arg = arg_to_value!(value);
                    register = match (register, arg) {
                        (Some(lhs), Some(rhs)) => {
                            let res = match (lhs, rhs) {
                                (
                                    RuntimePrimValue::NativeUInt(a),
                                    RuntimePrimValue::NativeUInt(b),
                                ) => Ok(RuntimePrimValue::NativeUInt(a + b)),
                                (
                                    RuntimePrimValue::Ptr(a),
                                    &RuntimePrimValue::NativeUInt(b),
                                ) => Ok(RuntimePrimValue::Ptr(a + b)),
                                (
                                    RuntimePrimValue::NativeUInt(a),
                                    &RuntimePrimValue::Ptr(b),
                                ) => Ok(RuntimePrimValue::Ptr(b + a)),
                                (lhs, rhs) => {
                                    Err(Error::InvalidOperandsForAddition {
                                        lhs: lhs.runtime_type().into(),
                                        rhs: rhs.runtime_type().into(),
                                    })
                                }
                            }?;
                            Some(res)
                        }
                        _ => None,
                    };
                }
                Instruction::Mul(value) => {
                    let arg = arg_to_value!(value);
                    register = match (register, arg) {
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
                            Some(res)
                        }
                        _ => None,
                    };
                }
                Instruction::Downcast { ty } => {
                    register = match register {
                        None => None,
                        Some(RuntimePrimValue::Ptr(ptr)) => {
                            let actual_type_ptr: Pointer = {
                                let mut arr = [0; Pointer::SIZE];
                                reader.read_bytes(ptr, &mut arr)?;
                                arr.into()
                            };
                            reader
                                .is_dotnet_base_class_of(
                                    *ty,
                                    actual_type_ptr.into(),
                                )?
                                .then(|| RuntimePrimValue::Ptr(ptr))
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
                Instruction::Read { ty } => {
                    register = match register {
                        None => None,
                        Some(RuntimePrimValue::Ptr(ptr)) => {
                            let bytes = {
                                let mut vec = vec![0; ty.size_bytes()];
                                reader.read_bytes(ptr, &mut vec)?;
                                vec
                            };
                            let prim_value = ty.parse(&bytes)?;
                            match prim_value {
                                RuntimePrimValue::Ptr(ptr) if ptr.is_null() => {
                                    None
                                }
                                other => Some(other),
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
            }
        }

        values.truncate(self.num_outputs);

        Ok(VMResults(values))
    }

    // TODO: Remove this method.  Initially implemented as part of
    // migration from PhysicalAccessChain.
    pub fn read_as<T>(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<Option<T>, Error>
    where
        RuntimePrimValue: TryInto<T>,
        Error: From<<RuntimePrimValue as TryInto<T>>::Error>,
    {
        Ok(self.evaluate(reader)?[0]
            .map(|value| value.as_type::<T>())
            .transpose()?)
    }
}

impl Instruction {
    fn input_indices(&self) -> impl Iterator<Item = usize> {
        let opt_index = match self {
            Instruction::Add(VMArg::SavedValue { index })
            | Instruction::Mul(VMArg::SavedValue { index })
            | Instruction::LoadToRegister(VMArg::SavedValue { index }) => {
                Some(*index)
            }

            Instruction::SaveValue { .. }
            | Instruction::Add(_)
            | Instruction::Mul(_)
            | Instruction::LoadToRegister(_)
            | Instruction::PrimCast(_)
            | Instruction::Downcast { .. }
            | Instruction::Read { .. } => None,
        };
        opt_index.into_iter()
    }

    fn output_indices(&self) -> impl Iterator<Item = usize> {
        let opt_index = match self {
            Instruction::SaveValue { index } => Some(*index),

            Instruction::Add(_)
            | Instruction::Mul(_)
            | Instruction::LoadToRegister(_)
            | Instruction::PrimCast(_)
            | Instruction::Downcast { .. }
            | Instruction::Read { .. } => None,
        };
        opt_index.into_iter()
    }
}

impl From<RuntimePrimValue> for VMArg {
    fn from(value: RuntimePrimValue) -> Self {
        VMArg::Const(value)
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
impl Display for VMArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            VMArg::Const(prim) => write!(f, "{prim}"),
            VMArg::SavedValue { index } => write!(f, "*{index}"),
        }
    }
}
impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::SaveValue { index } => write!(f, "SaveValue({index})"),
            Instruction::LoadToRegister(value) => {
                write!(f, "LoadToRegister({value})")
            }
            Instruction::Add(value) => write!(f, "Add({value})"),
            Instruction::Mul(value) => write!(f, "Mul({value})"),
            Instruction::PrimCast(prim_type) => {
                write!(f, "PrimCast({prim_type})")
            }
            Instruction::Downcast { ty } => write!(f, "Downcast({ty})"),
            Instruction::Read { ty } => write!(f, "Read({ty})"),
        }
    }
}

impl IntoIterator for VMResults {
    type Item = <Vec<Option<RuntimePrimValue>> as IntoIterator>::Item;

    type IntoIter = <Vec<Option<RuntimePrimValue>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl std::ops::Deref for VMResults {
    type Target = Vec<Option<RuntimePrimValue>>;

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
    type Output = Option<RuntimePrimValue>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.0[index]
    }
}

impl std::ops::Index<ValueToken> for VMResults {
    type Output = Option<RuntimePrimValue>;

    fn index(&self, index: ValueToken) -> &Self::Output {
        &self.0[index.0]
    }
}
