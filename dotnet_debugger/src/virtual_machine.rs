use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use memory_reader::Pointer;
use thiserror::Error;

use crate::{
    runtime_type::RuntimePrimType, CachedReader, Error, MethodTable,
    RuntimePrimValue, TypedPointer,
};

pub struct VirtualMachine {
    /// The instructions to execute in the virtual machine.
    instructions: Vec<Instruction>,

    /// The number of output values to produce.
    num_outputs: usize,

    /// The number of additional temporary values to allocate.
    num_temporaries: usize,
}

#[derive(Clone)]
pub enum Instruction {
    // Write the current value of the register into vm.values[index]
    SaveValue { index: usize },

    // Read from vm.values[index] into the register
    RestoreValue { index: usize },

    // Overwrite the register with a pointer.
    Const { value: RuntimePrimValue },

    // Increment the value of the register by `byte_offset`
    StaticOffset { byte_offset: usize },

    // Convert the value of the register to `usize`
    AsIndex,

    // Scale the register by the specified value
    StaticScale { factor: usize },

    // Increment the value of the register by the value stored in
    // vm.values[index].
    DynamicOffset { index: usize },

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

impl VirtualMachine {
    pub fn new(instructions: Vec<Instruction>, num_outputs: usize) -> Self {
        let num_values = instructions
            .iter()
            .filter_map(|instruction| match instruction {
                Instruction::SaveValue { index }
                | Instruction::RestoreValue { index }
                | Instruction::DynamicOffset { index } => Some(*index + 1),
                _ => None,
            })
            .max()
            .unwrap_or(0);

        assert!(num_values >= num_outputs);

        Self {
            instructions,
            num_outputs,
            num_temporaries: num_values - num_outputs,
        }
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
                Instruction::RestoreValue { index } => {
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
            .filter_map(|inst| match inst {
                Instruction::RestoreValue { index } => Some(*index),
                Instruction::DynamicOffset { index } => Some(*index),
                _ => None,
            })
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
                Instruction::RestoreValue { index } => {
                    let index = do_remap(index);
                    Instruction::RestoreValue { index }
                }
                Instruction::DynamicOffset { index } => {
                    let index = do_remap(index);
                    Instruction::DynamicOffset { index }
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

    pub fn evaluate(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<Vec<Option<RuntimePrimValue>>, Error> {
        let mut register = None;
        let mut values = vec![None; self.num_outputs + self.num_temporaries];

        for instruction in &self.instructions {
            match instruction {
                Instruction::SaveValue { index } => {
                    values[*index] = register;
                }
                Instruction::RestoreValue { index } => {
                    register = values[*index];
                }
                Instruction::Const { value } => {
                    register = Some(*value);
                }
                Instruction::AsIndex => {
                    register = match register {
                        None => None,
                        Some(value) => Some(RuntimePrimValue::NativeUInt(
                            value.as_usize()?,
                        )),
                    };
                }
                Instruction::StaticOffset {
                    byte_offset: offset,
                } => {
                    register = match register {
                        None => None,
                        Some(RuntimePrimValue::Ptr(ptr)) => {
                            Some(RuntimePrimValue::Ptr(ptr + *offset))
                        }
                        Some(other) => {
                            return Err(
                                VMExecutionError::OffsetAppliedToNonPointer(
                                    other.runtime_type(),
                                )
                                .into(),
                            );
                        }
                    };
                }
                Instruction::StaticScale { factor } => {
                    register = match register {
                        None => None,
                        Some(value) => {
                            let offset = value.as_usize()?;
                            Some(RuntimePrimValue::NativeUInt(offset * factor))
                        }
                    };
                }
                Instruction::DynamicOffset { index } => {
                    register = match register {
                        None => None,
                        Some(RuntimePrimValue::Ptr(ptr)) => {
                            let offset = match values[*index] {
                                Some(value) => Ok(value.as_usize()?),
                                None => Err(
                                    VMExecutionError::DynamicOffsetWasEmpty(
                                        *index,
                                    ),
                                ),
                            }?;
                            Some(RuntimePrimValue::Ptr(ptr + offset))
                        }
                        Some(other) => {
                            return Err(
                                VMExecutionError::OffsetAppliedToNonPointer(
                                    other.runtime_type(),
                                )
                                .into(),
                            );
                        }
                    }
                }
                Instruction::Downcast { ty } => {
                    register = match register {
                        None => None,
                        Some(RuntimePrimValue::Ptr(ptr)) => {
                            let actual_type_ptr: Pointer =
                                reader.read_byte_array(ptr)?.into();
                            reader
                                .is_base_of(*ty, actual_type_ptr.into())?
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
                            let bytes = reader
                                .read_bytes(ptr..ptr + ty.size_bytes())?;
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
        Ok(values)
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
impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::SaveValue { index } => write!(f, "SaveValue({index})"),
            Instruction::RestoreValue { index } => {
                write!(f, "RestoreValue({index})")
            }
            Instruction::Const { value } => write!(f, "Const({value})"),
            Instruction::StaticOffset { byte_offset } => {
                write!(f, "StaticOffset({byte_offset})")
            }
            Instruction::StaticScale { factor } => {
                write!(f, "StaticScale({factor})")
            }
            Instruction::DynamicOffset { index } => {
                write!(f, "DynamicOffset({index})")
            }
            Instruction::Downcast { ty } => write!(f, "Downcast({ty})"),
            Instruction::Read { ty } => write!(f, "Read({ty})"),
            Instruction::AsIndex => write!(f, "AsIndex"),
        }
    }
}
