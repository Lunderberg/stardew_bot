use std::fmt::Display;

use itertools::Either;
use memory_reader::Pointer;

use crate::{
    runtime_type::RuntimePrimType, virtual_machine::Instruction, Error,
    MethodTable, RuntimePrimValue, TypedPointer, VirtualMachine,
};

#[derive(Clone)]
pub enum PhysicalExpr {
    Int(usize),
    Location(Pointer),
    Dereference(Box<PhysicalExpr>),
    DynamicOffset {
        base: Box<PhysicalExpr>,
        element_index: Box<PhysicalExpr>,
        bytes_per_element: usize,
    },
    Offset {
        base: Box<PhysicalExpr>,
        byte_offset: usize,
    },

    Downcast {
        obj: Box<PhysicalExpr>,
        ty: TypedPointer<MethodTable>,
    },

    // TODO: Better representation, which would allow an array of
    // values to be extracted.
    PrimCast {
        obj: Box<PhysicalExpr>,
        prim_type: RuntimePrimType,
    },

    Tuple(Vec<PhysicalExpr>),
}

struct IndexTracker {
    free_indices: Vec<bool>,
}

impl IndexTracker {
    fn new() -> Self {
        Self {
            free_indices: Vec::new(),
        }
    }

    fn alloc(&mut self) -> usize {
        if let Some((index, availability)) = self
            .free_indices
            .iter_mut()
            .enumerate()
            .find(|(_, value)| **value)
        {
            *availability = false;
            index
        } else {
            self.free_indices.push(false);
            self.free_indices.len() - 1
        }
    }

    fn free(&mut self, index: usize) {
        assert!(
            index < self.free_indices.len(),
            "Internal errror: \
             Index {index} is outside of range 0 <= index < {}",
            self.free_indices.len()
        );
        assert!(
            !self.free_indices[index],
            "Internal error: \
             Cannot free index {index}, \
             as it is not currently allocated."
        );
        self.free_indices[index] = true;
    }
}

impl PhysicalExpr {
    pub(crate) fn simplify(self) -> Self {
        match self {
            PhysicalExpr::Int(_) | PhysicalExpr::Location(_) => self,
            PhysicalExpr::Dereference(expr) => {
                let expr = expr.simplify();
                PhysicalExpr::Dereference(Box::new(expr))
            }
            PhysicalExpr::DynamicOffset {
                base,
                element_index,
                bytes_per_element,
            } => {
                let base = base.simplify();
                let element_index = element_index.simplify();
                match (base, element_index) {
                    (
                        PhysicalExpr::Offset {
                            base: inner_base,
                            byte_offset: inner_offset,
                        },
                        PhysicalExpr::Int(fixed_index),
                    ) => PhysicalExpr::Offset {
                        base: inner_base,
                        byte_offset: fixed_index * bytes_per_element
                            + inner_offset,
                    },
                    (base, PhysicalExpr::Int(fixed_index)) => {
                        PhysicalExpr::Offset {
                            base: Box::new(base),
                            byte_offset: fixed_index * bytes_per_element,
                        }
                    }
                    (base, element_index) => PhysicalExpr::DynamicOffset {
                        base: Box::new(base),
                        element_index: Box::new(element_index),
                        bytes_per_element,
                    },
                }
            }
            PhysicalExpr::Offset { base, byte_offset } => {
                let base = base.simplify();

                match base {
                    PhysicalExpr::Offset {
                        base: inner_base,
                        byte_offset: inner_offset,
                    } => PhysicalExpr::Offset {
                        base: inner_base,
                        byte_offset: byte_offset + inner_offset,
                    },
                    _ => PhysicalExpr::Offset {
                        base: Box::new(base),
                        byte_offset,
                    },
                }
            }
            PhysicalExpr::Downcast { obj, ty } => {
                let obj = obj.simplify();
                PhysicalExpr::Downcast {
                    obj: Box::new(obj),
                    ty,
                }
            }
            PhysicalExpr::PrimCast { obj, prim_type } => {
                let obj = obj.simplify();
                PhysicalExpr::PrimCast {
                    obj: Box::new(obj),
                    prim_type,
                }
            }
            PhysicalExpr::Tuple(tuple) => {
                let tuple =
                    tuple.into_iter().map(|value| value.simplify()).collect();
                PhysicalExpr::Tuple(tuple)
            }
        }
    }

    pub(crate) fn to_virtual_machine(&self) -> Result<VirtualMachine, Error> {
        let mut instructions = Vec::new();

        let mut index_tracker = IndexTracker::new();

        let (num_outputs, iter_outputs) =
            if let PhysicalExpr::Tuple(tuple) = self {
                (tuple.len(), Either::Left(tuple.iter()))
            } else {
                (1, Either::Right(std::iter::once(self)))
            };

        let output_indices: Vec<_> =
            (0..num_outputs).map(|_| index_tracker.alloc()).collect();
        for (i_output, expr_output) in iter_outputs.enumerate() {
            expr_output
                .collect_instructions(&mut instructions, &mut index_tracker)?;
            instructions.push(Instruction::SaveValue {
                index: output_indices[i_output],
            });
        }

        Ok(VirtualMachine::new(instructions))
    }

    fn collect_instructions(
        &self,
        instructions: &mut Vec<Instruction>,
        index_tracker: &mut IndexTracker,
    ) -> Result<(), Error> {
        match self {
            PhysicalExpr::Int(value) => {
                instructions.push(Instruction::Const {
                    value: RuntimePrimValue::NativeUInt(*value),
                });
            }
            PhysicalExpr::Location(pointer) => {
                instructions.push(Instruction::Const {
                    value: RuntimePrimValue::Ptr(*pointer),
                });
            }
            PhysicalExpr::Dereference(expr) => {
                expr.collect_instructions(instructions, index_tracker)?;
                instructions.push(Instruction::Read {
                    ty: RuntimePrimType::Ptr,
                });
            }
            PhysicalExpr::DynamicOffset {
                base,
                element_index,
                bytes_per_element,
            } => {
                element_index
                    .collect_instructions(instructions, index_tracker)?;
                instructions.push(Instruction::AsIndex);
                instructions.push(Instruction::StaticScale {
                    factor: *bytes_per_element,
                });
                let index = index_tracker.alloc();

                instructions.push(Instruction::SaveValue { index });
                base.collect_instructions(instructions, index_tracker)?;
                instructions.push(Instruction::DynamicOffset { index });

                index_tracker.free(index);
            }

            PhysicalExpr::Offset { base, byte_offset } => {
                base.collect_instructions(instructions, index_tracker)?;
                instructions.push(Instruction::StaticOffset {
                    byte_offset: *byte_offset,
                });
            }
            PhysicalExpr::Downcast { obj, ty } => {
                obj.collect_instructions(instructions, index_tracker)?;
                instructions.push(Instruction::Downcast { ty: *ty });
            }
            PhysicalExpr::PrimCast { obj, prim_type } => {
                obj.collect_instructions(instructions, index_tracker)?;
                instructions.push(Instruction::Read { ty: *prim_type });
            }
            PhysicalExpr::Tuple(_) => {
                return Err(Error::TupleExpressionOnlySupportedAtTopLevel);
            }
        }

        Ok(())
    }
}

impl Display for PhysicalExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PhysicalExpr::Int(value) => write!(f, "{value}"),
            PhysicalExpr::Location(pointer) => write!(f, "{pointer}"),
            PhysicalExpr::Dereference(expr) => write!(f, "{expr}.deref()"),
            PhysicalExpr::DynamicOffset {
                base,
                element_index,
                bytes_per_element,
            } => write!(
                f,
                "{base}.offset({element_index} * {bytes_per_element})"
            ),
            PhysicalExpr::Offset { base, byte_offset } => {
                write!(f, "{base}.offset({byte_offset})")
            }
            PhysicalExpr::Downcast { obj, ty } => {
                write!(f, "{obj}.downcast({ty})")
            }
            PhysicalExpr::PrimCast { obj, prim_type } => {
                write!(f, "{obj}.as::<{prim_type}>()")
            }
            PhysicalExpr::Tuple(tuple) => {
                write!(f, "(")?;
                for (i, value) in tuple.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{value}")?;
                    if tuple.len() == 1 {
                        write!(f, ",")?;
                    }
                }
                write!(f, ")")?;

                Ok(())
            }
        }
    }
}
