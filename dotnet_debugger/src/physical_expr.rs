use std::fmt::Display;

use memory_reader::Pointer;

use crate::{
    runtime_type::RuntimePrimType, virtual_machine::Instruction, MethodTable,
    RuntimePrimValue, TypedPointer, VirtualMachine,
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
        }
    }

    pub(crate) fn to_virtual_machine(&self) -> VirtualMachine {
        let num_outputs = 1;
        let mut instructions = Vec::new();
        let mut current_index = num_outputs;

        self.collect_instructions(&mut instructions, &mut current_index);

        instructions.push(Instruction::SaveValue { index: 0 });

        VirtualMachine::new(instructions)
    }

    fn collect_instructions(
        &self,
        instructions: &mut Vec<Instruction>,
        current_index: &mut usize,
    ) {
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
                expr.collect_instructions(instructions, current_index);
                instructions.push(Instruction::Read {
                    ty: RuntimePrimType::Ptr,
                });
            }
            PhysicalExpr::DynamicOffset {
                base,
                element_index,
                bytes_per_element,
            } => {
                element_index.collect_instructions(instructions, current_index);
                instructions.push(Instruction::AsIndex);
                instructions.push(Instruction::StaticScale {
                    factor: *bytes_per_element,
                });
                let index = *current_index;
                *current_index += 1;

                instructions.push(Instruction::SaveValue { index });
                base.collect_instructions(instructions, current_index);
                instructions.push(Instruction::DynamicOffset { index });
            }

            PhysicalExpr::Offset { base, byte_offset } => {
                base.collect_instructions(instructions, current_index);
                instructions.push(Instruction::StaticOffset {
                    byte_offset: *byte_offset,
                });
            }
            PhysicalExpr::Downcast { obj, ty } => {
                obj.collect_instructions(instructions, current_index);
                instructions.push(Instruction::Downcast { ty: *ty });
            }
            PhysicalExpr::PrimCast { obj, prim_type } => {
                obj.collect_instructions(instructions, current_index);
                instructions.push(Instruction::Read { ty: *prim_type });
            }
        }
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
        }
    }
}
