use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use derive_more::derive::From;
use itertools::Either;
use memory_reader::Pointer;

use crate::{
    runtime_type::RuntimePrimType, virtual_machine::Instruction, Error,
    MethodTable, OpIndex, RuntimePrimValue, TypedPointer, VirtualMachine,
};

pub struct PhysicalGraph {
    ops: Vec<PhysicalExpr>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, From)]
pub enum PhysicalValue {
    Int(usize),
    Ptr(Pointer),
    Result(OpIndex),
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum PhysicalExpr {
    Add {
        lhs: PhysicalValue,
        rhs: PhysicalValue,
    },
    Mul {
        lhs: PhysicalValue,
        rhs: PhysicalValue,
    },
    Downcast {
        obj: PhysicalValue,
        ty: TypedPointer<MethodTable>,
    },
    ReadValue {
        ptr: PhysicalValue,
        prim_type: RuntimePrimType,
    },
    Tuple(Vec<PhysicalValue>),
}

impl PhysicalExpr {
    fn remap(
        &self,
        current_index: OpIndex,
        map: &HashMap<OpIndex, PhysicalValue>,
    ) -> Result<Self, Error> {
        let remap = |value: &PhysicalValue| -> Result<PhysicalValue, Error> {
            Ok(match value {
                PhysicalValue::Result(op_index) => map
                    .get(op_index)
                    .cloned()
                    .ok_or_else(|| Error::InvalidReference {
                        from: current_index,
                        to: *op_index,
                    })?
                    .into(),
                other => *other,
            })
        };

        let new_op = match self {
            PhysicalExpr::Add { lhs, rhs } => PhysicalExpr::Add {
                lhs: remap(lhs)?,
                rhs: remap(rhs)?,
            },
            PhysicalExpr::Mul { lhs, rhs } => PhysicalExpr::Mul {
                lhs: remap(lhs)?,
                rhs: remap(rhs)?,
            },
            PhysicalExpr::Downcast { obj, ty } => PhysicalExpr::Downcast {
                obj: remap(obj)?,
                ty: *ty,
            },
            PhysicalExpr::ReadValue { ptr, prim_type } => {
                PhysicalExpr::ReadValue {
                    ptr: remap(ptr)?,
                    prim_type: *prim_type,
                }
            }
            PhysicalExpr::Tuple(vec) => PhysicalExpr::Tuple(
                vec.into_iter().map(remap).collect::<Result<_, _>>()?,
            ),
        };

        Ok(new_op)
    }

    fn visit_upstream_producers(&self, mut callback: impl FnMut(OpIndex)) {
        let mut value_callback = |value: &PhysicalValue| {
            if let PhysicalValue::Result(op_index) = value {
                callback(*op_index);
            }
        };

        match self {
            PhysicalExpr::Add { lhs, rhs } | PhysicalExpr::Mul { lhs, rhs } => {
                value_callback(lhs);
                value_callback(rhs);
            }
            PhysicalExpr::Downcast { obj, .. } => value_callback(obj),
            PhysicalExpr::ReadValue { ptr, .. } => value_callback(ptr),
            PhysicalExpr::Tuple(vec) => vec.iter().for_each(value_callback),
        }
    }
}

impl PhysicalGraph {
    pub fn new() -> Self {
        Self { ops: Vec::new() }
    }

    fn push_impl(&mut self, op: PhysicalExpr) -> PhysicalValue {
        let index = self.ops.len();
        self.ops.push(op);
        OpIndex::new(index).into()
    }

    pub fn push<T>(&mut self, value: T) -> PhysicalValue
    where
        T: Into<PhysicalExpr>,
    {
        self.push_impl(value.into())
    }

    pub fn add(
        &mut self,
        lhs: impl Into<PhysicalValue>,
        rhs: impl Into<PhysicalValue>,
    ) -> PhysicalValue {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push_impl(PhysicalExpr::Add { lhs, rhs })
    }

    pub fn mul(
        &mut self,
        lhs: impl Into<PhysicalValue>,
        rhs: impl Into<PhysicalValue>,
    ) -> PhysicalValue {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push_impl(PhysicalExpr::Mul { lhs, rhs })
    }

    pub fn downcast(
        &mut self,
        obj: impl Into<PhysicalValue>,
        ty: TypedPointer<MethodTable>,
    ) -> PhysicalValue {
        let obj = obj.into();
        self.push_impl(PhysicalExpr::Downcast { obj, ty })
    }

    pub fn read_value(
        &mut self,
        ptr: impl Into<PhysicalValue>,
        prim_type: RuntimePrimType,
    ) -> PhysicalValue {
        let ptr = ptr.into();
        self.push_impl(PhysicalExpr::ReadValue { ptr, prim_type })
    }

    fn iter_ops(&self) -> impl Iterator<Item = (OpIndex, PhysicalExpr)> + '_ {
        self.ops
            .iter()
            .enumerate()
            .map(|(i, expr)| (OpIndex::new(i), expr.clone()))
    }

    pub fn simplify(self) -> Result<Self, Error> {
        let mut prev_index_lookup: HashMap<OpIndex, PhysicalValue> =
            HashMap::new();
        let mut builder = Self::new();

        for (prev_index, op) in self.iter_ops() {
            let new_op = op.remap(prev_index, &prev_index_lookup)?;
            let mut value = builder.push(new_op);

            let mut do_simplify_step =
                |value: &PhysicalValue| -> Option<PhysicalValue> {
                    let &PhysicalValue::Result(index) = value else {
                        return None;
                    };
                    let op = &builder[index];
                    match op {
                        // Constant-folding, lhs and rhs are known
                        &PhysicalExpr::Add {
                            lhs: PhysicalValue::Int(a),
                            rhs: PhysicalValue::Int(b),
                        } => Some(PhysicalValue::Int(a + b)),

                        &PhysicalExpr::Mul {
                            lhs: PhysicalValue::Int(a),
                            rhs: PhysicalValue::Int(b),
                        } => Some(PhysicalValue::Int(a * b)),

                        // lhs + 0 => lhs
                        &PhysicalExpr::Add {
                            lhs,
                            rhs: PhysicalValue::Int(0),
                        } => Some(lhs),

                        // 0 + rhs => rhs
                        &PhysicalExpr::Add {
                            lhs: PhysicalValue::Int(0),
                            rhs,
                        } => Some(rhs),

                        // 0 * rhs => 0
                        PhysicalExpr::Mul {
                            rhs: PhysicalValue::Int(0),
                            ..
                        } => Some(PhysicalValue::Int(0)),

                        // lhs * 0 => 0
                        PhysicalExpr::Mul {
                            lhs: PhysicalValue::Int(0),
                            ..
                        } => Some(PhysicalValue::Int(0)),

                        // lhs * 1 => lhs
                        &PhysicalExpr::Mul {
                            lhs,
                            rhs: PhysicalValue::Int(1),
                        } => Some(lhs),

                        // 1 * rhs => rhs
                        &PhysicalExpr::Mul {
                            lhs: PhysicalValue::Int(1),
                            rhs,
                        } => Some(rhs),

                        // const + rhs => rhs + const
                        &PhysicalExpr::Add {
                            lhs: lhs @ PhysicalValue::Int(_),
                            rhs,
                        } => Some(builder.add(rhs, lhs)),

                        // const * rhs => rhs * const
                        &PhysicalExpr::Mul {
                            lhs: lhs @ PhysicalValue::Int(_),
                            rhs,
                        } => Some(builder.mul(rhs, lhs)),

                        // lhs + a + b => lhs + (a+b)
                        &PhysicalExpr::Add {
                            lhs: PhysicalValue::Result(lhs),
                            rhs: PhysicalValue::Int(b),
                        } => match &builder[lhs] {
                            &PhysicalExpr::Add {
                                lhs,
                                rhs: PhysicalValue::Int(a),
                            } => Some(builder.add(lhs, a + b)),
                            _ => None,
                        },

                        // lhs * a * b => lhs * (a*b)
                        &PhysicalExpr::Mul {
                            lhs: PhysicalValue::Result(lhs),
                            rhs: PhysicalValue::Int(b),
                        } => match &builder[lhs] {
                            &PhysicalExpr::Mul {
                                lhs,
                                rhs: PhysicalValue::Int(a),
                            } => Some(builder.mul(lhs, a * b)),
                            _ => None,
                        },

                        _ => None,
                    }
                };

            while let Some(simplified) = do_simplify_step(&value) {
                value = simplified;
            }

            prev_index_lookup.insert(prev_index, value);
        }

        Ok(builder)
    }

    pub fn dead_code_elimination(self) -> Result<Self, Error> {
        let mut prev_index_lookup: HashMap<OpIndex, PhysicalValue> =
            HashMap::new();
        let mut builder = Self::new();

        let reachable = {
            let mut reachable = HashSet::new();
            let mut to_visit = Vec::new();

            let start = OpIndex::new(self.ops.len());
            to_visit.push(start);
            reachable.insert(start);

            while let Some(visiting) = to_visit.pop() {
                self.ops[visiting.0].visit_upstream_producers(|input_node| {
                    if !reachable.contains(&input_node) {
                        reachable.insert(input_node);
                        to_visit.push(input_node);
                    }
                });
            }

            reachable
        };

        for (prev_index, op) in self.iter_ops() {
            if reachable.contains(&prev_index) {
                let op = op.remap(prev_index, &prev_index_lookup)?;
                let new_index = builder.push(op);
                prev_index_lookup.insert(prev_index, new_index);
            }
        }

        Ok(builder)
    }

    pub fn eliminate_common_subexpresssions(self) -> Result<Self, Error> {
        let mut builder = Self::new();

        let mut prev_index_lookup: HashMap<OpIndex, PhysicalValue> =
            HashMap::new();
        let mut dedup_lookup: HashMap<PhysicalExpr, PhysicalValue> =
            HashMap::new();

        for (prev_index, op) in self.iter_ops() {
            let new_op = op.remap(prev_index, &prev_index_lookup)?;

            let new_index = if let Some(new_index) = dedup_lookup.get(&new_op) {
                *new_index
            } else {
                let new_index = builder.push(new_op.clone());
                dedup_lookup.insert(new_op, new_index);
                new_index
            };
            prev_index_lookup.insert(prev_index, new_index);
        }

        Ok(builder)
    }
}

impl<T> From<Vec<T>> for PhysicalExpr
where
    T: Into<PhysicalValue>,
{
    fn from(value: Vec<T>) -> Self {
        PhysicalExpr::Tuple(value.into_iter().map(Into::into).collect())
    }
}

impl PhysicalGraph {
    pub(crate) fn to_virtual_machine(&self) -> Result<VirtualMachine, Error> {
        let (num_ops, iter_outputs) = if let PhysicalExpr::Tuple(tuple) =
            &self.ops[self.ops.len() - 1]
        {
            (self.ops.len() - 1, Either::Left(tuple.iter().cloned()))
        } else {
            let value: PhysicalValue = OpIndex::new(self.ops.len() - 1).into();
            (self.ops.len(), Either::Right(std::iter::once(value)))
        };
        let output_indices: HashMap<OpIndex, usize> = iter_outputs
            .map(|output| match output {
                PhysicalValue::Result(op_index) => op_index,
                other => panic!("Output of {other} not yet handled"),
            })
            .enumerate()
            .map(|(i, output)| (output, i))
            .collect();
        let num_outputs = output_indices.len();

        let mut instructions = Vec::new();
        let mut currently_stored: HashMap<OpIndex, usize> = HashMap::new();
        let mut next_free_index = num_outputs;

        for (op_index, op) in self.ops.iter().take(num_ops).enumerate() {
            let op_index = OpIndex::new(op_index);
            match op {
                PhysicalExpr::Add { lhs, rhs } => {
                    let lhs_instruction = match lhs {
                        &PhysicalValue::Ptr(ptr) => Instruction::Const {
                            value: RuntimePrimValue::Ptr(ptr),
                        },
                        PhysicalValue::Result(op_index) => {
                            let index = *currently_stored.get(op_index).expect(
                                "Internal error, \
                                 {op_index} not located anywhere",
                            );
                            Instruction::RestoreValue { index }
                        }

                        PhysicalValue::Int(value) => panic!(
                            "LHS of add should be a pointer, \
                             but was instead {value}"
                        ),
                    };
                    instructions.push(lhs_instruction);

                    let rhs_instruction = match rhs {
                        &PhysicalValue::Int(offset) => {
                            Instruction::StaticOffset {
                                byte_offset: offset,
                            }
                        }
                        PhysicalValue::Result(op_index) => {
                            let index = *currently_stored.get(op_index).expect(
                                "Internal error, \
                                 {op_index} not located anywhere",
                            );
                            Instruction::DynamicOffset { index }
                        }
                        PhysicalValue::Ptr(pointer) => panic!(
                            "RHS of add should be byte offset, \
                             but was instead {pointer}"
                        ),
                    };
                    instructions.push(rhs_instruction);
                }
                PhysicalExpr::Mul { lhs, rhs } => {
                    let lhs_instruction = match lhs {
                        PhysicalValue::Result(op_index) => {
                            let index = *currently_stored.get(op_index).expect(
                                "Internal error, \
                                 {op_index} not located anywhere",
                            );
                            Instruction::RestoreValue { index }
                        }

                        PhysicalValue::Ptr(ptr) => panic!(
                            "LHS of multiply should be output of previous op, \
                             but was instead {ptr}"
                        ),

                        PhysicalValue::Int(value) => panic!(
                            "LHS of multiply should be a pointer, \
                             but was instead {value}"
                        ),
                    };
                    instructions.push(lhs_instruction);

                    let rhs_instruction = match rhs {
                        &PhysicalValue::Int(factor) => {
                            Instruction::StaticScale { factor }
                        }
                        PhysicalValue::Ptr(ptr) => panic!(
                            "RHS of multiply should be a static value, \
                             but was instead {ptr}"
                        ),
                        PhysicalValue::Result(op_index) => panic!(
                            "RHS of multiply should be a static value, \
                             but was instead {op_index}"
                        ),
                    };
                    instructions.push(rhs_instruction);
                }
                &PhysicalExpr::Downcast { obj, ty } => {
                    let obj_instruction = match obj {
                        PhysicalValue::Ptr(ptr) => Instruction::Const {
                            value: RuntimePrimValue::Ptr(ptr),
                        },
                        PhysicalValue::Result(op_index) => {
                            let index =
                                *currently_stored.get(&op_index).expect(
                                    "Internal error, \
                                     {op_index} not located anywhere",
                                );
                            Instruction::RestoreValue { index }
                        }
                        PhysicalValue::Int(value) => panic!(
                            "LHS of downcast should be a pointer, \
                             but was instead {value}"
                        ),
                    };
                    instructions.push(obj_instruction);
                    instructions.push(Instruction::Downcast { ty });
                }
                &PhysicalExpr::ReadValue { ptr, prim_type } => {
                    let obj_instruction = match ptr {
                        PhysicalValue::Ptr(ptr) => Instruction::Const {
                            value: RuntimePrimValue::Ptr(ptr),
                        },
                        PhysicalValue::Result(op_index) => {
                            let index =
                                *currently_stored.get(&op_index).expect(
                                    "Internal error, \
                                     {op_index} not located anywhere",
                                );
                            Instruction::RestoreValue { index }
                        }
                        PhysicalValue::Int(value) => panic!(
                            "LHS of ReadValue should be a pointer, \
                             but was instead {value}"
                        ),
                    };
                    instructions.push(obj_instruction);
                    instructions.push(Instruction::Read { ty: prim_type });
                }
                PhysicalExpr::Tuple(_) => {
                    panic!("Tuple should only appear at top-level")
                }
            }

            let register_index =
                output_indices.get(&op_index).cloned().unwrap_or_else(|| {
                    let index = next_free_index;
                    next_free_index += 1;
                    index
                });
            instructions.push(Instruction::SaveValue {
                index: register_index,
            });
            currently_stored.insert(op_index, register_index);
        }

        Ok(VirtualMachine::new(instructions, num_outputs))
    }
}

impl Display for PhysicalValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PhysicalValue::Int(value) => write!(f, "{value}"),
            PhysicalValue::Ptr(pointer) => write!(f, "{pointer}"),
            PhysicalValue::Result(op_index) => write!(f, "{op_index}"),
        }
    }
}

impl Display for PhysicalExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PhysicalExpr::Add { lhs, rhs } => write!(f, "{lhs} + {rhs}")?,
            PhysicalExpr::Mul { lhs, rhs } => write!(f, "{lhs}*{rhs}")?,
            PhysicalExpr::Downcast { obj, ty } => {
                write!(f, "{obj}.downcast({ty})")?
            }
            PhysicalExpr::ReadValue { ptr, prim_type } => {
                write!(f, "{ptr}.read::<{prim_type}>()")?
            }
            PhysicalExpr::Tuple(vec) => {
                write!(f, "[")?;
                for (vec_i, input) in vec.iter().enumerate() {
                    if vec_i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{input}")?;
                }
                write!(f, "]")?;
            }
        }
        Ok(())
    }
}

impl Display for PhysicalGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i_output, op) in self.ops.iter().enumerate() {
            writeln!(f, "[{i_output}] <- {op}")?;
        }
        Ok(())
    }
}

impl std::ops::Index<OpIndex> for PhysicalGraph {
    type Output = PhysicalExpr;

    fn index(&self, index: OpIndex) -> &Self::Output {
        &self.ops[index.0]
    }
}
