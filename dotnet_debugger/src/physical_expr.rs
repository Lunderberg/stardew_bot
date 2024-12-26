use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use derive_more::derive::From;
use itertools::Either;
use memory_reader::Pointer;

use crate::{
    runtime_type::RuntimePrimType, virtual_machine::Instruction, Error,
    MethodTable, RuntimePrimValue, TypedPointer, VirtualMachine,
};

pub struct PhysicalSequence {
    ops: Vec<Expr>,
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, From)]
pub enum Value {
    Int(usize),
    Ptr(Pointer),
    Result(OpIndex),
}

#[derive(PartialEq, Eq, Hash, Clone)]
pub enum Expr {
    Add {
        lhs: Value,
        rhs: Value,
    },
    Mul {
        lhs: Value,
        rhs: Value,
    },
    Downcast {
        obj: Value,
        ty: TypedPointer<MethodTable>,
    },
    ReadValue {
        ptr: Value,
        prim_type: RuntimePrimType,
    },
    Tuple(Vec<Value>),
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub struct OpIndex(usize);

impl Expr {
    fn remap(
        &self,
        current_index: OpIndex,
        map: &HashMap<OpIndex, Value>,
    ) -> Result<Self, Error> {
        let remap = |value: &Value| -> Result<Value, Error> {
            Ok(match value {
                Value::Result(op_index) => map
                    .get(op_index)
                    .cloned()
                    .ok_or_else(|| Error::InvalidReference {
                        from: current_index.0,
                        to: op_index.0,
                    })?
                    .into(),
                other => *other,
            })
        };

        let new_op = match self {
            Expr::Add { lhs, rhs } => Expr::Add {
                lhs: remap(lhs)?,
                rhs: remap(rhs)?,
            },
            Expr::Mul { lhs, rhs } => Expr::Mul {
                lhs: remap(lhs)?,
                rhs: remap(rhs)?,
            },
            Expr::Downcast { obj, ty } => Expr::Downcast {
                obj: remap(obj)?,
                ty: *ty,
            },
            Expr::ReadValue { ptr, prim_type } => Expr::ReadValue {
                ptr: remap(ptr)?,
                prim_type: *prim_type,
            },
            Expr::Tuple(vec) => Expr::Tuple(
                vec.into_iter().map(remap).collect::<Result<_, _>>()?,
            ),
        };

        Ok(new_op)
    }

    fn visit_upstream_producers(&self, mut callback: impl FnMut(OpIndex)) {
        let mut value_callback = |value: &Value| {
            if let Value::Result(op_index) = value {
                callback(*op_index);
            }
        };

        match self {
            Expr::Add { lhs, rhs } | Expr::Mul { lhs, rhs } => {
                value_callback(lhs);
                value_callback(rhs);
            }
            Expr::Downcast { obj, .. } => value_callback(obj),
            Expr::ReadValue { ptr, .. } => value_callback(ptr),
            Expr::Tuple(vec) => vec.iter().for_each(value_callback),
        }
    }
}

impl PhysicalSequence {
    pub fn new() -> Self {
        Self { ops: Vec::new() }
    }

    fn push_impl(&mut self, op: Expr) -> Value {
        let index = self.ops.len();
        self.ops.push(op);
        OpIndex(index).into()
    }

    pub fn push<T>(&mut self, value: T) -> Value
    where
        T: Into<Expr>,
    {
        self.push_impl(value.into())
    }

    pub fn add(
        &mut self,
        lhs: impl Into<Value>,
        rhs: impl Into<Value>,
    ) -> Value {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push_impl(Expr::Add { lhs, rhs })
    }

    pub fn mul(
        &mut self,
        lhs: impl Into<Value>,
        rhs: impl Into<Value>,
    ) -> Value {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push_impl(Expr::Mul { lhs, rhs })
    }

    pub fn downcast(
        &mut self,
        obj: impl Into<Value>,
        ty: TypedPointer<MethodTable>,
    ) -> Value {
        let obj = obj.into();
        self.push_impl(Expr::Downcast { obj, ty })
    }

    pub fn read_value(
        &mut self,
        ptr: impl Into<Value>,
        prim_type: RuntimePrimType,
    ) -> Value {
        let ptr = ptr.into();
        self.push_impl(Expr::ReadValue { ptr, prim_type })
    }

    fn iter_ops(&self) -> impl Iterator<Item = (OpIndex, Expr)> + '_ {
        self.ops
            .iter()
            .enumerate()
            .map(|(i, expr)| (OpIndex(i), expr.clone()))
    }

    pub fn simplify(self) -> Result<Self, Error> {
        let mut prev_index_lookup: HashMap<OpIndex, Value> = HashMap::new();
        let mut builder = Self::new();

        for (prev_index, op) in self.iter_ops() {
            let new_op = op.remap(prev_index, &prev_index_lookup)?;
            let mut value = builder.push(new_op);

            let mut do_simplify_step = |value: &Value| -> Option<Value> {
                let &Value::Result(index) = value else {
                    return None;
                };
                let op = &builder[index];
                match op {
                    // Constant-folding, lhs and rhs are known
                    &Expr::Add {
                        lhs: Value::Int(a),
                        rhs: Value::Int(b),
                    } => Some(Value::Int(a + b)),

                    // lhs + 0 => lhs
                    &Expr::Add {
                        lhs,
                        rhs: Value::Int(0),
                    } => Some(lhs),

                    // 0 + rhs => rhs
                    &Expr::Add {
                        lhs: Value::Int(0),
                        rhs,
                    } => Some(rhs),

                    // lhs + a + b => lhs + (a+b)
                    &Expr::Add {
                        lhs: Value::Result(lhs),
                        rhs: Value::Int(b),
                    } => match &builder[lhs] {
                        &Expr::Add {
                            lhs,
                            rhs: Value::Int(a),
                        } => Some(builder.add(lhs, a + b)),
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
        let mut prev_index_lookup: HashMap<OpIndex, Value> = HashMap::new();
        let mut builder = Self::new();

        let reachable = {
            let mut reachable = HashSet::new();
            let mut to_visit = Vec::new();

            let start = OpIndex(self.ops.len() - 1);
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

        let mut prev_index_lookup: HashMap<OpIndex, Value> = HashMap::new();
        let mut dedup_lookup: HashMap<Expr, Value> = HashMap::new();

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

impl<T> From<Vec<T>> for Expr
where
    T: Into<Value>,
{
    fn from(value: Vec<T>) -> Self {
        Expr::Tuple(value.into_iter().map(Into::into).collect())
    }
}

impl PhysicalSequence {
    pub(crate) fn to_virtual_machine(&self) -> Result<VirtualMachine, Error> {
        let (num_ops, iter_outputs) =
            if let Expr::Tuple(tuple) = &self.ops[self.ops.len() - 1] {
                (self.ops.len() - 1, Either::Left(tuple.iter().cloned()))
            } else {
                let value: Value = OpIndex(self.ops.len() - 1).into();
                (self.ops.len(), Either::Right(std::iter::once(value)))
            };
        let output_indices: HashMap<OpIndex, usize> = iter_outputs
            .map(|output| match output {
                Value::Result(op_index) => op_index,
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
            let op_index = OpIndex(op_index);
            match op {
                Expr::Add { lhs, rhs } => {
                    let lhs_instruction = match lhs {
                        &Value::Ptr(ptr) => Instruction::Const {
                            value: RuntimePrimValue::Ptr(ptr),
                        },
                        Value::Result(op_index) => {
                            let index = *currently_stored.get(op_index).expect(
                                "Internal error, \
                                 {op_index} not located anywhere",
                            );
                            Instruction::RestoreValue { index }
                        }

                        Value::Int(value) => panic!(
                            "LHS of add should be a pointer, \
                             but was instead {value}"
                        ),
                    };
                    instructions.push(lhs_instruction);

                    let rhs_instruction = match rhs {
                        &Value::Int(offset) => Instruction::StaticOffset {
                            byte_offset: offset,
                        },
                        Value::Result(op_index) => {
                            let index = *currently_stored.get(op_index).expect(
                                "Internal error, \
                                 {op_index} not located anywhere",
                            );
                            Instruction::DynamicOffset { index }
                        }
                        Value::Ptr(pointer) => panic!(
                            "RHS of add should be byte offset, \
                             but was instead {pointer}"
                        ),
                    };
                    instructions.push(rhs_instruction);
                }
                Expr::Mul { lhs, rhs } => {
                    let lhs_instruction = match lhs {
                        Value::Result(op_index) => {
                            let index = *currently_stored.get(op_index).expect(
                                "Internal error, \
                                 {op_index} not located anywhere",
                            );
                            Instruction::RestoreValue { index }
                        }

                        Value::Ptr(ptr) => panic!(
                            "LHS of multiply should be output of previous op, \
                             but was instead {ptr}"
                        ),

                        Value::Int(value) => panic!(
                            "LHS of add should be a pointer, \
                             but was instead {value}"
                        ),
                    };
                    instructions.push(lhs_instruction);

                    let rhs_instruction = match rhs {
                        &Value::Int(factor) => {
                            Instruction::StaticScale { factor }
                        }
                        Value::Ptr(ptr) => panic!(
                            "RHS of multiply should be a static value, \
                             but was instead {ptr}"
                        ),
                        Value::Result(op_index) => panic!(
                            "RHS of multiply should be a static value, \
                             but was instead {op_index}"
                        ),
                    };
                    instructions.push(rhs_instruction);
                }
                &Expr::Downcast { obj, ty } => {
                    let obj_instruction = match obj {
                        Value::Ptr(ptr) => Instruction::Const {
                            value: RuntimePrimValue::Ptr(ptr),
                        },
                        Value::Result(op_index) => {
                            let index =
                                *currently_stored.get(&op_index).expect(
                                    "Internal error, \
                                     {op_index} not located anywhere",
                                );
                            Instruction::RestoreValue { index }
                        }
                        Value::Int(value) => panic!(
                            "LHS of downcast should be a pointer, \
                             but was instead {value}"
                        ),
                    };
                    instructions.push(obj_instruction);
                    instructions.push(Instruction::Downcast { ty });
                }
                &Expr::ReadValue { ptr, prim_type } => {
                    let obj_instruction = match ptr {
                        Value::Ptr(ptr) => Instruction::Const {
                            value: RuntimePrimValue::Ptr(ptr),
                        },
                        Value::Result(op_index) => {
                            let index =
                                *currently_stored.get(&op_index).expect(
                                    "Internal error, \
                                     {op_index} not located anywhere",
                                );
                            Instruction::RestoreValue { index }
                        }
                        Value::Int(value) => panic!(
                            "LHS of ReadValue should be a pointer, \
                             but was instead {value}"
                        ),
                    };
                    instructions.push(obj_instruction);
                    instructions.push(Instruction::Read { ty: prim_type });
                }
                Expr::Tuple(_) => {
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

impl Display for OpIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]", self.0)
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Int(value) => write!(f, "{value}"),
            Value::Ptr(pointer) => write!(f, "{pointer}"),
            Value::Result(op_index) => write!(f, "{op_index}"),
        }
    }
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Add { lhs, rhs } => write!(f, "{lhs} + {rhs}")?,
            Expr::Mul { lhs, rhs } => write!(f, "{lhs}*{rhs}")?,
            Expr::Downcast { obj, ty } => write!(f, "{obj}.downcast({ty})")?,
            Expr::ReadValue { ptr, prim_type } => {
                write!(f, "{ptr}.read::<{prim_type}>()")?
            }
            Expr::Tuple(vec) => {
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

impl Display for PhysicalSequence {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i_output, op) in self.ops.iter().enumerate() {
            writeln!(f, "[{i_output}] <- {op}")?;
        }
        Ok(())
    }
}

impl std::ops::Index<OpIndex> for PhysicalSequence {
    type Output = Expr;

    fn index(&self, index: OpIndex) -> &Self::Output {
        &self.ops[index.0]
    }
}
