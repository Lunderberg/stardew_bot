use derive_more::derive::From;
use dotnet_debugger::RuntimePrimValue;

use crate::{OpIndex, Pointer};

#[derive(Debug, Clone, Copy, From)]
pub enum SymbolicValue {
    Const(RuntimePrimValue),
    Result(OpIndex),
}

impl SymbolicValue {
    pub fn as_op_index(self) -> Option<OpIndex> {
        match self {
            SymbolicValue::Result(op_index) => Some(op_index),
            _ => None,
        }
    }

    pub fn as_prim_value(self) -> Option<RuntimePrimValue> {
        match self {
            SymbolicValue::Const(prim) => Some(prim),
            SymbolicValue::Result(_) => None,
        }
    }
}

impl std::cmp::PartialEq for SymbolicValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            // Floating-point numbers are compared by their bit
            // representations.  While the IEEE comparisons for NaN
            // are appropriate for comparing the output of operations,
            // comparing the bit representations is appropriate for
            // the input of computations.  (e.g. If two computations
            // `f(x)` and `g(y)` each produce NaN as output, it is
            // unlikely that any code path relying on equality will be
            // useful, so `f(x) == g(y)` should return false when
            // `f(x)` and `g(y)` are both NaN.  If two computations
            // `f(x)` and `f(y)` each accept NaN as input, then they
            // will produce the same value, so `x==y` should return
            // true when `x` and `y` are both NaN.)  Since
            // `SymbolicValue` always represents an input to a
            // computation, it can compare equality of floats using
            // their bit representation.
            (
                Self::Const(RuntimePrimValue::F32(lhs)),
                Self::Const(RuntimePrimValue::F32(rhs)),
            ) => lhs.to_bits() == rhs.to_bits(),

            (
                Self::Const(RuntimePrimValue::F64(lhs)),
                Self::Const(RuntimePrimValue::F64(rhs)),
            ) => lhs.to_bits() == rhs.to_bits(),

            // All other types are compared using PartialEq on the
            // appropriate type.
            (Self::Const(lhs), Self::Const(rhs)) => lhs == rhs,
            (Self::Result(lhs), Self::Result(rhs)) => lhs == rhs,
            _ => false,
        }
    }
}

impl std::cmp::Eq for SymbolicValue {}

impl std::hash::Hash for SymbolicValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            SymbolicValue::Const(prim) => {
                core::mem::discriminant(prim).hash(state);
                match prim {
                    // Similar to the implementation of PartialEq,
                    // SymbolicValue only acts as an input to operations,
                    // so the hash can be provided based on the bit
                    // representation.  This would not be appropriate if
                    // hashing `RuntimePrimValue` directly, as the
                    // `RuntimePrimValue` could be used either as the
                    // input or output of computations.
                    RuntimePrimValue::F32(val) => val.to_bits().hash(state),
                    RuntimePrimValue::F64(val) => val.to_bits().hash(state),

                    // All other types are hashed using their default
                    // implementation.
                    RuntimePrimValue::Bool(val) => val.hash(state),
                    RuntimePrimValue::Char(val) => val.hash(state),
                    RuntimePrimValue::U8(val) => val.hash(state),
                    RuntimePrimValue::U16(val) => val.hash(state),
                    RuntimePrimValue::U32(val) => val.hash(state),
                    RuntimePrimValue::U64(val) => val.hash(state),
                    RuntimePrimValue::NativeUInt(val) => val.hash(state),
                    RuntimePrimValue::I8(val) => val.hash(state),
                    RuntimePrimValue::I16(val) => val.hash(state),
                    RuntimePrimValue::I32(val) => val.hash(state),
                    RuntimePrimValue::I64(val) => val.hash(state),
                    RuntimePrimValue::NativeInt(val) => val.hash(state),
                    RuntimePrimValue::Ptr(val) => val.hash(state),
                }
            }
            SymbolicValue::Result(op_index) => {
                op_index.hash(state);
            }
        }
    }
}

impl std::fmt::Display for SymbolicValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolicValue::Const(prim) => write!(f, "{prim}"),
            SymbolicValue::Result(op_index) => write!(f, "{op_index}"),
        }
    }
}

macro_rules! symbolic_value_from_prim {
    ($prim:ty) => {
        impl From<$prim> for SymbolicValue {
            fn from(prim: $prim) -> Self {
                Self::Const(prim.into())
            }
        }
    };
}
symbolic_value_from_prim!(bool);
symbolic_value_from_prim!(u8);
symbolic_value_from_prim!(u16);
symbolic_value_from_prim!(u32);
symbolic_value_from_prim!(u64);
symbolic_value_from_prim!(usize);
symbolic_value_from_prim!(i8);
symbolic_value_from_prim!(i16);
symbolic_value_from_prim!(i32);
symbolic_value_from_prim!(i64);
symbolic_value_from_prim!(isize);
symbolic_value_from_prim!(f32);
symbolic_value_from_prim!(f64);
symbolic_value_from_prim!(Pointer);
