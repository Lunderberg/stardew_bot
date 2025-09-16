use std::any::Any;

use derive_more::derive::From;
use dotnet_debugger::{Pointer, RuntimePrimValue};

use crate::{DSLType, Error, ExposedNativeObject, RustNativeObject};

// TODO: Rename the `StackValue` into something more appropriate,
// since it is used as the common interface at runtime, and the
// bytecode interpreter doesn't really have a stack anymore.

#[derive(Debug, From)]
pub enum StackValue {
    Prim(RuntimePrimValue),
    ByteArray(Vec<u8>),
    SmallByteArray([u8; StackValue::SMALL_BYTE_ARRAY_SIZE]),
    Native(ExposedNativeObject),
}

impl StackValue {
    pub const SMALL_BYTE_ARRAY_SIZE: usize = 8;

    pub fn runtime_type(&self) -> DSLType {
        match self {
            StackValue::Prim(prim) => prim.runtime_type().into(),
            StackValue::ByteArray(_) | StackValue::SmallByteArray(_) => {
                DSLType::ByteArray
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
}

impl std::fmt::Display for StackValue {
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
        impl From<$prim> for StackValue {
            fn from(value: $prim) -> Self {
                Self::Prim(value.into())
            }
        }

        impl TryInto<$prim> for StackValue {
            type Error = Error;

            fn try_into(self) -> Result<$prim, Self::Error> {
                match self {
                    StackValue::Prim(value) => {
                        value.try_into().map_err(Into::into)
                    }
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
                    StackValue::Prim(value) => {
                        (*value).try_into().map_err(Into::into)
                    }
                    other => Err(Error::IllegalConversionToPrimitiveValue(
                        other.runtime_type(),
                    )),
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
