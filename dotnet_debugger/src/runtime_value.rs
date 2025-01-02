use memory_reader::{MemoryReader, Pointer};

use crate::runtime_type::RuntimePrimType;
use crate::{
    Error, MethodTable, RuntimeArray, RuntimeMultiDimArray, RuntimeObject,
    RuntimeString, RuntimeType, TypedPointer,
};

/// A value read out from the remote process.  This only handles
/// fixed-size types, and does not handle structs (CLR ValueType).  To
/// unpack structs, it should be broken down into its constituent
/// fixed-size values.
#[derive(Clone, Copy)]
pub enum RuntimeValue {
    /// A primitive value with fixed size.
    Prim(RuntimePrimValue),

    /// A garbage-collected object.  No method table is required here,
    /// because classes are self-describing.  The method table is
    /// stored at the pointed-to location, followed by the instance
    /// fields.
    Object(TypedPointer<RuntimeObject>),

    /// A struct.  Unliked RuntimeValue::Object, an additional method
    /// table is required to interpret the struct's contents, because
    /// a struct does not contain pointer to its own method tables.
    ValueType {
        location: Pointer,
        method_table: TypedPointer<MethodTable>,
    },

    String(TypedPointer<RuntimeString>),

    Array(TypedPointer<RuntimeArray>),

    MultiDimArray(TypedPointer<RuntimeMultiDimArray>),
}

#[derive(Clone, Copy)]
pub enum RuntimePrimValue {
    Bool(bool),

    // CLR uses `ELEMENT_TYPE_CHAR` to specify a UTF-16 code unit,
    // whereas Rust uses UTF-8.  This should have special handling for
    // the String type.
    Char(u16),

    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    NativeUInt(usize),

    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    NativeInt(isize),

    F32(f32),
    F64(f64),

    Ptr(Pointer),
}

impl RuntimePrimValue {
    pub fn parse(kind: RuntimePrimType, bytes: &[u8]) -> Result<Self, Error> {
        let bytes = Self::truncate_to_length(kind, bytes)?;
        match kind {
            RuntimePrimType::Bool => Ok(RuntimePrimValue::Bool(bytes[0] > 0)),
            RuntimePrimType::Char => Ok(RuntimePrimValue::Char(
                u16::from_ne_bytes(bytes.try_into().unwrap()),
            )),
            RuntimePrimType::I8 => Ok(RuntimePrimValue::I8(i8::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            RuntimePrimType::I16 => Ok(RuntimePrimValue::I16(
                i16::from_ne_bytes(bytes.try_into().unwrap()),
            )),
            RuntimePrimType::I32 => Ok(RuntimePrimValue::I32(
                i32::from_ne_bytes(bytes.try_into().unwrap()),
            )),
            RuntimePrimType::I64 => Ok(RuntimePrimValue::I64(
                i64::from_ne_bytes(bytes.try_into().unwrap()),
            )),
            RuntimePrimType::NativeInt => Ok(RuntimePrimValue::NativeInt(
                isize::from_ne_bytes(bytes.try_into().unwrap()),
            )),

            RuntimePrimType::U8 => Ok(RuntimePrimValue::U8(u8::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            RuntimePrimType::U16 => Ok(RuntimePrimValue::U16(
                u16::from_ne_bytes(bytes.try_into().unwrap()),
            )),
            RuntimePrimType::U32 => Ok(RuntimePrimValue::U32(
                u32::from_ne_bytes(bytes.try_into().unwrap()),
            )),
            RuntimePrimType::U64 => Ok(RuntimePrimValue::U64(
                u64::from_ne_bytes(bytes.try_into().unwrap()),
            )),
            RuntimePrimType::NativeUInt => Ok(RuntimePrimValue::NativeUInt(
                usize::from_ne_bytes(bytes.try_into().unwrap()),
            )),

            RuntimePrimType::F32 => Ok(RuntimePrimValue::F32(
                f32::from_ne_bytes(bytes.try_into().unwrap()),
            )),
            RuntimePrimType::F64 => Ok(RuntimePrimValue::F64(
                f64::from_ne_bytes(bytes.try_into().unwrap()),
            )),
            RuntimePrimType::Ptr => {
                Ok(RuntimePrimValue::Ptr(bytes.try_into().unwrap()))
            }
        }
    }

    fn truncate_to_length(
        runtime_type: RuntimePrimType,
        bytes: &[u8],
    ) -> Result<&[u8], Error> {
        let expected = runtime_type.size_bytes();
        let provided = bytes.len();
        if provided >= expected {
            Ok(&bytes[..expected])
        } else {
            Err(Error::InsufficientBytesForValue {
                runtime_type: RuntimeType::Prim(runtime_type),
                provided,
                expected,
            })
        }
    }

    pub fn runtime_type(&self) -> RuntimePrimType {
        match self {
            RuntimePrimValue::Bool(_) => RuntimePrimType::Bool,
            RuntimePrimValue::Char(_) => RuntimePrimType::Char,
            RuntimePrimValue::U8(_) => RuntimePrimType::U8,
            RuntimePrimValue::U16(_) => RuntimePrimType::U16,
            RuntimePrimValue::U32(_) => RuntimePrimType::U32,
            RuntimePrimValue::U64(_) => RuntimePrimType::U64,
            RuntimePrimValue::NativeUInt(_) => RuntimePrimType::NativeUInt,
            RuntimePrimValue::I8(_) => RuntimePrimType::I8,
            RuntimePrimValue::I16(_) => RuntimePrimType::I16,
            RuntimePrimValue::I32(_) => RuntimePrimType::I32,
            RuntimePrimValue::I64(_) => RuntimePrimType::I64,
            RuntimePrimValue::NativeInt(_) => RuntimePrimType::NativeInt,
            RuntimePrimValue::F32(_) => RuntimePrimType::F32,
            RuntimePrimValue::F64(_) => RuntimePrimType::F64,
            RuntimePrimValue::Ptr(_) => RuntimePrimType::Ptr,
        }
    }

    pub fn as_type<T>(self) -> Result<T, <Self as TryInto<T>>::Error>
    where
        Self: TryInto<T>,
    {
        self.try_into()
    }

    pub fn as_isize(self) -> Result<isize, Error> {
        match self {
            RuntimePrimValue::U8(val) => Ok(val as isize),
            RuntimePrimValue::U16(val) => Ok(val as isize),
            RuntimePrimValue::U32(val) => Ok(val as isize),
            RuntimePrimValue::U64(val) => Ok(val as isize),
            RuntimePrimValue::NativeUInt(val) => Ok(val as isize),
            RuntimePrimValue::I8(val) => Ok(val as isize),
            RuntimePrimValue::I16(val) => Ok(val as isize),
            RuntimePrimValue::I32(val) => Ok(val as isize),
            RuntimePrimValue::I64(val) => Ok(val as isize),
            RuntimePrimValue::NativeInt(val) => Ok(val),
            other => Err(Error::ValueNotConvertibleToIndex(other)),
        }
    }

    pub fn as_usize(self) -> Result<usize, Error> {
        match self {
            RuntimePrimValue::U8(val) => Ok(val as usize),
            RuntimePrimValue::U16(val) => Ok(val as usize),
            RuntimePrimValue::U32(val) => Ok(val as usize),
            RuntimePrimValue::U64(val) => Ok(val as usize),
            RuntimePrimValue::NativeUInt(val) => Ok(val as usize),
            RuntimePrimValue::I8(val) if val >= 0 => Ok(val as usize),
            RuntimePrimValue::I16(val) if val >= 0 => Ok(val as usize),
            RuntimePrimValue::I32(val) if val >= 0 => Ok(val as usize),
            RuntimePrimValue::I64(val) if val >= 0 => Ok(val as usize),
            RuntimePrimValue::NativeInt(val) if val >= 0 => Ok(val as usize),
            other => Err(Error::ValueNotConvertibleToIndex(other)),
        }
    }

    fn type_name(&self) -> &'static str {
        match self {
            RuntimePrimValue::Bool(_) => "bool",
            RuntimePrimValue::Char(_) => "char",
            RuntimePrimValue::U8(_) => "u8",
            RuntimePrimValue::U16(_) => "u16",
            RuntimePrimValue::U32(_) => "u32",
            RuntimePrimValue::U64(_) => "u64",
            RuntimePrimValue::NativeUInt(_) => "usize",
            RuntimePrimValue::I8(_) => "i8",
            RuntimePrimValue::I16(_) => "i16",
            RuntimePrimValue::I32(_) => "i32",
            RuntimePrimValue::I64(_) => "i64",
            RuntimePrimValue::NativeInt(_) => "isize",
            RuntimePrimValue::F32(_) => "f32",
            RuntimePrimValue::F64(_) => "f64",
            RuntimePrimValue::Ptr(_) => "ptr",
        }
    }

    pub fn read_string_ptr(
        self,
        reader: &MemoryReader,
    ) -> Result<String, Error> {
        let ptr: Pointer = self.try_into()?;
        let ptr: TypedPointer<RuntimeString> = ptr.into();
        let runtime_string = ptr.read(reader)?;
        Ok(runtime_string.into())
    }
}

impl std::fmt::Display for RuntimePrimValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool(val) => write!(f, "{val}"),
            Self::Char(val) => write!(f, "{val}"),
            Self::U8(val) => write!(f, "{val}"),
            Self::U16(val) => write!(f, "{val}"),
            Self::U32(val) => write!(f, "{val}"),
            Self::U64(val) => write!(f, "{val}"),
            Self::NativeUInt(val) => write!(f, "{val}"),
            Self::I8(val) => write!(f, "{val}"),
            Self::I16(val) => write!(f, "{val}"),
            Self::I32(val) => write!(f, "{val}"),
            Self::I64(val) => write!(f, "{val}"),
            Self::NativeInt(val) => write!(f, "{val}"),
            Self::F32(val) => write!(f, "{val}"),
            Self::F64(val) => write!(f, "{val}"),
            Self::Ptr(val) => write!(f, "{val}"),
        }
    }
}

impl std::fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Prim(val) => write!(f, "{val}"),
            Self::Object(val) if val.is_null() => write!(f, "null"),
            Self::Object(val) => write!(f, "Object@{val}"),
            Self::String(val) if val.is_null() => write!(f, "null"),
            Self::String(val) => write!(f, "String@{val}"),
            Self::Array(val) if val.is_null() => write!(f, "null"),
            Self::Array(val) => write!(f, "Array@{val}"),
            Self::MultiDimArray(val) if val.is_null() => write!(f, "null"),
            Self::MultiDimArray(val) => write!(f, "MultiDimArray@{val}"),
            Self::ValueType { location, .. } => write!(f, "Struct@{location}"),
        }
    }
}

macro_rules! prim_value_into {
    ($variant:ident, $into:ty) => {
        impl TryInto<$into> for RuntimePrimValue {
            type Error = Error;
            fn try_into(self) -> Result<$into, Self::Error> {
                match self {
                    Self::$variant(value) => Ok(value),
                    other => Err(Error::UnexpectedRuntimeValue {
                        expected: stringify!($into),
                        actual: other.type_name(),
                    }),
                }
            }
        }
    };
}
prim_value_into!(Bool, bool);
prim_value_into!(U8, u8);
prim_value_into!(U16, u16);
prim_value_into!(U32, u32);
prim_value_into!(U64, u64);
prim_value_into!(NativeUInt, usize);
prim_value_into!(I8, i8);
prim_value_into!(I16, i16);
prim_value_into!(I32, i32);
prim_value_into!(I64, i64);
prim_value_into!(NativeInt, isize);
prim_value_into!(F32, f32);
prim_value_into!(F64, f64);
prim_value_into!(Ptr, Pointer);
