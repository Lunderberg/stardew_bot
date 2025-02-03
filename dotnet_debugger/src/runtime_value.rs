use memory_reader::{MemoryReader, Pointer};

use crate::runtime_type::RuntimePrimType;
use crate::{
    Error, MethodTable, RuntimeArray, RuntimeMultiDimArray, RuntimeObject,
    RuntimeString, TypedPointer,
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

#[derive(Debug, Clone, Copy, PartialEq)]
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
                runtime_type: runtime_type.into(),
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

    pub fn prim_cast(self, prim_type: RuntimePrimType) -> Result<Self, Error> {
        use RuntimePrimType as Type;
        match (prim_type, self) {
            (Type::Bool, Self::Bool(_))
            | (Type::Char, Self::Char(_))
            | (Type::U8, Self::U8(_))
            | (Type::U16, Self::U16(_))
            | (Type::U32, Self::U32(_))
            | (Type::U64, Self::U64(_))
            | (Type::NativeUInt, Self::NativeUInt(_))
            | (Type::I8, Self::I8(_))
            | (Type::I16, Self::I16(_))
            | (Type::I32, Self::I32(_))
            | (Type::I64, Self::I64(_))
            | (Type::NativeInt, Self::NativeInt(_))
            | (Type::F32, Self::F32(_))
            | (Type::F64, Self::F64(_))
            | (Type::Ptr, Self::Ptr(_)) => Ok(self),

            (Type::NativeUInt, Self::U8(value)) => Ok((value as usize).into()),
            (Type::NativeUInt, Self::U16(value)) => Ok((value as usize).into()),
            (Type::NativeUInt, Self::U32(value)) => Ok((value as usize).into()),
            (Type::NativeUInt, Self::U64(value)) => Ok((value as usize).into()),
            (Type::NativeUInt, Self::I8(value)) => Ok((value as usize).into()),
            (Type::NativeUInt, Self::I16(value)) => Ok((value as usize).into()),
            (Type::NativeUInt, Self::I32(value)) => Ok((value as usize).into()),
            (Type::NativeUInt, Self::I64(value)) => Ok((value as usize).into()),
            (Type::NativeUInt, Self::NativeInt(value)) => {
                Ok((value as usize).into())
            }

            (Type::NativeInt, Self::U8(value)) => Ok((value as isize).into()),
            (Type::NativeInt, Self::U16(value)) => Ok((value as isize).into()),
            (Type::NativeInt, Self::U32(value)) => Ok((value as isize).into()),
            (Type::NativeInt, Self::U64(value)) => Ok((value as isize).into()),
            (Type::NativeInt, Self::NativeUInt(value)) => {
                Ok((value as isize).into())
            }
            (Type::NativeInt, Self::I8(value)) => Ok((value as isize).into()),
            (Type::NativeInt, Self::I16(value)) => Ok((value as isize).into()),
            (Type::NativeInt, Self::I32(value)) => Ok((value as isize).into()),
            (Type::NativeInt, Self::I64(value)) => Ok((value as isize).into()),

            (Type::F32, Self::F64(value)) => Ok((value as f32).into()),

            (Type::F64, Self::F32(value)) => Ok((value as f64).into()),

            (prim_type, value) => {
                Err(Error::InvalidPrimCast { value, prim_type })
            }
        }
    }

    pub fn as_type<T>(self) -> Result<T, <Self as TryInto<T>>::Error>
    where
        Self: TryInto<T>,
    {
        self.try_into()
    }

    pub fn as_isize(self) -> Result<isize, Error> {
        self.try_into()
    }

    pub fn as_usize(self) -> Result<usize, Error> {
        self.try_into()
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

impl From<RuntimePrimValue> for RuntimeValue {
    fn from(prim: RuntimePrimValue) -> Self {
        RuntimeValue::Prim(prim)
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

macro_rules! prim_value_from_native_prim {
    ($variant:ident, $prim:ty) => {
        impl From<$prim> for RuntimePrimValue {
            fn from(value: $prim) -> Self {
                Self::$variant(value)
            }
        }
    };
}

macro_rules! prim_value_conversions {
    ($variant:ident, $prim:ty) => {
        prim_value_from_native_prim!($variant, $prim);

        impl TryInto<$prim> for RuntimePrimValue {
            type Error = Error;
            fn try_into(self) -> Result<$prim, Self::Error> {
                match self {
                    Self::$variant(value) => Ok(value),
                    other => Err(Error::UnexpectedRuntimeValue {
                        expected: stringify!($prim),
                        actual: other.type_name(),
                    }),
                }
            }
        }
    };
}
prim_value_conversions!(Bool, bool);
prim_value_conversions!(U8, u8);
prim_value_conversions!(U16, u16);
prim_value_conversions!(U32, u32);
prim_value_conversions!(U64, u64);
prim_value_from_native_prim!(NativeUInt, usize);
prim_value_conversions!(I8, i8);
prim_value_conversions!(I16, i16);
prim_value_conversions!(I32, i32);
prim_value_conversions!(I64, i64);
prim_value_from_native_prim!(NativeInt, isize);
prim_value_conversions!(F32, f32);
prim_value_conversions!(F64, f64);
prim_value_conversions!(Ptr, Pointer);

impl TryInto<usize> for RuntimePrimValue {
    type Error = Error;
    fn try_into(self) -> Result<usize, Self::Error> {
        match self {
            Self::U8(val) => Ok(val as usize),
            Self::U16(val) => Ok(val as usize),
            Self::U32(val) => Ok(val as usize),
            Self::U64(val) => Ok(val as usize),
            Self::NativeUInt(val) => Ok(val as usize),
            Self::I8(val) if val >= 0 => Ok(val as usize),
            Self::I16(val) if val >= 0 => Ok(val as usize),
            Self::I32(val) if val >= 0 => Ok(val as usize),
            Self::I64(val) if val >= 0 => Ok(val as usize),
            Self::NativeInt(val) if val >= 0 => Ok(val as usize),
            other => Err(Error::ValueNotConvertibleToIndex(
                other,
                other.runtime_type(),
            )),
        }
    }
}

impl TryInto<isize> for RuntimePrimValue {
    type Error = Error;
    fn try_into(self) -> Result<isize, Self::Error> {
        match self {
            Self::U8(val) => Ok(val as isize),
            Self::U16(val) => Ok(val as isize),
            Self::U32(val) => Ok(val as isize),
            Self::U64(val) => Ok(val as isize),
            Self::NativeUInt(val) => Ok(val as isize),
            Self::I8(val) => Ok(val as isize),
            Self::I16(val) => Ok(val as isize),
            Self::I32(val) => Ok(val as isize),
            Self::I64(val) => Ok(val as isize),
            Self::NativeInt(val) => Ok(val),
            other => Err(Error::ValueNotConvertibleToIndex(
                other,
                other.runtime_type(),
            )),
        }
    }
}
