use memory_reader::Pointer;

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
