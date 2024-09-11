use memory_reader::Pointer;

use crate::{
    runtime_value::RuntimePrimValue, Error, MethodTable, RuntimeValue,
    TypedPointer,
};

#[derive(Clone, Copy, Debug)]
pub enum RuntimeType {
    Prim(RuntimePrimType),
    ValueType {
        method_table: TypedPointer<MethodTable>,
        size: usize,
    },
    Class,
}

#[derive(Clone, Copy, Debug)]
pub enum RuntimePrimType {
    Bool,
    Char,

    U8,
    U16,
    U32,
    U64,
    NativeUInt,

    I8,
    I16,
    I32,
    I64,
    NativeInt,

    F32,
    F64,

    Ptr,
}

impl RuntimeType {
    pub fn parse(&self, bytes: &[u8]) -> Result<RuntimeValue, Error> {
        match self {
            RuntimeType::Prim(prim) => {
                let prim = prim.parse(bytes)?;
                Ok(RuntimeValue::Prim(prim))
            }
            RuntimeType::Class => {
                let ptr: Pointer = bytes[..8].try_into().unwrap();
                Ok(RuntimeValue::Object(ptr.into()))
            }
            RuntimeType::ValueType { .. } => {
                Err(Error::ValueTypeRequiresContextualParsing)
            }
        }
    }

    pub fn size_bytes(&self) -> usize {
        match self {
            RuntimeType::Prim(prim) => prim.size_bytes(),
            RuntimeType::Class => Pointer::SIZE,
            RuntimeType::ValueType { size, .. } => *size,
        }
    }
}

impl RuntimePrimType {
    pub fn size_bytes(&self) -> usize {
        match self {
            RuntimePrimType::Bool => 1,
            // CLR uses this type to represent an element of UTF-16.
            // This is neither a C/C++ char (1 byte), nor is it a
            // unicode code point (4 bytes), nor is it a unicode code
            // point encoded into UTF-16 (either 2 or 4 bytes).  It is
            // a UTF-16 code unit, and may not have any semantic
            // meaning outside of the string in which it occurs.
            //
            // (From ECMA-335, section III.1.1.3, " CLI char type
            // occupies 2 bytes in memory and represents a Unicode
            // code unit using UTF-16 encoding.")
            RuntimePrimType::Char => 2,
            RuntimePrimType::U8 => 1,
            RuntimePrimType::U16 => 2,
            RuntimePrimType::U32 => 4,
            RuntimePrimType::U64 => 8,
            RuntimePrimType::NativeUInt => Pointer::SIZE,
            RuntimePrimType::I8 => 1,
            RuntimePrimType::I16 => 2,
            RuntimePrimType::I32 => 4,
            RuntimePrimType::I64 => 8,
            RuntimePrimType::NativeInt => Pointer::SIZE,
            RuntimePrimType::F32 => 4,
            RuntimePrimType::F64 => 8,
            RuntimePrimType::Ptr => Pointer::SIZE,
        }
    }

    pub fn parse(&self, bytes: &[u8]) -> Result<RuntimePrimValue, Error> {
        let bytes = self.truncate_to_length(bytes)?;
        match self {
            Self::Bool => Ok(RuntimePrimValue::Bool(bytes[0] > 0)),
            Self::Char => Ok(RuntimePrimValue::Char(u16::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            Self::I8 => Ok(RuntimePrimValue::I8(i8::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            Self::I16 => Ok(RuntimePrimValue::I16(i16::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            Self::I32 => Ok(RuntimePrimValue::I32(i32::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            Self::I64 => Ok(RuntimePrimValue::I64(i64::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            Self::NativeInt => Ok(RuntimePrimValue::NativeInt(
                isize::from_ne_bytes(bytes.try_into().unwrap()),
            )),

            Self::U8 => Ok(RuntimePrimValue::U8(u8::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            Self::U16 => Ok(RuntimePrimValue::U16(u16::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            Self::U32 => Ok(RuntimePrimValue::U32(u32::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            Self::U64 => Ok(RuntimePrimValue::U64(u64::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            Self::NativeUInt => Ok(RuntimePrimValue::NativeUInt(
                usize::from_ne_bytes(bytes.try_into().unwrap()),
            )),

            Self::F32 => Ok(RuntimePrimValue::F32(f32::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            Self::F64 => Ok(RuntimePrimValue::F64(f64::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            Self::Ptr => Ok(RuntimePrimValue::Ptr(bytes.try_into().unwrap())),
        }
    }

    fn truncate_to_length<'a>(
        &self,
        bytes: &'a [u8],
    ) -> Result<&'a [u8], Error> {
        let expected = self.size_bytes();
        let provided = bytes.len();
        if provided >= expected {
            Ok(&bytes[..expected])
        } else {
            Err(Error::InsufficientBytesForValue {
                runtime_type: RuntimeType::Prim(*self),
                provided,
                expected,
            })
        }
    }
}

impl std::fmt::Display for RuntimeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeType::Prim(prim) => write!(f, "{prim}"),
            RuntimeType::ValueType { method_table, size } => {
                write!(f, "struct({size} bytes, vtable {method_table})")
            }
            RuntimeType::Class => write!(f, "Object"),
        }
    }
}

impl std::fmt::Display for RuntimePrimType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimePrimType::Bool => write!(f, "bool"),
            RuntimePrimType::Char => write!(f, "char"),
            RuntimePrimType::U8 => write!(f, "u8"),
            RuntimePrimType::U16 => write!(f, "u16"),
            RuntimePrimType::U32 => write!(f, "u32"),
            RuntimePrimType::U64 => write!(f, "u64"),
            RuntimePrimType::NativeUInt => write!(f, "usize"),
            RuntimePrimType::I8 => write!(f, "i8"),
            RuntimePrimType::I16 => write!(f, "i16"),
            RuntimePrimType::I32 => write!(f, "i32"),
            RuntimePrimType::I64 => write!(f, "i64"),
            RuntimePrimType::NativeInt => write!(f, "isize"),
            RuntimePrimType::F32 => write!(f, "f32"),
            RuntimePrimType::F64 => write!(f, "f64"),
            RuntimePrimType::Ptr => write!(f, "Ptr"),
        }
    }
}
