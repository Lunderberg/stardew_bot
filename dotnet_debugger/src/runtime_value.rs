use memory_reader::Pointer;

use crate::{Error, RuntimeType};

pub enum RuntimeValue {
    Bool(bool),

    // CLR uses `ELEMENT_TYPE_CHAR` to specify a UTF-16 code unit,
    // whereas Rust uses UTF-8.  This should be converted when
    // constructing the RuntimeValue.
    Char(char),

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

    Object(Pointer),
    // String,
    // Ptr,
    // ByRef,

    // ValueType,
    // Class,
    // Object,

    // Array,
    // GenericInst,
    // TypedByRef,
    // FunctionPtr,
    // SizeArray,
    // MethodType,
}

impl RuntimeValue {
    pub fn parse(kind: RuntimeType, bytes: &[u8]) -> Result<Self, Error> {
        let bytes = Self::truncate_to_length(kind, bytes)?;

        match kind {
            RuntimeType::Bool => Ok(RuntimeValue::Bool(bytes[0] > 0)),
            RuntimeType::Char => todo!(),

            RuntimeType::I8 => Ok(RuntimeValue::I8(i8::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            RuntimeType::I16 => Ok(RuntimeValue::I16(i16::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            RuntimeType::I32 => Ok(RuntimeValue::I32(i32::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            RuntimeType::I64 => Ok(RuntimeValue::I64(i64::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            RuntimeType::NativeInt => Ok(RuntimeValue::NativeInt(
                isize::from_ne_bytes(bytes.try_into().unwrap()),
            )),

            RuntimeType::U8 => Ok(RuntimeValue::U8(u8::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            RuntimeType::U16 => Ok(RuntimeValue::U16(u16::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            RuntimeType::U32 => Ok(RuntimeValue::U32(u32::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            RuntimeType::U64 => Ok(RuntimeValue::U64(u64::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            RuntimeType::NativeUInt => Ok(RuntimeValue::NativeUInt(
                usize::from_ne_bytes(bytes.try_into().unwrap()),
            )),

            RuntimeType::F32 => Ok(RuntimeValue::F32(f32::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),
            RuntimeType::F64 => Ok(RuntimeValue::F64(f64::from_ne_bytes(
                bytes.try_into().unwrap(),
            ))),

            RuntimeType::ValueType | RuntimeType::Class => {
                let ptr: Pointer = bytes[..8].try_into().unwrap();
                Ok(Self::Object(ptr))
            }
            RuntimeType::Object => todo!(),
            RuntimeType::String => todo!(),
            RuntimeType::Ptr => todo!(),

            RuntimeType::Array => todo!(),
            RuntimeType::SizeArray => todo!(),

            RuntimeType::FunctionPtr => todo!(),
            RuntimeType::MethodType => todo!(),

            RuntimeType::ByRef => todo!(),
            RuntimeType::TypedByRef => todo!(),

            RuntimeType::Var => todo!(),
            RuntimeType::GenericInst => todo!(),

            other => Err(Error::NoSuchRuntimeValue(other)),
        }
    }

    fn truncate_to_length(
        runtime_type: RuntimeType,
        bytes: &[u8],
    ) -> Result<&[u8], Error> {
        let expected = runtime_type.size_bytes();
        let provided = bytes.len();
        if provided >= expected {
            Ok(&bytes[..expected])
        } else {
            Err(Error::InsufficientBytesForValue {
                runtime_type,
                provided,
                expected,
            })
        }
    }
}

impl std::fmt::Display for RuntimeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Bool(val) => write!(f, "{val}"),
            RuntimeValue::Char(val) => write!(f, "{val}"),
            RuntimeValue::U8(val) => write!(f, "{val}"),
            RuntimeValue::U16(val) => write!(f, "{val}"),
            RuntimeValue::U32(val) => write!(f, "{val}"),
            RuntimeValue::U64(val) => write!(f, "{val}"),
            RuntimeValue::NativeUInt(val) => write!(f, "{val}"),
            RuntimeValue::I8(val) => write!(f, "{val}"),
            RuntimeValue::I16(val) => write!(f, "{val}"),
            RuntimeValue::I32(val) => write!(f, "{val}"),
            RuntimeValue::I64(val) => write!(f, "{val}"),
            RuntimeValue::NativeInt(val) => write!(f, "{val}"),
            RuntimeValue::F32(val) => write!(f, "{val}"),
            RuntimeValue::F64(val) => write!(f, "{val}"),
            RuntimeValue::Object(val) => write!(f, "{val}"),
        }
    }
}
