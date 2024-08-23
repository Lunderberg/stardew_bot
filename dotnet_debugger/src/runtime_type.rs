use crate::Error;

/// Equivalent representation to CorElementType
#[derive(Clone, Copy, Debug)]
pub enum RuntimeType {
    End,
    Void,
    Bool,
    Char,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    I64,
    U64,
    F32,
    F64,
    String,
    Ptr,
    ByRef,
    ValueType,
    Class,
    Var,
    Array,
    GenericInst,
    TypedByRef,
    NativeInt,
    NativeUInt,
    FunctionPtr,
    Object,
    SizeArray,
    MethodType,
    RequiredCModifier,
    OptionalCModifier,
    Internal,

    // Not sure whether this will need special handling
    Modifier,
    Sentinel,
    Pinned,
}

impl std::fmt::Display for RuntimeType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl TryFrom<u8> for RuntimeType {
    type Error = Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x00 => Ok(Self::End),
            0x01 => Ok(Self::Void),
            0x02 => Ok(Self::Bool),
            0x03 => Ok(Self::Char),
            0x04 => Ok(Self::I8),
            0x05 => Ok(Self::U8),
            0x06 => Ok(Self::I16),
            0x07 => Ok(Self::U16),
            0x08 => Ok(Self::I32),
            0x09 => Ok(Self::U32),
            0x0a => Ok(Self::I64),
            0x0b => Ok(Self::U64),
            0x0c => Ok(Self::F32),
            0x0d => Ok(Self::F64),
            0x0e => Ok(Self::String),
            0x0f => Ok(Self::Ptr),
            0x10 => Ok(Self::ByRef),
            0x11 => Ok(Self::ValueType),
            0x12 => Ok(Self::Class),
            0x13 => Ok(Self::Var),
            0x14 => Ok(Self::Array),
            0x15 => Ok(Self::GenericInst),
            0x16 => Ok(Self::TypedByRef),
            0x18 => Ok(Self::NativeInt),
            0x19 => Ok(Self::NativeUInt),
            0x1b => Ok(Self::FunctionPtr),
            0x1c => Ok(Self::Object),
            0x1d => Ok(Self::SizeArray),
            0x1e => Ok(Self::MethodType),
            0x1f => Ok(Self::RequiredCModifier),
            0x20 => Ok(Self::OptionalCModifier),
            0x21 => Ok(Self::Internal),
            0x40 => Ok(Self::Modifier),
            0x41 => Ok(Self::Sentinel),
            0x45 => Ok(Self::Pinned),
            value => Err(Error::InvalidRuntimeType(value)),
        }
    }
}

impl RuntimeType {
    pub fn is_ptr(self) -> bool {
        matches!(self, Self::Ptr | Self::Object | Self::Class)
    }

    pub fn size_bytes(self) -> usize {
        match self {
            // RuntimeType::End => todo!(),
            // RuntimeType::Void => todo!(),
            RuntimeType::Bool => 1,
            // RuntimeType::Char => todo!(),
            RuntimeType::I8 => 1,
            RuntimeType::U8 => 1,
            RuntimeType::I16 => 2,
            RuntimeType::U16 => 2,
            RuntimeType::I32 => 4,
            RuntimeType::U32 => 4,
            RuntimeType::I64 => 8,
            RuntimeType::U64 => 8,
            RuntimeType::F32 => 4,
            RuntimeType::F64 => 8,
            // RuntimeType::String => todo!(),
            RuntimeType::Ptr => 8,
            // RuntimeType::ByRef => todo!(),
            // RuntimeType::ValueType => todo!(),
            RuntimeType::Class => 8,
            // RuntimeType::Var => todo!(),
            // RuntimeType::Array => todo!(),
            // RuntimeType::GenericInst => todo!(),
            // RuntimeType::TypedByRef => todo!(),
            // RuntimeType::NativeInt => todo!(),
            // RuntimeType::NativeUInt => todo!(),
            // RuntimeType::FunctionPtr => todo!(),
            RuntimeType::Object => 8,
            // RuntimeType::SizeArray => todo!(),
            // RuntimeType::MethodType => todo!(),
            // RuntimeType::RequiredCModifier => todo!(),
            // RuntimeType::OptionalCModifier => todo!(),
            // RuntimeType::Internal => todo!(),
            // RuntimeType::Modifier => todo!(),
            // RuntimeType::Sentinel => todo!(),
            // RuntimeType::Pinned => todo!(),
            other => {
                println!("Encountered RuntimeType {other}");
                1
            }
        }
    }
}