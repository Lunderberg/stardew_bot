use crate::{runtime_type::RuntimePrimType, Error};

/// Equivalent representation to CorElementType
#[derive(Clone, Copy, Debug)]
pub enum CorElementType {
    End,
    Void,
    Prim(RuntimePrimType),
    String,
    ByRef,
    ValueType,
    Class,
    Var,
    Array,
    GenericInst,
    TypedByRef,
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

impl std::fmt::Display for CorElementType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl TryFrom<u8> for CorElementType {
    type Error = Error;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x00 => Ok(Self::End),
            0x01 => Ok(Self::Void),
            0x02 => Ok(Self::Prim(RuntimePrimType::Bool)),
            0x03 => Ok(Self::Prim(RuntimePrimType::Char)),
            0x04 => Ok(Self::Prim(RuntimePrimType::I8)),
            0x05 => Ok(Self::Prim(RuntimePrimType::U8)),
            0x06 => Ok(Self::Prim(RuntimePrimType::I16)),
            0x07 => Ok(Self::Prim(RuntimePrimType::U16)),
            0x08 => Ok(Self::Prim(RuntimePrimType::I32)),
            0x09 => Ok(Self::Prim(RuntimePrimType::U32)),
            0x0a => Ok(Self::Prim(RuntimePrimType::I64)),
            0x0b => Ok(Self::Prim(RuntimePrimType::U64)),
            0x0c => Ok(Self::Prim(RuntimePrimType::F32)),
            0x0d => Ok(Self::Prim(RuntimePrimType::F64)),
            0x0e => Ok(Self::String),
            0x0f => Ok(Self::Prim(RuntimePrimType::Ptr)),
            0x10 => Ok(Self::ByRef),
            0x11 => Ok(Self::ValueType),
            0x12 => Ok(Self::Class),
            0x13 => Ok(Self::Var),
            0x14 => Ok(Self::Array),
            0x15 => Ok(Self::GenericInst),
            0x16 => Ok(Self::TypedByRef),
            0x18 => Ok(Self::Prim(RuntimePrimType::NativeInt)),
            0x19 => Ok(Self::Prim(RuntimePrimType::NativeUInt)),
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

impl CorElementType {
    pub fn is_ptr(self) -> bool {
        matches!(
            self,
            Self::Prim(RuntimePrimType::Ptr) | Self::Object | Self::Class
        )
    }
}
