use crate::RuntimePrimType;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolicType {
    /// The full name of the type, including the namespace, if any.
    pub full_name: String,
    pub generics: Vec<SymbolicType>,
}

impl SymbolicType {
    pub(crate) fn try_prim_type(&self) -> Option<RuntimePrimType> {
        match self.full_name.as_str() {
            "bool" => Some(RuntimePrimType::Bool),
            "char" => Some(RuntimePrimType::Char),
            "u8" => Some(RuntimePrimType::U8),
            "u16" => Some(RuntimePrimType::U16),
            "u32" => Some(RuntimePrimType::U32),
            "u64" => Some(RuntimePrimType::U64),
            "usize" => Some(RuntimePrimType::NativeUInt),
            "i8" => Some(RuntimePrimType::I8),
            "i16" => Some(RuntimePrimType::I16),
            "i32" => Some(RuntimePrimType::I32),
            "i64" => Some(RuntimePrimType::I64),
            "isize" => Some(RuntimePrimType::NativeInt),
            "f32" => Some(RuntimePrimType::F32),
            "f64" => Some(RuntimePrimType::F64),
            "Pointer" | "ptr" | "Ptr" => Some(RuntimePrimType::Ptr),
            _ => None,
        }
    }
}

impl<T> From<T> for SymbolicType
where
    T: Into<String>,
{
    fn from(full_name: T) -> Self {
        Self {
            full_name: full_name.into(),
            generics: Vec::new(),
        }
    }
}
