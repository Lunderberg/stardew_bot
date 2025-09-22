use thiserror::Error;

use dll_unpacker::{MetadataCodedIndex, TypeDefOrRef};
use format_utils::MaybeZeroWidthSpace;

use crate::{Error, RuntimePrimType};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SymbolicType {
    Named {
        module: Option<String>,
        name: String,
    },
    Metadata {
        module: String,
        index: MetadataCodedIndex<TypeDefOrRef>,
    },
    GenericInst {
        base: Box<SymbolicType>,
        args: Vec<SymbolicType>,
    },
    Array(Box<SymbolicType>),
    MultiDimArray {
        element_type: Box<SymbolicType>,
        rank: usize,
    },
}

#[derive(Clone, Copy)]
struct TypePrinter<'a> {
    ty: &'a SymbolicType,
    insert_zero_width_space_at_breakpoint: bool,
}

#[derive(Error)]
pub enum SymbolicTypeError {
    #[error(
        "Generic type '{0}' should use SymbolicType::GenericInst, \
         but instead used SymbolicType::Name"
    )]
    NameContainsGenericType(String),

    #[error(
        "Array type '{0}' should use SymbolicType::Array, \
         but instead used SymbolicType::Name"
    )]
    NameContainsArray(String),
}

impl SymbolicType {
    pub fn named(name: String, module: Option<String>) -> Self {
        let ty = Self::Named { name, module };
        ty.validate().unwrap();
        ty
    }

    pub fn try_prim_type(&self) -> Option<RuntimePrimType> {
        let SymbolicType::Named { name, .. } = self else {
            return None;
        };
        match name.as_str() {
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

    pub fn with_type_args(
        self,
        args: impl IntoIterator<Item = SymbolicType>,
    ) -> SymbolicType {
        let base = Box::new(self);
        let args = args.into_iter().collect();
        SymbolicType::GenericInst { base, args }
    }

    pub fn array_of(self) -> SymbolicType {
        SymbolicType::Array(Box::new(self))
    }

    pub fn printer(
        &self,
        insert_zero_width_space_at_breakpoint: bool,
    ) -> impl std::fmt::Display + '_ {
        TypePrinter {
            ty: self,
            insert_zero_width_space_at_breakpoint,
        }
    }

    pub fn validate(&self) -> Result<(), Error> {
        use SymbolicTypeError as SymErr;
        match self {
            SymbolicType::Named { name, .. } if name.contains('<') => {
                Err(SymErr::NameContainsGenericType(name.clone()))
            }
            SymbolicType::Named { name, .. } if name.contains("[]") => {
                Err(SymErr::NameContainsArray(name.clone()))
            }
            _ => Ok(()),
        }
        .map_err(Into::into)
    }
}

impl From<String> for SymbolicType {
    fn from(full_name: String) -> Self {
        Self::named(full_name, None)
    }
}

impl From<&str> for SymbolicType {
    fn from(full_name: &str) -> Self {
        full_name.to_string().into()
    }
}

impl<'a> TypePrinter<'a> {
    fn with_ty<'b>(&self, ty: &'b SymbolicType) -> TypePrinter<'b> {
        TypePrinter {
            ty,
            insert_zero_width_space_at_breakpoint: self
                .insert_zero_width_space_at_breakpoint,
        }
    }
}

impl<'a> std::fmt::Display for TypePrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sep =
            MaybeZeroWidthSpace(self.insert_zero_width_space_at_breakpoint);

        match &self.ty {
            SymbolicType::Named { name, module } => {
                let delim = if name.contains('`') { "\"" } else { "" };
                if let Some(module) = module {
                    write!(f, "{module}::{delim}{name}{delim}")?;
                } else {
                    write!(f, "{delim}{name}{delim}")?;
                }
            }
            SymbolicType::Metadata { module, index } => {
                write!(f, "{module}::{index}")?;
            }
            SymbolicType::GenericInst { base, args } => {
                write!(f, "{}", self.with_ty(base))?;
                if args.is_empty() {
                    write!(f, "<>")?;
                } else {
                    write!(f, "<{sep}")?;
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", {sep}")?;
                        }
                        write!(f, "{}", self.with_ty(arg))?;
                    }
                    write!(f, ">")?;
                }
            }
            SymbolicType::Array(element) => {
                write!(f, "{}", self.with_ty(element))?;
                write!(f, "[]")?;
            }
            SymbolicType::MultiDimArray { element_type, rank } => {
                write!(
                    f,
                    "array(rank {rank}, {})",
                    self.with_ty(element_type)
                )?;
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for SymbolicType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let printer = TypePrinter {
            ty: self,
            insert_zero_width_space_at_breakpoint: false,
        };
        write!(f, "{printer}")
    }
}

impl std::fmt::Debug for SymbolicTypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
