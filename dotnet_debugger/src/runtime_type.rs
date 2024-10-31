use memory_reader::Pointer;

use crate::{
    runtime_value::RuntimePrimValue, Error, MethodTable, RuntimeValue,
    TypedPointer,
};

/// The runtime representation of the static type of a field.
///
/// This isn't really the static type of a field, because that
/// wouldn't have a `MethodTable` at all.  The static type of a field
/// is the `Signature`, and may refer to classes by name, that exist
/// in other assemblies.
///
/// But this also isn't really the runtime type of a field, because
/// that would imply the exact type of a field.  For a field of type
/// `BaseClass`, holding an instance of type `DerivedClass`, this
/// would refer to the method table of `BaseClass`.
///
/// So in absence of a better name, calling it `RuntimeType` for now.
#[derive(Clone, Debug, PartialEq)]
pub enum RuntimeType {
    Prim(RuntimePrimType),
    ValueType {
        /// The MethodTable used to unpack the struct.  This must be
        /// determined from the parent type, since structs do not have
        /// a pointer to their own vtable.
        method_table: TypedPointer<MethodTable>,

        /// The size of the struct, in bytes.
        size: usize,
    },
    Class {
        /// The method table of the class.  This
        ///
        /// While all objects must have a MethodTable, a MethodTable
        /// may not yet exist in the remote process.  If no instances
        /// of a class have been loaded, the MethodTable pointer may
        /// not yet exist
        method_table: Option<TypedPointer<MethodTable>>,
    },

    /// A `System.String` instance.
    ///
    /// This may be identified from a signature as
    /// `COR_ELEMENT_STRING`, or may be identified from a method table
    /// as being the only Class that has a component size (2 bytes per
    /// wchar_t).
    String,

    /// A 1-d array
    ///
    /// This may be identified from a signature as
    /// `COR_ELEMENT_SZARRAY`, or may be identified from a method
    /// table as having the is-array bit set, along with the
    /// if-array-then-sz-array bit.
    Array {
        element_type: Option<Box<RuntimeType>>,
    },

    /// A multi-dimensional array
    ///
    /// This may be identified from a signature as
    /// `COR_ELEMENT_ARRAY`, or may be identified from a method table
    /// as having the is-array bit set, but not the
    /// if-array-then-sz-array bit.
    ///
    /// Even if `rank == 1`, this has distinct semantics and distinct
    /// layout from `RuntimeType::Array`.  Semantically, the multi-dim
    /// array may have a fixed size known at compile-time.  Laid out
    /// in memory, a regular 1-d array has a 16-byte header (8-byte
    /// pointer and 8-byte size), where a 1-d multi-dim array has an
    /// additional 4-byte size of its only dimension and a 4-byte
    /// lower bound of that dimension.
    MultiDimArray {
        element_type: Option<Box<RuntimeType>>,
        rank: usize,
    },
}

#[derive(Clone, Copy, Debug, PartialEq)]
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
            RuntimeType::Class { .. } => {
                // The RuntimeType holds a pointer to the
                // statically-known type, while the object holds a
                // pointer to the instance's type.  For example,
                // element of `Array<Object>` would have a RuntimeType
                // pointing to `Object`, but each element may be a
                // subclass of `Object`.  Therefore, in order to know
                // the actual type and continue unpacking the object,
                // we need to read the method table of the
                // instance.
                let ptr: Pointer = bytes[..8].try_into().unwrap();
                Ok(RuntimeValue::Object(ptr.into()))
            }
            RuntimeType::ValueType { .. } => {
                Err(Error::ValueTypeRequiresContextualParsing)
            }
            RuntimeType::String => {
                let ptr: Pointer = bytes[..8].try_into().unwrap();
                Ok(RuntimeValue::String(ptr.into()))
            }
            RuntimeType::Array { .. } => {
                let ptr: Pointer = bytes[..8].try_into().unwrap();
                Ok(RuntimeValue::Array(ptr.into()))
            }
            RuntimeType::MultiDimArray { .. } => {
                let ptr: Pointer = bytes[..8].try_into().unwrap();
                Ok(RuntimeValue::MultiDimArray(ptr.into()))
            }
        }
    }

    pub fn size_bytes(&self) -> usize {
        match self {
            RuntimeType::Prim(prim) => prim.size_bytes(),
            RuntimeType::ValueType { size, .. } => *size,
            RuntimeType::Class { .. }
            | RuntimeType::String
            | RuntimeType::Array { .. }
            | RuntimeType::MultiDimArray { .. } => Pointer::SIZE,
        }
    }

    /// Returns true if instances of this type are held by pointer by
    /// their containing class.
    pub fn stored_as_ptr(&self) -> bool {
        match self {
            RuntimeType::Prim(_) => false,
            RuntimeType::ValueType { .. } => false,
            RuntimeType::Class { .. } => true,
            RuntimeType::String => true,
            RuntimeType::Array { .. } => true,
            RuntimeType::MultiDimArray { .. } => true,
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
            RuntimeType::Class { .. } => write!(f, "Object"),
            RuntimeType::String => write!(f, "String"),
            RuntimeType::Array { element_type } => {
                match element_type {
                    Some(ty) => write!(f, "{ty}")?,
                    None => write!(f, "(not-yet-loaded)")?,
                }
                write!(f, "[]")
            }
            RuntimeType::MultiDimArray { element_type, rank } => {
                match element_type {
                    Some(ty) => write!(f, "{ty}")?,
                    None => write!(f, "(not-yet-loaded)")?,
                }
                write!(f, "[")?;

                for _ in 0..*rank - 1 {
                    write!(f, ",")?;
                }
                write!(f, "]")
            }
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

// This seems a bit silly to have separate types with identical
// members, but only the SignaturePrimType and RuntimePrimType have
// this strong of a correspondence.  For all other types, there's
// extra information in the signature type (e.g. the
// `MetadataCodedIndex` of a class or struct) or extra information in
// the runtime type (e.g. pointers to method tables).  Since it
// wouldn't make sense to combine the entire hierarchy of
// signature/runtime types, keeping them entirely separate for now,
// even though that duplicates the signature/runtime prim type.
impl From<dll_unpacker::SignaturePrimType> for RuntimePrimType {
    fn from(value: dll_unpacker::SignaturePrimType) -> Self {
        match value {
            dll_unpacker::SignaturePrimType::Bool => Self::Bool,
            dll_unpacker::SignaturePrimType::Char => Self::Char,
            dll_unpacker::SignaturePrimType::I8 => Self::I8,
            dll_unpacker::SignaturePrimType::U8 => Self::U8,
            dll_unpacker::SignaturePrimType::I16 => Self::I16,
            dll_unpacker::SignaturePrimType::U16 => Self::U16,
            dll_unpacker::SignaturePrimType::I32 => Self::I32,
            dll_unpacker::SignaturePrimType::U32 => Self::U32,
            dll_unpacker::SignaturePrimType::I64 => Self::I64,
            dll_unpacker::SignaturePrimType::U64 => Self::U64,
            dll_unpacker::SignaturePrimType::F32 => Self::F32,
            dll_unpacker::SignaturePrimType::F64 => Self::F64,
            dll_unpacker::SignaturePrimType::NativeInt => Self::NativeInt,
            dll_unpacker::SignaturePrimType::NativeUInt => Self::NativeUInt,
        }
    }
}

impl std::cmp::PartialEq<dll_unpacker::SignaturePrimType> for RuntimePrimType {
    fn eq(&self, other: &dll_unpacker::SignaturePrimType) -> bool {
        let other: Self = (*other).into();
        *self == other
    }
}

impl std::cmp::PartialEq<RuntimePrimType> for dll_unpacker::SignaturePrimType {
    fn eq(&self, other: &RuntimePrimType) -> bool {
        other == self
    }
}
