use derive_more::derive::From;
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
#[derive(Clone, Debug, PartialEq, Eq, Hash, From)]
pub enum RuntimeType {
    // Primitive types.  These have a direct representation within the
    // .NET process, can be parsed from a contiguous chunk of bytes,
    // and have a value that can be represented in `RuntimePrimValue`.
    Prim(RuntimePrimType),

    // Composite types in .NET.  These are represented within the .NET
    // process, and may contain internal member variables, either of
    // primitive types or of other .NET types.  While each .NET object
    // exists at a specific memory location in the remote process,
    // they do not necessarily occuply a contiguous memory range, and
    // may contain pointers to their internal members.
    DotNet(DotNetType),

    // A native Rust type, exposed for use in the VM.  These types do
    // not have a representation in the .NET process.  They can only
    // be produced as the output of a native Rust function.
    Rust(RustType),

    // A function, typically defined using
    // `SymbolicGraph::function_def`.  These represent functions that
    // may be evaluated over the course of the VM's execution.
    //
    // TODO: Represent native Rust functions as nodes within the
    // graph, rather than as extra free-floating functions within the
    // VirtualMachine.  This will allow them to be type-checked.
    Function(FunctionType),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum DotNetType {
    ValueType {
        /// The MethodTable used to unpack the struct.  This must be
        /// determined from the parent type, since structs do not have
        /// a pointer to their own vtable.
        ///
        /// If no instance of this type has been constructed in the
        /// remote process, then the method table will not yet exist.
        /// For example, even if an `Array<Generic<i32>>` exists, if
        /// it is empty, the method table for `Generic<i32>` may not
        /// exist.
        method_table: Option<TypedPointer<MethodTable>>,

        /// The size of the struct, in bytes.
        size: usize,
    },
    Class {
        /// The method table of the class.  This is always the
        /// statically-known type, even if the object itself is a
        /// subclass of the statically-known type.
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
        method_table: Option<TypedPointer<MethodTable>>,
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
        method_table: Option<TypedPointer<MethodTable>>,
        rank: usize,
    },
}

#[derive(Clone, Debug, PartialEq, Eq, Hash, From)]
pub enum RustType {
    Opaque(std::any::TypeId),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionType {
    pub(crate) params: Vec<RuntimeType>,
    pub(crate) outputs: Vec<RuntimeType>,
}

impl RuntimeType {
    pub fn parse(&self, bytes: &[u8]) -> Result<RuntimeValue, Error> {
        match self {
            RuntimeType::Prim(prim) => {
                let prim = prim.parse(bytes)?;
                Ok(prim.into())
            }
            RuntimeType::DotNet(DotNetType::Class { .. }) => {
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
            RuntimeType::DotNet(DotNetType::ValueType { .. }) => {
                Err(Error::ValueTypeRequiresContextualParsing)
            }
            RuntimeType::DotNet(DotNetType::String) => {
                let ptr: Pointer = bytes[..8].try_into().unwrap();
                Ok(RuntimeValue::String(ptr.into()))
            }
            RuntimeType::DotNet(DotNetType::Array { .. }) => {
                let ptr: Pointer = bytes[..8].try_into().unwrap();
                Ok(RuntimeValue::Array(ptr.into()))
            }
            RuntimeType::DotNet(DotNetType::MultiDimArray { .. }) => {
                let ptr: Pointer = bytes[..8].try_into().unwrap();
                Ok(RuntimeValue::MultiDimArray(ptr.into()))
            }
            RuntimeType::Rust(_) => Err(Error::UnexpectedRustTypeInDotNet),
            RuntimeType::Function(_) => {
                Err(Error::UnexpectedFunctionTypeInDotNet)
            }
        }
    }

    pub fn size_bytes(&self) -> Result<usize, Error> {
        match self {
            RuntimeType::Prim(prim) => Ok(prim.size_bytes()),
            RuntimeType::DotNet(DotNetType::ValueType { size, .. }) => {
                Ok(*size)
            }
            RuntimeType::DotNet(DotNetType::Class { .. })
            | RuntimeType::DotNet(DotNetType::String)
            | RuntimeType::DotNet(DotNetType::Array { .. })
            | RuntimeType::DotNet(DotNetType::MultiDimArray { .. }) => {
                Ok(Pointer::SIZE)
            }
            RuntimeType::Rust(_) => Err(Error::UnexpectedRustTypeInDotNet),
            RuntimeType::Function(_) => {
                Err(Error::UnexpectedFunctionTypeInDotNet)
            }
        }
    }

    /// Returns true if instances of this type are held by pointer by
    /// their containing class.
    pub fn stored_as_ptr(&self) -> bool {
        match self {
            RuntimeType::Prim(_) => false,
            RuntimeType::DotNet(DotNetType::ValueType { .. }) => false,
            RuntimeType::DotNet(DotNetType::Class { .. }) => true,
            RuntimeType::DotNet(DotNetType::String) => true,
            RuntimeType::DotNet(DotNetType::Array { .. }) => true,
            RuntimeType::DotNet(DotNetType::MultiDimArray { .. }) => true,
            RuntimeType::Rust(_) | RuntimeType::Function(_) => false,
        }
    }

    pub fn storage_type(&self) -> Option<RuntimePrimType> {
        match self {
            RuntimeType::Prim(prim_type) => Some(*prim_type),
            RuntimeType::DotNet(DotNetType::ValueType { .. }) => None,
            RuntimeType::DotNet(DotNetType::Class { .. }) => {
                Some(RuntimePrimType::Ptr)
            }
            RuntimeType::DotNet(DotNetType::String) => {
                Some(RuntimePrimType::Ptr)
            }
            RuntimeType::DotNet(DotNetType::Array { .. }) => {
                Some(RuntimePrimType::Ptr)
            }
            RuntimeType::DotNet(DotNetType::MultiDimArray { .. }) => {
                Some(RuntimePrimType::Ptr)
            }
            RuntimeType::Rust(_) | RuntimeType::Function(_) => None,
        }
    }

    /// Some types may be represented as either a bytecode, or a named
    /// type.  This function returns the corresponding named type for
    /// builtin instances.
    pub(crate) fn builtin_class_name(&self) -> Option<&'static str> {
        match self {
            RuntimeType::Prim(prim) => prim.builtin_class_name(),
            RuntimeType::DotNet(DotNetType::String) => {
                Some("System.Private.CoreLib.String")
            }
            RuntimeType::DotNet(
                DotNetType::ValueType { .. }
                | DotNetType::Class { .. }
                | DotNetType::Array { .. }
                | DotNetType::MultiDimArray { .. },
            )
            | RuntimeType::Rust(_)
            | RuntimeType::Function(_) => None,
        }
    }

    pub(crate) fn method_table_for_field_access(
        &self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        match self {
            RuntimeType::DotNet(DotNetType::ValueType {
                method_table, ..
            })
            | RuntimeType::DotNet(DotNetType::Class { method_table }) => {
                method_table
                    .ok_or_else(|| Error::UnexpectedNullMethodTable(gen_name()))
            }
            _ => Err(Error::FieldAccessRequiresClassOrStruct(self.clone())),
        }
    }

    pub(crate) fn method_table_for_downcast(
        &self,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        match self {
            RuntimeType::DotNet(DotNetType::Class { method_table }) => {
                method_table.ok_or(Error::DowncastRequiresKnownBaseClass)
            }
            _ => Err(Error::DowncastRequiresClassInstance(self.clone())),
        }
    }

    /// Returns true if the type signature is complete.  If false,
    /// unpacking the same .NET type signature at a later time may
    /// produce a more defined RuntimeType.
    ///
    /// For example, suppose there exists a member variable with type
    /// `Array<MyType>`.  On encountering the member variable, at
    /// least one instance of `Array<MyType>` has been constructed, so
    /// the method table of `Array<MyType>` must have been
    /// instantiated by the .NET runtime.  However, there may not have
    /// been any instances of `MyType` constructed, so the method
    /// table of `MyType` may not yet have been instantiated.  In this
    /// case, it is represented as an array of unspecified element
    /// type.  Such a representation is incomplete, as unpacking the
    /// same signature at a later time could result in an array of
    /// known element type.
    ///
    /// This is used when caching type information, to ensure that the cache is only used when the type information is known.
    pub(crate) fn is_complete(&self) -> bool {
        match self {
            RuntimeType::Prim(_)
            | RuntimeType::DotNet(DotNetType::String)
            | RuntimeType::Rust(_) => true,

            RuntimeType::Function(FunctionType { params, outputs }) => params
                .iter()
                .chain(outputs.iter())
                .all(|ty| ty.is_complete()),

            RuntimeType::DotNet(
                DotNetType::Class { method_table }
                | DotNetType::ValueType { method_table, .. }
                | DotNetType::Array { method_table, .. }
                | DotNetType::MultiDimArray { method_table, .. },
            ) => method_table.is_some(),
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

    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            RuntimePrimType::U8
                | RuntimePrimType::U16
                | RuntimePrimType::U32
                | RuntimePrimType::U64
                | RuntimePrimType::NativeUInt
                | RuntimePrimType::I8
                | RuntimePrimType::I16
                | RuntimePrimType::I32
                | RuntimePrimType::I64
                | RuntimePrimType::NativeInt
        )
    }

    /// Some types may be represented as either a bytecode, or a named
    /// type.  This function returns the corresponding named type for
    /// builtin instances.
    fn builtin_class_name(&self) -> Option<&'static str> {
        match self {
            RuntimePrimType::Bool => Some("System.Private.CoreLib.Boolean"),
            RuntimePrimType::Char => Some("System.Private.CoreLib.Char"),
            RuntimePrimType::U8 => Some("System.Private.CoreLib.Byte"),
            RuntimePrimType::U16 => Some("System.Private.CoreLib.UInt16"),
            RuntimePrimType::U32 => Some("System.Private.CoreLib.UInt32"),
            RuntimePrimType::U64 => Some("System.Private.CoreLib.UInt64"),
            RuntimePrimType::NativeUInt => {
                Some("System.Private.CoreLib.UIntPtr")
            }
            RuntimePrimType::I8 => Some("System.Private.CoreLib.SByte"),
            RuntimePrimType::I16 => Some("System.Private.CoreLib.Int16"),
            RuntimePrimType::I32 => Some("System.Private.CoreLib.Int32"),
            RuntimePrimType::I64 => Some("System.Private.CoreLib.Int64"),
            RuntimePrimType::NativeInt => Some("System.Private.CoreLib.IntPtr"),
            RuntimePrimType::F32 => Some("System.Private.CoreLib.Single"),
            RuntimePrimType::F64 => Some("System.Private.CoreLib.Double"),
            RuntimePrimType::Ptr => None,
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
            let runtime_type = (*self).into();
            Err(Error::InsufficientBytesForValue {
                runtime_type,
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
            RuntimeType::DotNet(DotNetType::ValueType {
                method_table: Some(method_table),
                size,
            }) => {
                write!(f, "struct({size} bytes, vtable {method_table})")
            }
            RuntimeType::DotNet(DotNetType::ValueType {
                method_table: None,
                size,
            }) => {
                write!(f, "struct({size} bytes, unknown vtable)")
            }
            RuntimeType::DotNet(DotNetType::Class { .. }) => {
                write!(f, "Object")
            }
            RuntimeType::DotNet(DotNetType::String) => write!(f, "String"),
            RuntimeType::DotNet(DotNetType::Array {
                method_table: None,
                ..
            }) => {
                write!(f, "array(unknown vtable)")
            }
            RuntimeType::DotNet(DotNetType::Array {
                method_table: Some(method_table),
                ..
            }) => {
                write!(f, "array(vtable {method_table})")
            }
            RuntimeType::DotNet(DotNetType::MultiDimArray {
                method_table: None,
                rank,
            }) => {
                write!(f, "array_nd({rank}, unknown vtable)")
            }
            RuntimeType::DotNet(DotNetType::MultiDimArray {
                method_table: Some(method_table),
                rank,
            }) => {
                write!(f, "array({rank}, vtable {method_table})")
            }
            RuntimeType::Rust(rust_type) => write!(f, "{rust_type}"),
            RuntimeType::Function(func_type) => write!(f, "{func_type}"),
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

impl std::fmt::Display for RustType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RustType::Opaque(type_id) => write!(f, "{type_id:?}"),
        }
    }
}

impl std::fmt::Display for FunctionType {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let write_tuple = |fmt: &mut std::fmt::Formatter,
                           tuple: &[RuntimeType]|
         -> std::fmt::Result {
            write!(fmt, "(")?;
            tuple.iter().enumerate().try_for_each(|(i, element)| {
                if i > 0 {
                    write!(fmt, ", ")?;
                }
                write!(fmt, "{element}")
            })?;
            write!(fmt, ")")?;
            Ok(())
        };

        write!(fmt, "Fn")?;
        write_tuple(fmt, &self.params)?;

        write!(fmt, " -> ")?;
        write_tuple(fmt, &self.outputs)?;

        Ok(())
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

impl From<std::any::TypeId> for RuntimeType {
    fn from(type_id: std::any::TypeId) -> Self {
        RuntimeType::Rust(type_id.into())
    }
}

impl std::cmp::PartialEq<RuntimeType> for RuntimePrimType {
    fn eq(&self, other: &RuntimeType) -> bool {
        match other {
            RuntimeType::Prim(other) => other == self,
            _ => false,
        }
    }
}

impl std::cmp::PartialEq<RuntimePrimType> for RuntimeType {
    fn eq(&self, other: &RuntimePrimType) -> bool {
        other == self
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
