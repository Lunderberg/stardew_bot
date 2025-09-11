use std::borrow::Cow;

use derive_more::derive::From;
use memory_reader::{Pointer, TypedPointer};

use dotnet_debugger::{DotNetType, MethodTable, RuntimeType};

use crate::{
    Error, ExposedNativeObject, RuntimePrimType, RuntimePrimValue,
    RustNativeObject, RustNativeTypeUtils, RustNativeUtilContainer, StackValue,
    TypeInferenceError, VMExecutionError,
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, From)]
pub enum DSLType {
    // An unknown type, such as the return value of an opaque
    // function.
    Unknown,

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

    // A tuple of return values.
    Tuple(TupleType),

    // An iterator over some underlying type
    Iterator(IteratorType),

    // An array of bytes that have been read from the remote process.
    ByteArray,
}

pub struct RustType {
    type_id: std::any::TypeId,
    utils: Box<dyn RustNativeTypeUtils>,
}

impl RustType {
    pub(crate) fn new<T: RustNativeObject>() -> Self {
        let type_id = std::any::TypeId::of::<T>();
        let utils = Box::new(RustNativeUtilContainer::<T>::new());

        Self { type_id, utils }
    }

    pub(crate) fn new_vector(&self) -> Result<ExposedNativeObject, Error> {
        self.utils.new_vector()
    }

    pub(crate) fn collect_into_vector(
        &self,
        vec: &mut StackValue,
        item: &mut Option<StackValue>,
        output_name: &str,
    ) -> Result<(), Error> {
        self.utils.collect_into_vector(vec, item, output_name)
    }

    pub(crate) fn vector_type(&self) -> Result<DSLType, Error> {
        self.utils.vector_type()
    }

    pub(crate) fn type_name(&self) -> Cow<'static, str> {
        self.utils.type_name()
    }
}

impl Clone for RustType {
    fn clone(&self) -> Self {
        Self {
            type_id: self.type_id,
            utils: self.utils.clone(),
        }
    }
}
impl std::fmt::Debug for RustType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("RustType")
            .field("type_id", &self.type_id)
            .finish()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionType {
    /// The parameter types accepted by the function.  Should be
    /// `None` for functions with unknown number of arguments.  For
    /// functions with known arity but unknown argument types, should
    /// be a vector with all elements being `DSLType::Unknown`.
    pub(crate) params: Option<Vec<DSLType>>,

    /// The return type of the function
    pub(crate) output: Box<DSLType>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TupleType(pub(crate) Vec<DSLType>);

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct IteratorType {
    pub(crate) item: Box<DSLType>,
}

impl DSLType {
    pub fn size_bytes(&self) -> Result<usize, Error> {
        match self {
            DSLType::Prim(prim) => Ok(prim.size_bytes()),
            DSLType::DotNet(DotNetType::ValueType { size, .. }) => Ok(*size),
            DSLType::DotNet(DotNetType::Class { .. })
            | DSLType::DotNet(DotNetType::String)
            | DSLType::DotNet(DotNetType::Array { .. })
            | DSLType::DotNet(DotNetType::MultiDimArray { .. }) => {
                Ok(Pointer::SIZE)
            }

            other @ (DSLType::Unknown
            | DSLType::Rust(_)
            | DSLType::Function(_)
            | DSLType::Tuple(_)
            | DSLType::Iterator(_)
            | DSLType::ByteArray) => {
                Err(Error::UnexpectedTypeFoundInDotNetContext(other.clone()))
            }
        }
    }

    pub fn storage_type(&self) -> Option<RuntimePrimType> {
        match self {
            DSLType::Prim(prim_type) => Some(*prim_type),
            DSLType::DotNet(DotNetType::ValueType { .. }) => None,
            DSLType::DotNet(DotNetType::Class { .. }) => {
                Some(RuntimePrimType::Ptr)
            }
            DSLType::DotNet(DotNetType::String) => Some(RuntimePrimType::Ptr),
            DSLType::DotNet(DotNetType::Array { .. }) => {
                Some(RuntimePrimType::Ptr)
            }
            DSLType::DotNet(DotNetType::MultiDimArray { .. }) => {
                Some(RuntimePrimType::Ptr)
            }
            DSLType::Unknown
            | DSLType::Rust(_)
            | DSLType::Function(_)
            | DSLType::Tuple(_)
            | DSLType::Iterator(_)
            | DSLType::ByteArray => None,
        }
    }

    pub(crate) fn method_table_for_field_access(
        &self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        match self {
            DSLType::DotNet(DotNetType::ValueType { method_table, .. })
            | DSLType::DotNet(DotNetType::Class { method_table }) => {
                method_table
                    .ok_or_else(|| Error::UnexpectedNullMethodTable(gen_name()))
            }
            _ => Err(Error::FieldAccessRequiresClassOrStruct(
                gen_name(),
                self.clone(),
            )),
        }
    }

    pub(crate) fn method_table_for_downcast(
        &self,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        match self {
            DSLType::DotNet(DotNetType::Class { method_table }) => {
                method_table.ok_or(Error::DowncastRequiresKnownBaseClass)
            }
            _ => Err(Error::DowncastRequiresClassInstance(self.clone())),
        }
    }

    pub(crate) fn new_vector(&self) -> Result<ExposedNativeObject, Error> {
        match self {
            DSLType::Prim(prim_type) => Ok(prim_type.new_vector()),
            DSLType::Rust(rust_type) => rust_type.new_vector(),
            other => {
                Err(TypeInferenceError::InvalidVectorElementType(other.clone())
                    .into())
            }
        }
    }

    pub(crate) fn collect_into_vector(
        &self,
        vec: &mut StackValue,
        item: &mut Option<StackValue>,
        output_name: &str,
    ) -> Result<(), Error> {
        match self {
            DSLType::Prim(prim_type) => {
                prim_type.collect_into_vector(vec, item, output_name)
            }
            DSLType::Rust(rust_type) => {
                rust_type.collect_into_vector(vec, item, output_name)
            }
            other => {
                Err(TypeInferenceError::InvalidVectorElementType(other.clone())
                    .into())
            }
        }
    }

    pub(crate) fn vector_type(&self) -> Result<DSLType, Error> {
        match self {
            DSLType::Prim(prim_type) => Ok(prim_type.vector_type()),
            DSLType::Rust(rust_type) => rust_type.vector_type(),
            other => {
                Err(TypeInferenceError::InvalidVectorElementType(other.clone())
                    .into())
            }
        }
    }
}

impl std::fmt::Display for DSLType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            DSLType::Unknown => write!(f, "(???)"),
            DSLType::Prim(prim) => write!(f, "{prim}"),
            DSLType::DotNet(DotNetType::ValueType {
                method_table: Some(method_table),
                size,
            }) => {
                write!(f, "struct({size} bytes, vtable {method_table})")
            }
            DSLType::DotNet(DotNetType::ValueType {
                method_table: None,
                size,
            }) => {
                write!(f, "struct({size} bytes, unknown vtable)")
            }
            DSLType::DotNet(DotNetType::Class {
                method_table: Some(method_table),
            }) => {
                write!(f, "Object(vtable {method_table})")
            }
            DSLType::DotNet(DotNetType::Class { method_table: None }) => {
                write!(f, "Object(unknown vtable)")
            }
            DSLType::DotNet(DotNetType::String) => write!(f, "String"),
            DSLType::DotNet(DotNetType::Array {
                method_table: None, ..
            }) => {
                write!(f, "array(unknown vtable)")
            }
            DSLType::DotNet(DotNetType::Array {
                method_table: Some(method_table),
                ..
            }) => {
                write!(f, "array(vtable {method_table})")
            }
            DSLType::DotNet(DotNetType::MultiDimArray {
                method_table: None,
                rank,
            }) => {
                write!(f, "array_nd({rank}, unknown vtable)")
            }
            DSLType::DotNet(DotNetType::MultiDimArray {
                method_table: Some(method_table),
                rank,
            }) => {
                write!(f, "array({rank}, vtable {method_table})")
            }
            DSLType::Rust(rust_type) => write!(f, "{rust_type}"),
            DSLType::Function(func_type) => write!(f, "{func_type}"),
            DSLType::Tuple(tuple_type) => write!(f, "{tuple_type}"),
            DSLType::Iterator(iterator_type) => {
                write!(f, "{iterator_type}")
            }
            DSLType::ByteArray => write!(f, "ByteArray"),
        }
    }
}

impl std::fmt::Display for RustType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.type_name())
    }
}

impl std::fmt::Display for FunctionType {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let write_tuple = |fmt: &mut std::fmt::Formatter,
                           tuple: &[DSLType]|
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
        if let Some(params) = &self.params {
            write_tuple(fmt, params)?;
        } else {
            write!(fmt, "(...)")?;
        }

        write!(fmt, " -> {}", self.output)?;

        Ok(())
    }
}

impl std::fmt::Display for TupleType {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "(")?;
        for (i, element) in self.0.iter().enumerate() {
            if i > 0 {
                write!(fmt, ", ")?;
            }
            write!(fmt, "{element}")?;
        }
        write!(fmt, ")")?;
        Ok(())
    }
}

impl std::fmt::Display for IteratorType {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(fmt, "Iterator<Item = {}>", self.item)
    }
}

impl std::cmp::PartialEq<DSLType> for RuntimePrimType {
    fn eq(&self, other: &DSLType) -> bool {
        match other {
            DSLType::Prim(other) => other == self,
            _ => false,
        }
    }
}

impl std::cmp::PartialEq<RuntimePrimType> for DSLType {
    fn eq(&self, other: &RuntimePrimType) -> bool {
        other == self
    }
}

impl std::cmp::PartialEq for RustType {
    fn eq(&self, other: &Self) -> bool {
        self.type_id == other.type_id
    }
}
impl std::cmp::Eq for RustType {}
impl std::hash::Hash for RustType {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.type_id.hash(state);
    }
}

impl From<RuntimeType> for DSLType {
    fn from(ty: RuntimeType) -> Self {
        match ty {
            RuntimeType::Unknown => DSLType::Unknown,
            RuntimeType::Prim(ty) => DSLType::Prim(ty),
            RuntimeType::DotNet(ty) => DSLType::DotNet(ty),
        }
    }
}

pub(crate) trait RuntimePrimTypeExt {
    fn new_vector(&self) -> ExposedNativeObject;
    fn vector_type(&self) -> DSLType;
    fn collect_into_vector(
        &self,
        vec: &mut StackValue,
        item: &mut Option<StackValue>,
        output_name: &str,
    ) -> Result<(), Error>;
}
impl RuntimePrimTypeExt for RuntimePrimType {
    fn new_vector(&self) -> ExposedNativeObject {
        match self {
            RuntimePrimType::Bool => {
                ExposedNativeObject::new(Vec::<bool>::new())
            }
            RuntimePrimType::Char => {
                ExposedNativeObject::new(Vec::<char>::new())
            }
            RuntimePrimType::U8 => ExposedNativeObject::new(Vec::<u8>::new()),
            RuntimePrimType::U16 => ExposedNativeObject::new(Vec::<u16>::new()),
            RuntimePrimType::U32 => ExposedNativeObject::new(Vec::<u32>::new()),
            RuntimePrimType::U64 => ExposedNativeObject::new(Vec::<u64>::new()),
            RuntimePrimType::NativeUInt => {
                ExposedNativeObject::new(Vec::<usize>::new())
            }
            RuntimePrimType::I8 => ExposedNativeObject::new(Vec::<i8>::new()),
            RuntimePrimType::I16 => ExposedNativeObject::new(Vec::<i16>::new()),
            RuntimePrimType::I32 => ExposedNativeObject::new(Vec::<i32>::new()),
            RuntimePrimType::I64 => ExposedNativeObject::new(Vec::<i64>::new()),
            RuntimePrimType::NativeInt => {
                ExposedNativeObject::new(Vec::<isize>::new())
            }
            RuntimePrimType::F32 => ExposedNativeObject::new(Vec::<f32>::new()),
            RuntimePrimType::F64 => ExposedNativeObject::new(Vec::<f64>::new()),
            RuntimePrimType::Ptr => {
                ExposedNativeObject::new(Vec::<Pointer>::new())
            }
        }
    }

    fn vector_type(&self) -> DSLType {
        match self {
            RuntimePrimType::Bool => RustType::new::<Vec<bool>>().into(),
            RuntimePrimType::Char => RustType::new::<Vec<char>>().into(),
            RuntimePrimType::U8 => RustType::new::<Vec<u8>>().into(),
            RuntimePrimType::U16 => RustType::new::<Vec<u16>>().into(),
            RuntimePrimType::U32 => RustType::new::<Vec<u32>>().into(),
            RuntimePrimType::U64 => RustType::new::<Vec<u64>>().into(),
            RuntimePrimType::NativeUInt => RustType::new::<Vec<usize>>().into(),
            RuntimePrimType::I8 => RustType::new::<Vec<i8>>().into(),
            RuntimePrimType::I16 => RustType::new::<Vec<i16>>().into(),
            RuntimePrimType::I32 => RustType::new::<Vec<i32>>().into(),
            RuntimePrimType::I64 => RustType::new::<Vec<i64>>().into(),
            RuntimePrimType::NativeInt => RustType::new::<Vec<isize>>().into(),
            RuntimePrimType::F32 => RustType::new::<Vec<f32>>().into(),
            RuntimePrimType::F64 => RustType::new::<Vec<f64>>().into(),
            RuntimePrimType::Ptr => RustType::new::<Vec<Pointer>>().into(),
        }
    }

    fn collect_into_vector(
        &self,
        vec: &mut StackValue,
        item: &mut Option<StackValue>,
        output_name: &str,
    ) -> Result<(), Error> {
        let native = match vec {
            StackValue::Native(native) => Ok(native),
            other => Err(TypeInferenceError::InvalidVectorType(
                other.runtime_type().into(),
            )),
        }?;

        let item = item.as_ref().ok_or_else(|| {
            VMExecutionError::MissingElementTypeInVectorAccumulation {
                name: output_name.to_string(),
            }
        })?;

        let item = item.as_prim().ok_or_else(|| {
            VMExecutionError::IncorrectVectorElementType {
                element_type: (*self).into(),
                item_type: item.runtime_type(),
            }
        })?;

        macro_rules! handle_prim {
            ($variant:ident,$prim:ty) => {{
                let RuntimePrimValue::$variant(item) = item else {
                    return Err(VMExecutionError::IncorrectVectorElementType {
                        element_type: RuntimePrimType::$variant.into(),
                        item_type: item.runtime_type().into(),
                    }
                    .into());
                };

                if let Some(vec) = native.downcast_mut::<Vec<$prim>>() {
                    vec.push(item);
                } else {
                    return Err(VMExecutionError::IncorrectVectorType {
                        expected: RustType::new::<Vec<$prim>>().into(),
                        actual: native.runtime_type(),
                    }
                    .into());
                }
            }};
        }

        match self {
            RuntimePrimType::Bool => handle_prim!(Bool, bool),
            RuntimePrimType::Char => handle_prim!(Char, u16),
            RuntimePrimType::U8 => handle_prim!(U8, u8),
            RuntimePrimType::U16 => handle_prim!(U16, u16),
            RuntimePrimType::U32 => handle_prim!(U32, u32),
            RuntimePrimType::U64 => handle_prim!(U64, u64),
            RuntimePrimType::NativeUInt => handle_prim!(NativeUInt, usize),
            RuntimePrimType::I8 => handle_prim!(I8, i8),
            RuntimePrimType::I16 => handle_prim!(I16, i16),
            RuntimePrimType::I32 => handle_prim!(I32, i32),
            RuntimePrimType::I64 => handle_prim!(I64, i64),
            RuntimePrimType::NativeInt => handle_prim!(NativeInt, isize),
            RuntimePrimType::F32 => handle_prim!(F32, f32),
            RuntimePrimType::F64 => handle_prim!(F64, f64),
            RuntimePrimType::Ptr => handle_prim!(Ptr, Pointer),
        }
        Ok(())
    }
}

pub(crate) trait RuntimePrimValueExt {
    fn static_runtime_type_ref(&self) -> &'static DSLType;
}
impl RuntimePrimValueExt for RuntimePrimValue {
    fn static_runtime_type_ref(&self) -> &'static DSLType {
        match self {
            RuntimePrimValue::Bool(_) => &DSLType::Prim(RuntimePrimType::Bool),
            RuntimePrimValue::Char(_) => &DSLType::Prim(RuntimePrimType::Char),
            RuntimePrimValue::U8(_) => &DSLType::Prim(RuntimePrimType::U8),
            RuntimePrimValue::U16(_) => &DSLType::Prim(RuntimePrimType::U16),
            RuntimePrimValue::U32(_) => &DSLType::Prim(RuntimePrimType::U32),
            RuntimePrimValue::U64(_) => &DSLType::Prim(RuntimePrimType::U64),
            RuntimePrimValue::NativeUInt(_) => {
                &DSLType::Prim(RuntimePrimType::NativeUInt)
            }
            RuntimePrimValue::I8(_) => &DSLType::Prim(RuntimePrimType::I8),
            RuntimePrimValue::I16(_) => &DSLType::Prim(RuntimePrimType::I16),
            RuntimePrimValue::I32(_) => &DSLType::Prim(RuntimePrimType::I32),
            RuntimePrimValue::I64(_) => &DSLType::Prim(RuntimePrimType::I64),
            RuntimePrimValue::NativeInt(_) => {
                &DSLType::Prim(RuntimePrimType::NativeInt)
            }
            RuntimePrimValue::F32(_) => &DSLType::Prim(RuntimePrimType::F32),
            RuntimePrimValue::F64(_) => &DSLType::Prim(RuntimePrimType::F64),
            RuntimePrimValue::Ptr(_) => &DSLType::Prim(RuntimePrimType::Ptr),
        }
    }
}
