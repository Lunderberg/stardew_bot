use std::borrow::Borrow;

use memory_reader::{MemoryReader, OwnedBytes, Pointer};

use crate::{
    runtime_type::{DotNetType, FunctionType, IteratorType, TupleType},
    unpack_fields, CachedReader, CorElementType, Error, MethodTable,
    ReadTypedPointer, RuntimeType, TypedPointer,
};

pub struct TypeDescription {
    pub bytes: OwnedBytes,
}

pub enum TypeHandle {
    MethodTable(MethodTable),
    TypeDescription(TypeDescription),
}

pub struct PrintableTypeHandle<'a> {
    ty: TypeHandleRef<'a>,
    reader: CachedReader<'a>,
}

enum TypeHandleRef<'a> {
    MethodTable(&'a MethodTable),
    TypeDescription(&'a TypeDescription),
}

impl TypeDescription {
    // May be as small as 24 bytes for the `FnPtrTypeDesc`, but we
    // might as well read enough to unpack the `TypeVarTypeDesc`,
    // which has 48 bytes.
    pub const SIZE: usize = 48;

    unpack_fields! {
        element_type: {CorElementType, 0..1},
        raw_index: {u32, 44..48},
    }

    pub fn runtime_type(
        &self,
        _reader: &MemoryReader,
    ) -> Result<RuntimeType, Error> {
        match self.element_type() {
            CorElementType::Prim(prim) => Ok(prim.into()),
            CorElementType::End => todo!(),
            CorElementType::Void => todo!(),

            CorElementType::String => todo!(),
            CorElementType::ByRef => todo!(),
            CorElementType::ValueType => todo!(),
            CorElementType::Class => todo!(),
            CorElementType::Var => todo!(),
            CorElementType::Array => todo!(),
            CorElementType::GenericInst => todo!(),
            CorElementType::TypedByRef => todo!(),
            CorElementType::FunctionPtr => todo!(),
            CorElementType::Object => todo!(),
            CorElementType::SizeArray => todo!(),
            CorElementType::MethodType => todo!(),
            CorElementType::RequiredCModifier => todo!(),
            CorElementType::OptionalCModifier => todo!(),
            CorElementType::Internal => todo!(),
            CorElementType::Modifier => todo!(),
            CorElementType::Sentinel => todo!(),
            CorElementType::Pinned => todo!(),
        }
    }

    pub fn index(&self) -> Option<usize> {
        match self.element_type() {
            CorElementType::Var => Some(self.raw_index() as usize),
            _ => None,
        }
    }

    fn type_param(&self) -> Option<TypedPointer<TypeHandle>> {
        matches!(self.element_type(), CorElementType::ByRef).then(|| {
            let arg_ptr: TypedPointer<TypeHandle> =
                self.bytes.subrange(16..24).unpack().unwrap();
            arg_ptr
        })
    }

    fn value_type_method_table(&self) -> Option<TypedPointer<MethodTable>> {
        matches!(self.element_type(), CorElementType::ValueType).then(|| {
            let arg_ptr: TypedPointer<MethodTable> =
                self.bytes.subrange(16..24).unpack().unwrap();
            arg_ptr
        })
    }
}

impl TypeHandle {
    pub fn runtime_type(
        &self,
        reader: &MemoryReader,
    ) -> Result<RuntimeType, Error> {
        match self {
            TypeHandle::MethodTable(method_table) => {
                method_table.runtime_type(reader)
            }
            TypeHandle::TypeDescription(type_desc) => {
                type_desc.runtime_type(reader)
            }
        }
    }

    pub fn printable<'a>(
        &'a self,
        reader: CachedReader<'a>,
    ) -> PrintableTypeHandle<'a> {
        PrintableTypeHandle {
            ty: self.into(),
            reader,
        }
    }
}

impl<'a> TypeHandleRef<'a> {
    fn print(
        &self,
        fmt: &mut std::fmt::Formatter<'_>,
        reader: CachedReader,
    ) -> Result<(), Error> {
        match self {
            Self::MethodTable(method_table) => {
                Self::print_method_table(method_table, fmt, reader)?;
            }
            Self::TypeDescription(type_desc) => {
                match type_desc.element_type() {
                    CorElementType::Prim(prim) => {
                        write!(fmt, "{prim}")?;
                    }
                    CorElementType::Var => {
                        let type_index = type_desc.index().unwrap();
                        write!(fmt, "_T{type_index}")?;
                    }
                    CorElementType::ByRef => {
                        let arg_ptr = type_desc.type_param().unwrap();
                        let arg = arg_ptr.read(reader.borrow())?;
                        write!(fmt, "&{}", arg.printable(reader))?;
                    }

                    CorElementType::FunctionPtr => {
                        // A dynamic-sized FnPtrTypeDesc
                        write!(fmt, "Function pointer")?;
                    }

                    CorElementType::MethodType => {
                        write!(fmt, "Method")?;
                    }

                    CorElementType::ValueType => {
                        let method_table_ptr =
                            type_desc.value_type_method_table().unwrap();
                        let method_table =
                            reader.method_table(method_table_ptr)?;
                        Self::print_method_table(method_table, fmt, reader)?
                    }

                    other => todo!("Pretty-printing for {other}"),
                }
            }
        }
        Ok(())
    }

    fn print_method_table(
        method_table: &MethodTable,
        fmt: &mut std::fmt::Formatter<'_>,
        reader: CachedReader,
    ) -> Result<(), Error> {
        let runtime_type = method_table.runtime_type(reader)?;
        match runtime_type {
            RuntimeType::DotNet(
                DotNetType::Class { .. } | DotNetType::ValueType { .. },
            ) => {
                let row = reader
                    .runtime_module(method_table.module())?
                    .metadata(&reader)?
                    .get(method_table.token().expect(
                        "Class/ValueType should have metadata token",
                    ))?;
                let name = row.name()?;
                let namespace = row.namespace()?;
                write!(fmt, "{namespace}.{name}")?;

                if method_table.has_generics() {
                    write!(fmt, "<")?;
                    method_table
                        .generic_types_excluding_base_class(reader.borrow())?
                        .enumerate()
                        .try_for_each(
                            |(i, type_handle_ptr)| -> Result<_, Error> {
                                if i > 0 {
                                    write!(fmt, ", ")?;
                                }
                                let type_handle =
                                    reader.type_handle(type_handle_ptr)?;
                                write!(
                                    fmt,
                                    "{}",
                                    type_handle.printable(reader)
                                )?;
                                Ok(())
                            },
                        )?;
                    write!(fmt, ">")?;
                }
            }

            other => {
                Self::print_runtime_type(&other, fmt, reader)?;
            }
        }
        Ok(())
    }

    fn print_runtime_type(
        runtime_type: &RuntimeType,
        fmt: &mut std::fmt::Formatter<'_>,
        reader: CachedReader,
    ) -> Result<(), Error> {
        let write_tuple = |fmt: &mut std::fmt::Formatter,
                           tuple: &[RuntimeType]|
         -> Result<(), Error> {
            write!(fmt, "(")?;
            tuple.iter().enumerate().try_for_each(|(i, element)| {
                if i > 0 {
                    write!(fmt, ", ")?;
                }
                Self::print_runtime_type(element, fmt, reader)
            })?;
            write!(fmt, ")")?;
            Ok(())
        };

        match runtime_type {
            RuntimeType::Unknown => {
                write!(fmt, "(???)")?;
            }
            RuntimeType::Prim(prim) => {
                write!(fmt, "{prim}")?;
            }
            RuntimeType::DotNet(DotNetType::ValueType {
                method_table: None,
                ..
            }) => {
                write!(fmt, "(delayed-load-struct)")?;
            }
            RuntimeType::DotNet(DotNetType::Class { method_table: None }) => {
                write!(fmt, "(delayed-load-class)")?;
            }
            RuntimeType::DotNet(
                DotNetType::ValueType {
                    method_table: Some(ptr),
                    ..
                }
                | DotNetType::Class {
                    method_table: Some(ptr),
                },
            ) => {
                let method_table = reader.method_table(*ptr)?;
                Self::print_method_table(method_table, fmt, reader)?;
            }
            RuntimeType::DotNet(DotNetType::String) => {
                write!(fmt, "String")?;
            }
            RuntimeType::DotNet(DotNetType::Array { method_table, .. }) => {
                write!(fmt, "Array<")?;
                if let Some(ptr) = method_table {
                    let method_table = reader.method_table(*ptr)?;
                    let element_type = method_table
                        .array_element_type()
                        .ok_or(Error::ArrayMissingElementType)?;
                    let runtime_type = reader.runtime_type(element_type)?;
                    Self::print_runtime_type(&runtime_type, fmt, reader)?;
                }
                write!(fmt, ">")?;
            }
            RuntimeType::DotNet(DotNetType::MultiDimArray {
                method_table,
                rank,
            }) => {
                write!(fmt, "MultiDimArray<{rank}")?;
                if let Some(ptr) = method_table {
                    write!(fmt, ", ")?;
                    let method_table = reader.method_table(*ptr)?;
                    let element_type = method_table
                        .array_element_type()
                        .ok_or(Error::ArrayMissingElementType)?;
                    let runtime_type = reader.runtime_type(element_type)?;
                    Self::print_runtime_type(&runtime_type, fmt, reader)?;
                }
                write!(fmt, ">")?;
            }
            RuntimeType::Rust(rust_type) => write!(fmt, "{rust_type}")?,
            RuntimeType::Function(FunctionType { params, output }) => {
                write!(fmt, "Fn")?;
                if let Some(params) = params {
                    write_tuple(fmt, params)?;
                } else {
                    write!(fmt, "(...)")?;
                }

                write!(fmt, " -> ")?;
                Self::print_runtime_type(output, fmt, reader)?;
            }
            RuntimeType::Tuple(TupleType(elements)) => {
                write_tuple(fmt, elements)?;
            }
            RuntimeType::Iterator(IteratorType { item }) => {
                write!(fmt, "Iterator<Item = ")?;
                Self::print_runtime_type(item, fmt, reader)?;
                write!(fmt, ">")?;
            }
        }
        Ok(())
    }
}

impl TypedPointer<TypeHandle> {
    pub fn as_method_table(&self) -> Option<TypedPointer<MethodTable>> {
        if self.as_usize() & 2 > 0 {
            None
        } else {
            let ptr: Pointer = self.clone().into();
            let ptr: TypedPointer<MethodTable> = ptr.into();
            Some(ptr)
        }
    }
}

impl ReadTypedPointer for TypeDescription {
    fn read_typed_ptr(
        ptr: memory_reader::Pointer,
        reader: &memory_reader::MemoryReader,
    ) -> Result<Self, Error> {
        let bytes = reader.read_bytes(ptr..ptr + Self::SIZE)?;
        Ok(Self { bytes })
    }
}

impl ReadTypedPointer for TypeHandle {
    fn read_typed_ptr(
        ptr: Pointer,
        reader: &MemoryReader,
    ) -> Result<Self, Error> {
        if ptr.as_usize() & 2 > 0 {
            let ptr: TypedPointer<TypeDescription> = (ptr & (!2)).into();
            let type_desc = ptr.read(reader)?;
            Ok(TypeHandle::TypeDescription(type_desc))
        } else {
            let ptr: TypedPointer<MethodTable> = ptr.into();
            let method_table = ptr.read(reader)?;
            Ok(TypeHandle::MethodTable(method_table))
        }
    }
}

impl std::fmt::Display for PrintableTypeHandle<'_> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ty.print(fmt, self.reader) {
            Ok(_) => Ok(()),
            Err(Error::FmtError { err }) => Err(err),
            Err(err) => {
                write!(fmt, "PrintErr: {err}")
            }
        }
    }
}

impl<'a> From<&'a TypeHandle> for TypeHandleRef<'a> {
    fn from(handle: &'a TypeHandle) -> Self {
        match handle {
            TypeHandle::MethodTable(method_table) => {
                Self::MethodTable(method_table)
            }
            TypeHandle::TypeDescription(type_description) => {
                Self::TypeDescription(type_description)
            }
        }
    }
}
