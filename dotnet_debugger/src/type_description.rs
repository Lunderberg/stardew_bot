use std::borrow::Borrow;

use memory_reader::{MemoryReader, OwnedBytes, Pointer};

use crate::{
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
    ty: &'a TypeHandle,
    reader: CachedReader<'a>,
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
            CorElementType::Prim(prim) => Ok(RuntimeType::Prim(prim)),
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

    pub(crate) fn printable<'a>(
        &'a self,
        reader: CachedReader<'a>,
    ) -> PrintableTypeHandle<'a> {
        PrintableTypeHandle { ty: self, reader }
    }

    fn print_method_table(
        method_table: &MethodTable,
        fmt: &mut std::fmt::Formatter<'_>,
        reader: CachedReader,
    ) -> Result<(), Error> {
        match method_table.runtime_type(reader)? {
            RuntimeType::Prim(prim) => {
                write!(fmt, "{prim}")?;
            }
            RuntimeType::Class | RuntimeType::ValueType { .. } => {
                let row = reader
                    .runtime_module(method_table.module())?
                    .metadata(&reader)?
                    .get(method_table.token().expect(
                        "Class/ValueType should have metadata token",
                    ))?;
                let name = row.name()?;
                let namespace = row.namespace()?;
                write!(fmt, "{namespace}.{name}")?;
            }

            RuntimeType::String => {
                write!(fmt, "String")?;
            }
            RuntimeType::Array => {
                write!(fmt, "Array")?;
            }

            RuntimeType::FixedSizeArray { .. } => todo!(),
        }
        Ok(())
    }

    fn print(
        &self,
        fmt: &mut std::fmt::Formatter<'_>,
        reader: CachedReader,
    ) -> Result<(), Error> {
        match self {
            TypeHandle::MethodTable(method_table) => {
                Self::print_method_table(method_table, fmt, reader)?;
            }
            TypeHandle::TypeDescription(type_desc) => {
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

                    other => todo!("Pretty-printing for {other}"),
                }
            }
        }
        Ok(())
    }
}

impl TypedPointer<TypeHandle> {
    pub(crate) fn as_method_table(&self) -> Option<TypedPointer<MethodTable>> {
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
            //let method_table = ptr.read(reader)?;
            let method_table = ptr.read(reader).unwrap();
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
