use itertools::Itertools as _;
use memory_reader::Pointer;

use crate::{
    runtime_type::RuntimePrimType, runtime_value::RuntimePrimValue,
    CachedReader, Error, FieldContainer, FieldDescription, MethodTable,
    RuntimeArray, RuntimeType, SymbolicParser, SymbolicType, TypedPointer,
};

#[derive(Clone)]
pub struct SymbolicAccessChain {
    pub static_field: SymbolicStaticField,
    pub ops: Vec<SymbolicOperation>,
}

#[derive(Clone)]
pub struct SymbolicStaticField {
    pub class: SymbolicType,
    pub field_name: String,
}

struct SymbolicAccessChainView<'a> {
    static_field: &'a SymbolicStaticField,
    ops: &'a [SymbolicOperation],
}

#[derive(Clone)]
pub enum SymbolicOperation {
    Field(String),
    IndexAccess(usize),
    Downcast(SymbolicType),
}

pub struct PhysicalAccessChain {
    base_ptr: Pointer,
    ops: Vec<PhysicalAccessOperation>,
    prim_type: RuntimePrimType,
}

pub enum PhysicalAccessOperation {
    Dereference,
    Offset(usize),
    Downcast(TypedPointer<MethodTable>),
}

trait RuntimeTypeExt {
    fn method_table_for_field_access(
        &self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<TypedPointer<MethodTable>, Error>;

    fn method_table_for_downcast(
        &self,
    ) -> Result<TypedPointer<MethodTable>, Error>;

    fn as_array_type(
        &self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<&RuntimeType, Error>;
}
impl RuntimeTypeExt for RuntimeType {
    fn method_table_for_field_access(
        &self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        match self {
            RuntimeType::ValueType { method_table, .. }
            | RuntimeType::Class { method_table } => method_table
                .ok_or_else(|| Error::UnexpectedNullMethodTable(gen_name())),
            _ => Err(Error::FieldAccessRequiresClassOrStruct(self.clone())),
        }
    }

    fn method_table_for_downcast(
        &self,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        match self {
            RuntimeType::Class { method_table } => {
                method_table.ok_or(Error::DowncastRequiresKnownBaseClass)
            }
            _ => Err(Error::DowncastRequiresClassInstance(self.clone())),
        }
    }

    fn as_array_type(
        &self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<&RuntimeType, Error> {
        match self {
            RuntimeType::Array { element_type } => Ok(element_type.as_ref()),
            //RuntimeType::MultiDimArray { element_type, rank } => todo!(),
            _ => Err(Error::IndexAccessRequiresArray(self.clone())),
        }?
        .ok_or_else(|| Error::UnexpectedNullMethodTable(gen_name()))
        .map(|b| b.as_ref())
    }
}

trait CachedReaderExt<'a> {
    fn find_field(
        &self,
        method_table_ptr: TypedPointer<MethodTable>,
        field_name: &str,
    ) -> Result<(TypedPointer<MethodTable>, FieldDescription<'a>), Error>;
}
impl<'a> CachedReaderExt<'a> for CachedReader<'a> {
    fn find_field(
        &self,
        method_table_ptr: TypedPointer<MethodTable>,
        field_name: &str,
    ) -> Result<(TypedPointer<MethodTable>, FieldDescription<'a>), Error> {
        self.iter_instance_fields(method_table_ptr)?
            .find(|(_, field)| {
                self.field_to_name(field)
                    .map(|name| name == field_name)
                    .unwrap_or(false)
            })
            .ok_or_else(|| match self.method_table_to_name(method_table_ptr) {
                Ok(class_name) => Error::NoSuchInstanceField {
                    class_name: class_name.to_string(),
                    field_name: field_name.to_string(),
                },
                Err(err) => err,
            })
    }
}

impl SymbolicAccessChain {
    pub fn parse(chain: &str, reader: CachedReader<'_>) -> Result<Self, Error> {
        let chain = SymbolicParser::new(chain, reader).next_chain()?;
        Ok(chain)
    }

    fn prefix(&self, index: usize) -> SymbolicAccessChainView {
        SymbolicAccessChainView {
            static_field: &self.static_field,
            ops: &self.ops[..index],
        }
    }

    pub fn simplify(mut self, reader: CachedReader<'_>) -> Result<Self, Error> {
        let (_, mut item_type) =
            self.static_field.start_of_access_chain(reader)?;
        let old_ops = {
            let mut vec = Vec::new();
            std::mem::swap(&mut vec, &mut self.ops);
            vec
        };

        for (op_index, op) in old_ops.into_iter().enumerate() {
            match &op {
                SymbolicOperation::Field(field_name) => {
                    let method_table_ptr = item_type
                        .method_table_for_field_access(|| format!("{self}"))?;
                    let (parent_of_field, field) =
                        reader.find_field(method_table_ptr, field_name)?;
                    let new_item_type = reader
                        .field_to_runtime_type(parent_of_field, &field)?;

                    self.ops.push(op);
                    item_type = new_item_type;
                }
                SymbolicOperation::IndexAccess(_) => {
                    let element_type = item_type.as_array_type(|| {
                        format!("{}", self.prefix(op_index))
                    })?;

                    self.ops.push(op);
                    item_type = element_type.clone();
                }
                SymbolicOperation::Downcast(symbolic_type) => {
                    let static_method_table_ptr =
                        item_type.method_table_for_downcast()?;
                    let target_method_table_ptr =
                        symbolic_type.method_table(reader)?;

                    if reader.is_base_of(
                        target_method_table_ptr,
                        static_method_table_ptr,
                    )? {
                        // Static type is the same, or superclass of
                        // the desired runtime type.  This downcast
                        // can be simplified away.
                        item_type = RuntimeType::Class {
                            method_table: Some(static_method_table_ptr),
                        };
                    } else if reader.is_base_of(
                        static_method_table_ptr,
                        target_method_table_ptr,
                    )? {
                        // Target type is a subclass of the
                        // statically-known type.  The downcast must
                        // be retained.
                        self.ops.push(op);
                        item_type = RuntimeType::Class {
                            method_table: Some(target_method_table_ptr),
                        };
                    } else {
                        // Types are in separate hierachies.  This
                        // downcast is illegal.
                        return Err(Error::DowncastRequiresRelatedClasses(
                            format!("{item_type}"),
                            format!("{symbolic_type}"),
                        ));
                    }
                }
            }
        }

        Ok(self)
    }

    pub fn to_physical(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<PhysicalAccessChain, Error> {
        let (base_ptr, mut item_type) =
            self.static_field.start_of_access_chain(reader)?;

        let mut ops = Vec::new();

        if item_type.stored_as_ptr() {
            ops.push(PhysicalAccessOperation::Dereference);
        }

        for (op_index, op) in self.ops.iter().enumerate() {
            match &item_type {
                RuntimeType::ValueType {
                    method_table: Some(method_table_ptr),
                    ..
                }
                | RuntimeType::Class {
                    method_table: Some(method_table_ptr),
                } => {
                    let method_table_ptr = *method_table_ptr;
                    let method_table = reader.method_table(method_table_ptr)?;
                    assert!(
                        !method_table
                            .has_non_instantiated_generic_types(reader)?,
                        "Method table for {} has non-instantiated generics.",
                        reader.method_table_to_name(method_table_ptr)?
                    );
                }
                _ => {}
            }

            match op {
                SymbolicOperation::Field(field_name) => {
                    let method_table_ptr = item_type
                        .method_table_for_field_access(|| {
                            format!("{}", self.prefix(op_index))
                        })?;

                    let (parent_of_field, field) =
                        reader.find_field(method_table_ptr, field_name)?;

                    if matches!(&item_type, RuntimeType::Class { .. }) {
                        ops.push(PhysicalAccessOperation::Offset(
                            Pointer::SIZE,
                        ));
                    }
                    ops.push(PhysicalAccessOperation::Offset(field.offset()));
                    let new_item_type = reader
                        .field_to_runtime_type(parent_of_field, &field)?;

                    if let RuntimeType::Class { method_table: None } =
                        &new_item_type
                    {
                        panic!(
                            "Field should have known method table, \
                                but {field_name} did not\n\
                                \tPath to field: {}\n\
                                \tContained in: {}\n\
                                \tField's parent type: {}\n\
                                \tField signature: {}",
                            self.prefix(op_index + 1),
                            reader.method_table_to_name(method_table_ptr)?,
                            reader.method_table_to_name(parent_of_field)?,
                            reader.field_to_type_name(&field)?,
                        )
                    }

                    if new_item_type.stored_as_ptr() {
                        ops.push(PhysicalAccessOperation::Dereference);
                    }

                    item_type = new_item_type;
                }
                SymbolicOperation::IndexAccess(index) => {
                    let element_type = item_type.as_array_type(|| {
                        format!("{}", self.prefix(op_index))
                    })?;

                    let stride = element_type.size_bytes();

                    ops.push(PhysicalAccessOperation::Offset(
                        RuntimeArray::HEADER_SIZE,
                    ));
                    ops.push(PhysicalAccessOperation::Offset(stride * index));
                    if element_type.stored_as_ptr() {
                        ops.push(PhysicalAccessOperation::Dereference);
                    }
                    item_type = element_type.clone();
                }
                SymbolicOperation::Downcast(symbolic_type) => {
                    let target_method_table_ptr =
                        symbolic_type.method_table(reader)?;

                    ops.push(PhysicalAccessOperation::Downcast(
                        target_method_table_ptr,
                    ));

                    item_type = RuntimeType::Class {
                        method_table: Some(target_method_table_ptr),
                    };
                }
            }
        }

        let prim_type = match item_type {
            RuntimeType::Prim(runtime_prim_type) => Ok(runtime_prim_type),
            other => Err(Error::SymbolicExpressionMustProducePrimitive {
                field: format!("{self}"),
                ty: other,
            }),
        }?;

        Ok(PhysicalAccessChain {
            base_ptr,
            ops,
            prim_type,
        })
    }
}

impl SymbolicStaticField {
    fn start_of_access_chain(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<(Pointer, RuntimeType), Error> {
        let base_method_table_ptr = self.class.method_table(reader)?;

        let field = reader
            .iter_static_fields(base_method_table_ptr)?
            .find(|field| {
                reader
                    .field_to_name(field)
                    .map(|name| name == self.field_name)
                    .unwrap_or(false)
            })
            .ok_or_else(|| Error::NoSuchStaticField {
                class: format!("{}", self.class),
                field: self.field_name.clone(),
            })?;

        let base_ptr = {
            let method_table = reader.method_table(base_method_table_ptr)?;
            let module = reader.runtime_module(method_table.module())?;
            field.location(module, FieldContainer::Static, &reader)?
        };
        let base_type =
            reader.field_to_runtime_type(base_method_table_ptr, &field)?;

        Ok((base_ptr, base_type))
    }
}

impl PhysicalAccessChain {
    pub fn simplify(self) -> Self {
        let ops = self
            .ops
            .into_iter()
            .peekable()
            .batching(|iter| {
                let mut op = iter.next()?;
                if let PhysicalAccessOperation::Offset(offset) = &mut op {
                    while let Some(next) = iter.next_if(|next| {
                        matches!(next, PhysicalAccessOperation::Offset(_))
                    }) {
                        let PhysicalAccessOperation::Offset(next_offset) = next
                        else {
                            unreachable!("Protected by Peekable.next_if")
                        };
                        *offset += next_offset;
                    }
                }
                Some(op)
            })
            .collect();
        Self { ops, ..self }
    }

    pub fn read(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<Option<RuntimePrimValue>, Error> {
        let mut ptr = self.base_ptr;
        for op in &self.ops {
            match op {
                PhysicalAccessOperation::Dereference => {
                    ptr = reader.read_byte_array(ptr)?.into();
                }
                PhysicalAccessOperation::Offset(offset) => {
                    ptr = ptr + *offset;
                }
                PhysicalAccessOperation::Downcast(target_type_ptr) => {
                    let actual_type_ptr: Pointer =
                        reader.read_byte_array(ptr)?.into();
                    if !reader
                        .is_base_of(*target_type_ptr, actual_type_ptr.into())?
                    {
                        return Ok(None);
                    }
                }
            };
        }

        let bytes =
            reader.read_bytes(ptr..ptr + self.prim_type.size_bytes())?;
        let prim_value = self.prim_type.parse(&bytes)?;
        Ok(Some(prim_value))
    }

    pub fn read_as<T>(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<Option<T>, Error>
    where
        RuntimePrimValue: TryInto<T>,
        Error: From<<RuntimePrimValue as TryInto<T>>::Error>,
    {
        let value = self
            .read(reader)?
            .map(|value| value.try_into())
            .transpose()?;
        Ok(value)
    }
}

impl std::fmt::Display for SymbolicAccessChain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.prefix(self.ops.len()))
    }
}

impl<'a> std::fmt::Display for SymbolicAccessChainView<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.static_field)?;

        self.ops.iter().try_for_each(|op| write!(f, "{op}"))?;
        Ok(())
    }
}

impl std::fmt::Display for SymbolicType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(namespace) = &self.namespace {
            write!(f, "{namespace}.")?;
        }
        write!(f, "{}", self.name)?;

        if !self.generics.is_empty() {
            write!(f, "<\u{200B}")?;
            for (i, generic) in self.generics.iter().enumerate() {
                if i > 0 {
                    write!(f, ",\u{200B} ")?;
                }
                write!(f, "{generic}")?;
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}

impl std::fmt::Display for SymbolicStaticField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\u{200B}.{}", self.class, self.field_name)
    }
}

impl std::fmt::Display for SymbolicOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolicOperation::Field(field) => write!(f, "\u{200B}.{field}"),
            SymbolicOperation::IndexAccess(index) => write!(f, "[{index}]"),
            SymbolicOperation::Downcast(symbolic_type) => {
                write!(f, "\u{200B}.as<{symbolic_type}>()")
            }
        }
    }
}

impl std::fmt::Display for PhysicalAccessChain {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.base_ptr)?;

        self.ops.iter().try_for_each(|op| write!(f, "{op}"))?;

        write!(f, ".as::<{}>()", self.prim_type)?;

        Ok(())
    }
}

impl std::fmt::Display for PhysicalAccessOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PhysicalAccessOperation::Dereference => write!(f, ".deref()"),
            PhysicalAccessOperation::Offset(offset) => {
                write!(f, ".offset({offset})")
            }
            PhysicalAccessOperation::Downcast(typed_pointer) => {
                write!(f, ".downcast({typed_pointer})")
            }
        }
    }
}
