use memory_reader::{MemoryReader, Pointer};

use crate::{
    runtime_type::RuntimePrimType, runtime_value::RuntimePrimValue,
    CachedReader, Error, FieldContainer, MethodTable, RuntimeArray,
    RuntimeType, TypedPointer,
};

#[derive(Clone)]
pub struct SymbolicAccessChain {
    pub static_field: SymbolicStaticField,
    pub ops: Vec<SymbolicOperation>,
}

#[derive(Clone)]
pub struct SymbolicType {
    pub namespace: Option<String>,
    pub name: String,
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
    //Downcast { namespace: String, name: String },
}

pub struct PhysicalAccessChain {
    base_ptr: Pointer,
    ops: Vec<PhysicalAccessOperation>,
    prim_type: RuntimePrimType,
}

pub enum PhysicalAccessOperation {
    Dereference,
    Offset(usize),
    //AssertContainsMethodTable(Pointer),
}

impl PhysicalAccessChain {
    pub fn derive(
        symbolic: &SymbolicAccessChain,
        reader: CachedReader<'_>,
    ) -> Result<Self, Error> {
        let (base_ptr, mut item_type) =
            symbolic.static_field.start_of_access_chain(reader)?;

        match &item_type {
            RuntimeType::ValueType {
                method_table: method_table_ptr,
                ..
            }
            | RuntimeType::Class {
                method_table: Some(method_table_ptr),
            } => {
                let method_table_ptr = *method_table_ptr;
                let method_table = reader.method_table(method_table_ptr)?;
                assert!(
                    !method_table.has_non_instantiated_generic_types(reader)?,
                    "Method table for {} has non-instantiated generics.",
                    reader.method_table_to_name(method_table_ptr)?
                );
            }
            _ => {}
        }

        let mut ops = Vec::new();
        for (op_index, op) in symbolic.ops.iter().enumerate() {
            match &item_type {
                RuntimeType::ValueType {
                    method_table: method_table_ptr,
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
                    let method_table_ptr = match &item_type {
                        RuntimeType::ValueType { method_table, .. } => {
                            *method_table
                        }
                        RuntimeType::Class { method_table } => method_table
                            .ok_or_else(|| {
                                Error::UnexpectedNullMethodTable(format!(
                                    "{}",
                                    symbolic.prefix(op_index)
                                ))
                            })?,
                        _ => {
                            return Err(
                                Error::FieldAccessRequiresClassOrStruct(
                                    item_type,
                                ),
                            );
                        }
                    };
                    let (parent_of_field, field) = reader
                        .iter_instance_fields(method_table_ptr)?
                        .find(|(_, field)| {
                            reader
                                .field_to_name(field)
                                .map(|name| name == field_name)
                                .unwrap_or(false)
                        })
                        .ok_or_else(|| {
                            match reader.method_table_to_name(method_table_ptr)
                            {
                                Ok(class_name) => Error::NoSuchInstanceField {
                                    class_name: class_name.to_string(),
                                    field_name: field_name.clone(),
                                },
                                Err(err) => err,
                            }
                        })?;

                    if matches!(&item_type, RuntimeType::Class { .. }) {
                        ops.push(PhysicalAccessOperation::Dereference);
                        ops.push(PhysicalAccessOperation::Offset(
                            Pointer::SIZE,
                        ));
                    }
                    ops.push(PhysicalAccessOperation::Offset(field.offset()));
                    let new_item_type = reader.field_to_runtime_type(
                        //method_table_ptr,
                        parent_of_field,
                        &field,
                    )?;

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
                            symbolic.prefix(op_index + 1),
                            reader.method_table_to_name(method_table_ptr)?,
                            reader.method_table_to_name(parent_of_field)?,
                            reader.field_to_type_name(&field)?,
                        )
                    }

                    item_type = new_item_type;
                }
                SymbolicOperation::IndexAccess(index) => {
                    let element_type = match &item_type {
                        RuntimeType::Array { element_type } => {
                            element_type.as_ref()
                        }
                        //RuntimeType::MultiDimArray { element_type, rank } => todo!(),
                        _ => {
                            return Err(Error::IndexAccessRequiresArray(
                                item_type,
                            ));
                        }
                    }
                    .ok_or_else(|| {
                        Error::UnexpectedNullMethodTable(format!(
                            "{}",
                            symbolic.prefix(op_index)
                        ))
                    })?;

                    let stride = element_type.size_bytes();

                    ops.push(PhysicalAccessOperation::Dereference);
                    ops.push(PhysicalAccessOperation::Offset(
                        RuntimeArray::HEADER_SIZE,
                    ));
                    ops.push(PhysicalAccessOperation::Offset(stride * index));
                    item_type = (**element_type).clone();
                }
            }
        }

        let prim_type = match item_type {
            RuntimeType::Prim(runtime_prim_type) => Ok(runtime_prim_type),
            other => Err(Error::AccessChainMustTerminateInPrimitive {
                field: format!("{symbolic}"),
                ty: other,
            }),
        }?;

        Ok(Self {
            base_ptr,
            ops,
            prim_type,
        })
    }

    pub fn read(
        &self,
        reader: &MemoryReader,
    ) -> Result<RuntimePrimValue, Error> {
        let mut ptr = self.base_ptr;
        for op in &self.ops {
            ptr = match op {
                PhysicalAccessOperation::Dereference => {
                    reader.read_byte_array(ptr)?.into()
                }
                PhysicalAccessOperation::Offset(offset) => ptr + *offset,
            };
        }

        let bytes =
            reader.read_bytes(ptr..ptr + self.prim_type.size_bytes())?;
        self.prim_type.parse(&bytes)
    }
}

impl SymbolicAccessChain {
    fn prefix(&self, index: usize) -> SymbolicAccessChainView {
        SymbolicAccessChainView {
            static_field: &self.static_field,
            ops: &self.ops[..index],
        }
    }
}

impl SymbolicType {
    fn method_table(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        reader
            .method_table_by_name(
                self.namespace.as_ref().map(|s| s.as_str()).unwrap_or(""),
                &self.name,
            )?
            .ok_or_else(|| {
                Error::UnexpectedNullMethodTable(format!("{}", self))
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
            write!(f, "{namespace}.{}", self.name)
        } else {
            write!(f, "{}", self.name)
        }
    }
}

impl std::fmt::Display for SymbolicStaticField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.{}", self.class, self.field_name)
    }
}

impl std::fmt::Display for SymbolicOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolicOperation::Field(field) => write!(f, ".{field}"),
            SymbolicOperation::IndexAccess(index) => write!(f, "[{index}]"),
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
        }
    }
}
