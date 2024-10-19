use iterator_extensions::ResultIteratorExt as _;
use itertools::Itertools as _;
use memory_reader::Pointer;

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
    pub generics: Vec<SymbolicType>,
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
                SymbolicOperation::Downcast(symbolic_type) => {
                    let static_method_table_ptr = match &item_type {
                        RuntimeType::Class { method_table } => method_table
                            .ok_or(Error::DowncastRequiresKnownBaseClass)?,
                        _ => {
                            return Err(Error::DowncastRequiresClassInstance(
                                item_type,
                            ));
                        }
                    };
                    let target_method_table_ptr =
                        symbolic_type.method_table(reader)?;

                    if reader.is_base_of(
                        target_method_table_ptr,
                        static_method_table_ptr,
                    )? {
                        // Static type is the same, or superclass of
                        // the desired runtime type.  No runtime check
                        // needed.
                    } else if reader.is_base_of(
                        static_method_table_ptr,
                        target_method_table_ptr,
                    )? {
                        // Target type is a subclass of the
                        // statically-known type.  Emit a runtime
                        // check.
                        ops.push(PhysicalAccessOperation::Downcast(
                            target_method_table_ptr,
                        ));
                    } else {
                        // Types are in separate hierachies, cannot
                        // perform downcast.
                        return Err(Error::DowncastRequiresRelatedClasses(
                            format!("{item_type}"),
                            format!("{symbolic_type}"),
                        ));
                    }

                    item_type = RuntimeType::Class {
                        method_table: Some(target_method_table_ptr),
                    };
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
        reader: CachedReader<'_>,
    ) -> Result<RuntimePrimValue, Error> {
        let mut ptr = self.base_ptr;
        for op in &self.ops {
            ptr = match op {
                PhysicalAccessOperation::Dereference => {
                    reader.read_byte_array(ptr)?.into()
                }
                PhysicalAccessOperation::Offset(offset) => ptr + *offset,
                PhysicalAccessOperation::Downcast(target_type_ptr) => {
                    // TODO: Avoid having the double read of the
                    // object's location.
                    let object_loc: Pointer =
                        reader.read_byte_array(ptr)?.into();
                    let actual_type_ptr: Pointer =
                        reader.read_byte_array(object_loc)?.into();
                    if reader
                        .is_base_of(*target_type_ptr, actual_type_ptr.into())?
                    {
                        ptr
                    } else {
                        return Err(Error::DowncastFailed);
                    }
                }
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
        let method_table_ptr = reader
            .method_table_by_name(
                self.namespace.as_ref().map(|s| s.as_str()).unwrap_or(""),
                &self.name,
            )?
            .ok_or_else(|| {
                Error::UnexpectedNullMethodTable(format!("{}", self))
            })?;

        if self.generics.is_empty() {
            return Ok(method_table_ptr);
        }

        let method_table = reader.method_table(method_table_ptr)?;

        let expected_type_def = method_table.token();

        // TODO: Move this to a cache inside the StaticValueCache.
        let generics = self
            .generics
            .iter()
            .map(|generic| generic.method_table(reader))
            .collect::<Result<Vec<_>, _>>()?;

        let instantiated_generic = [method_table_ptr, generics[0]]
            .into_iter()
            .map(|method_table_ptr| reader.method_table(method_table_ptr))
            .map_ok(|method_table| method_table.module())
            .and_map_ok(|module_ptr| reader.runtime_module(module_ptr))
            .and_map_ok(|module| module.loaded_types(reader))
            .flatten_ok()
            .and_flat_map_ok(|instantiated_generics| {
                instantiated_generics.iter_method_tables(&reader)
            })
            .filter_map_ok(|type_handle_ptr| type_handle_ptr.as_method_table())
            .and_map_ok(|method_table_ptr| {
                reader.method_table(method_table_ptr)
            })
            .filter_ok(|method_table| method_table.token() == expected_type_def)
            .filter_ok(|method_table| method_table.has_generics())
            .and_filter_ok(|candidate_method_table| {
                let mut iter_candidate_generics = candidate_method_table
                    .generic_types_excluding_base_class(&reader)?;
                for generic in generics.iter().cloned() {
                    let Some(candidate_generic) =
                        iter_candidate_generics.next()
                    else {
                        return Ok(false);
                    };

                    let Some(candidate_generic) =
                        candidate_generic.as_method_table()
                    else {
                        return Ok(false);
                    };

                    if candidate_generic != generic {
                        return Ok(false);
                    }
                }

                if iter_candidate_generics.next().is_some() {
                    return Ok(false);
                }

                Ok(true)
            })
            .next()
            .ok_or_else(|| {
                Error::NoSuchMethodTableFound(format!("{self}"))
            })??;

        Ok(instantiated_generic.ptr())
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
            write!(f, "{namespace}.")?;
        }
        write!(f, "{}", self.name)?;

        if !self.generics.is_empty() {
            write!(f, "<")?;
            for (i, generic) in self.generics.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
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
        write!(f, "{}.{}", self.class, self.field_name)
    }
}

impl std::fmt::Display for SymbolicOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolicOperation::Field(field) => write!(f, ".{field}"),
            SymbolicOperation::IndexAccess(index) => write!(f, "[{index}]"),
            SymbolicOperation::Downcast(symbolic_type) => {
                write!(f, ".as<{symbolic_type}>()")
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
