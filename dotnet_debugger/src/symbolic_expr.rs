use std::fmt::Display;

use derive_more::derive::From;
use itertools::Itertools as _;

use crate::{
    runtime_type::RuntimePrimType, CachedReader, Error, FieldDescription,
    MethodTable, PhysicalExpr, RuntimeArray, RuntimeType, SymbolicParser,
    TypedPointer, VirtualMachine,
};
use iterator_extensions::ResultIteratorExt as _;
use memory_reader::Pointer;

#[derive(Clone, From)]
pub enum SymbolicExpr {
    Int(usize),
    StaticField(StaticField),
    FieldAccess(FieldAccess),
    IndexAccess(IndexAccess),
    Downcast(Downcast),
    Tuple(Tuple),
}

#[derive(Clone)]
pub struct SymbolicType {
    pub namespace: Option<String>,
    pub name: String,
    pub generics: Vec<SymbolicType>,
}

#[derive(Clone)]
pub struct StaticField {
    pub class: SymbolicType,
    pub field_name: String,
}

#[derive(Clone)]
pub struct FieldAccess {
    pub obj: Box<SymbolicExpr>,
    pub field: String,
}

#[derive(Clone)]
pub struct IndexAccess {
    pub obj: Box<SymbolicExpr>,
    pub index: Box<SymbolicExpr>,
}

#[derive(Clone)]
pub struct Downcast {
    pub obj: Box<SymbolicExpr>,
    pub ty: SymbolicType,
}

#[derive(Clone)]
pub struct Tuple {
    pub items: Vec<SymbolicExpr>,
}

impl SymbolicType {
    pub(crate) fn method_table(
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

impl StaticField {
    fn method_table_and_field<'a>(
        &self,
        reader: CachedReader<'a>,
    ) -> Result<(TypedPointer<MethodTable>, FieldDescription<'a>), Error> {
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

        Ok((base_method_table_ptr, field))
    }

    fn runtime_type(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<RuntimeType, Error> {
        let (base_method_table_ptr, field) =
            self.method_table_and_field(reader)?;

        let base_type =
            reader.field_to_runtime_type(base_method_table_ptr, &field)?;

        Ok(base_type)
    }

    fn location(&self, reader: CachedReader<'_>) -> Result<Pointer, Error> {
        let (base_method_table_ptr, field) =
            self.method_table_and_field(reader)?;

        let method_table = reader.method_table(base_method_table_ptr)?;
        let module = reader.runtime_module(method_table.module())?;
        let location =
            field.location(module, crate::FieldContainer::Static, &reader)?;

        Ok(location)
    }
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
        self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<RuntimeType, Error>;
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
        self,
        gen_name: impl FnOnce() -> String,
    ) -> Result<RuntimeType, Error> {
        match self {
            RuntimeType::Array { element_type } => Ok(element_type),
            //RuntimeType::MultiDimArray { element_type, rank } => todo!(),
            _ => Err(Error::IndexAccessRequiresArray(self.clone())),
        }?
        .ok_or_else(|| Error::UnexpectedNullMethodTable(gen_name()))
        .map(|boxed| *boxed)
    }
}

trait CachedReaderExt<'a> {
    fn find_field(
        &self,
        method_table_ptr: TypedPointer<MethodTable>,
        field_name: &str,
    ) -> Result<(TypedPointer<MethodTable>, FieldDescription<'a>), Error>;

    fn find_field_type(
        &self,
        method_table_ptr: TypedPointer<MethodTable>,
        field_name: &str,
    ) -> Result<RuntimeType, Error>;
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

    fn find_field_type(
        &self,
        method_table_ptr: TypedPointer<MethodTable>,
        field_name: &str,
    ) -> Result<RuntimeType, Error> {
        let (parent_of_field, field) =
            self.find_field(method_table_ptr, field_name)?;
        let runtime_type =
            self.field_to_runtime_type(parent_of_field, &field)?;
        Ok(runtime_type)
    }
}

impl SymbolicExpr {
    pub fn parse(expr: &str, reader: CachedReader<'_>) -> Result<Self, Error> {
        let expr = SymbolicParser::new(expr, reader).parse_expr()?;
        Ok(expr)
    }

    pub fn compile(
        self,
        reader: CachedReader<'_>,
    ) -> Result<VirtualMachine, Error> {
        let vm = self
            .simplify(reader)?
            .to_physical(reader)?
            .simplify()
            .to_virtual_machine()?;
        Ok(vm)
    }

    pub(crate) fn simplify(
        self,
        reader: CachedReader<'_>,
    ) -> Result<Self, Error> {
        let expr = if let Self::Tuple(Tuple { items }) = self {
            let items = items
                .into_iter()
                .map(|item| item.simplify(reader))
                .collect::<Result<Vec<_>, Error>>()?;
            Tuple { items }.into()
        } else {
            let (expr, _inferred_type) =
                self.simplify_and_infer_type(reader)?;
            expr
        };

        Ok(expr)
    }

    fn simplify_and_infer_type(
        self,
        reader: CachedReader<'_>,
    ) -> Result<(Self, RuntimeType), Error> {
        match self {
            SymbolicExpr::Int(_) => {
                let runtime_type =
                    RuntimeType::Prim(RuntimePrimType::NativeUInt);
                Ok((self, runtime_type))
            }
            SymbolicExpr::StaticField(static_field) => {
                let runtime_type = static_field.runtime_type(reader)?;
                Ok((static_field.into(), runtime_type))
            }
            SymbolicExpr::FieldAccess(FieldAccess { obj, field }) => {
                let (obj, obj_type) = obj.simplify_and_infer_type(reader)?;

                let method_table_ptr = obj_type
                    .method_table_for_field_access(|| format!("{obj}"))?;
                let field_type =
                    reader.find_field_type(method_table_ptr, field.as_str())?;

                let expr = FieldAccess {
                    obj: Box::new(obj),
                    field,
                }
                .into();
                Ok((expr, field_type))
            }
            SymbolicExpr::IndexAccess(IndexAccess { obj, index }) => {
                let (obj, obj_type) = obj.simplify_and_infer_type(reader)?;
                let (index, _) = index.simplify_and_infer_type(reader)?;

                let element_type =
                    obj_type.as_array_type(|| format!("{obj}"))?;

                let expr = IndexAccess {
                    obj: Box::new(obj),
                    index: Box::new(index),
                }
                .into();
                Ok((expr, element_type))
            }
            SymbolicExpr::Downcast(Downcast { obj, ty }) => {
                let (obj, obj_type) = obj.simplify_and_infer_type(reader)?;

                let static_method_table_ptr =
                    obj_type.method_table_for_downcast()?;
                let target_method_table_ptr = ty.method_table(reader)?;

                if reader.is_base_of(
                    target_method_table_ptr,
                    static_method_table_ptr,
                )? {
                    // Static type is the same, or superclass of
                    // the desired runtime type.  This downcast
                    // can be simplified away.
                    Ok((obj, obj_type))
                } else if reader.is_base_of(
                    static_method_table_ptr,
                    target_method_table_ptr,
                )? {
                    // Target type is a subclass of the
                    // statically-known type.  The downcast must
                    // be retained.
                    let runtime_type = RuntimeType::Class {
                        method_table: Some(target_method_table_ptr),
                    };
                    let expr = Downcast {
                        obj: Box::new(obj),
                        ty,
                    }
                    .into();
                    Ok((expr, runtime_type))
                } else {
                    // Types are in separate hierachies.  This
                    // downcast is illegal.
                    Err(Error::DowncastRequiresRelatedClasses(
                        format!("{obj_type}"),
                        format!("{ty}"),
                    ))
                }
            }
            SymbolicExpr::Tuple(_) => {
                Err(Error::TupleExpressionOnlySupportedAtTopLevel)
            }
        }
    }

    pub(crate) fn to_physical(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<PhysicalExpr, Error> {
        match self {
            Self::Tuple(Tuple { items }) => {
                let items = items
                    .into_iter()
                    .map(|item| item.to_physical(reader))
                    .collect::<Result<Vec<_>, _>>()?;
                Ok(PhysicalExpr::Tuple(items))
            }
            other => {
                let (expr, runtime_type) =
                    other.to_physical_and_infer_type(reader)?;

                let prim_type = match runtime_type {
                    RuntimeType::Prim(runtime_prim_type) => {
                        Ok(runtime_prim_type)
                    }
                    other => {
                        Err(Error::SymbolicExpressionMustProducePrimitive {
                            field: format!("{self}"),
                            ty: other,
                        })
                    }
                }?;

                let expr = PhysicalExpr::ReadValue {
                    ptr: Box::new(expr),
                    prim_type,
                };

                Ok(expr)
            }
        }
    }

    fn to_physical_and_infer_type(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<(PhysicalExpr, RuntimeType), Error> {
        match self {
            SymbolicExpr::Int(value) => {
                let expr = PhysicalExpr::Int(*value);
                let runtime_type =
                    RuntimeType::Prim(RuntimePrimType::NativeUInt);
                Ok((expr, runtime_type))
            }
            SymbolicExpr::StaticField(static_field) => {
                let runtime_type = static_field.runtime_type(reader)?;
                let location = static_field.location(reader)?;
                let expr = PhysicalExpr::Location(location);

                let expr = if runtime_type.stored_as_ptr() {
                    PhysicalExpr::ReadValue {
                        ptr: Box::new(expr),
                        prim_type: RuntimePrimType::Ptr,
                    }
                } else {
                    expr
                };

                Ok((expr, runtime_type))
            }
            SymbolicExpr::FieldAccess(FieldAccess { obj, field }) => {
                let (expr, obj_type) =
                    obj.to_physical_and_infer_type(reader)?;

                let method_table_ptr = obj_type
                    .method_table_for_field_access(|| format!("{obj}"))?;
                let (parent_of_field, field_description) =
                    reader.find_field(method_table_ptr, field.as_str())?;
                let field_type = reader.field_to_runtime_type(
                    parent_of_field,
                    &field_description,
                )?;

                let expr = if matches!(obj_type, RuntimeType::Class { .. }) {
                    PhysicalExpr::Offset {
                        base: Box::new(expr),
                        byte_offset: Pointer::SIZE,
                    }
                } else {
                    expr
                };

                let byte_offset = field_description.offset();

                let expr = PhysicalExpr::Offset {
                    base: Box::new(expr),
                    byte_offset,
                };

                let expr = if field_type.stored_as_ptr() {
                    PhysicalExpr::ReadValue {
                        ptr: Box::new(expr),
                        prim_type: RuntimePrimType::Ptr,
                    }
                } else {
                    expr
                };

                Ok((expr, field_type))
            }
            SymbolicExpr::IndexAccess(IndexAccess { obj, index }) => {
                let (expr, array_type) =
                    obj.to_physical_and_infer_type(reader)?;
                let element_index = index.to_physical(reader)?;

                let element_type =
                    array_type.as_array_type(|| format!("{obj}"))?;

                let bytes_per_element = element_type.size_bytes();

                let expr = PhysicalExpr::Offset {
                    base: Box::new(expr),
                    byte_offset: RuntimeArray::HEADER_SIZE,
                };
                let expr = PhysicalExpr::DynamicOffset {
                    base: Box::new(expr),
                    element_index: Box::new(element_index),
                    bytes_per_element,
                };
                let expr = if element_type.stored_as_ptr() {
                    PhysicalExpr::ReadValue {
                        ptr: Box::new(expr),
                        prim_type: RuntimePrimType::Ptr,
                    }
                } else {
                    expr
                };

                Ok((expr, element_type))
            }
            SymbolicExpr::Downcast(Downcast { obj, ty }) => {
                let (obj, _obj_type) =
                    obj.to_physical_and_infer_type(reader)?;

                let target_method_table_ptr = ty.method_table(reader)?;
                let expr_type = RuntimeType::Class {
                    method_table: Some(target_method_table_ptr),
                };
                let expr = PhysicalExpr::Downcast {
                    obj: Box::new(obj),
                    ty: target_method_table_ptr,
                };

                Ok((expr, expr_type))
            }
            SymbolicExpr::Tuple(_) => {
                Err(Error::TupleExpressionOnlySupportedAtTopLevel)
            }
        }
    }

    pub fn nil() -> Self {
        Self::Tuple(Tuple { items: Vec::new() })
    }

    pub fn concat(self, other: Self) -> Self {
        match (self, other) {
            (
                Self::Tuple(Tuple { items: items_a }),
                Self::Tuple(Tuple { items: items_b }),
            ) => Tuple {
                items: items_a.into_iter().chain(items_b).collect(),
            },
            (item_a, Self::Tuple(Tuple { items: items_b })) => Tuple {
                items: std::iter::once(item_a).chain(items_b).collect(),
            },
            (Self::Tuple(Tuple { items: items_a }), item_b) => Tuple {
                items: items_a
                    .into_iter()
                    .chain(std::iter::once(item_b))
                    .collect(),
            },

            (item_a, item_b) => Tuple {
                items: vec![item_a, item_b],
            },
        }
        .into()
    }
}

impl Display for SymbolicExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolicExpr::Int(expr) => write!(f, "{expr}"),
            SymbolicExpr::StaticField(expr) => write!(f, "{expr}"),
            SymbolicExpr::FieldAccess(expr) => write!(f, "{expr}"),
            SymbolicExpr::IndexAccess(expr) => write!(f, "{expr}"),
            SymbolicExpr::Downcast(expr) => write!(f, "{expr}"),
            SymbolicExpr::Tuple(tuple) => {
                write!(f, "[")?;
                for (i, item) in tuple.items.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "]")?;
                Ok(())
            }
        }
    }
}

impl Display for StaticField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\u{200B}.{}", self.class, self.field_name)
    }
}
impl Display for FieldAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\u{200B}.{}", self.obj, self.field)
    }
}
impl Display for IndexAccess {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}[\u{200B}{}\u{200B}]", self.obj, self.index)
    }
}
impl Display for Downcast {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}.as<\u{200B}{}\u{200B}>()", self.obj, self.ty)
    }
}

impl From<Vec<SymbolicExpr>> for SymbolicExpr {
    fn from(items: Vec<SymbolicExpr>) -> Self {
        SymbolicExpr::Tuple(Tuple { items })
    }
}
