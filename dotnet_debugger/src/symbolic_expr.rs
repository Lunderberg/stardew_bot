use std::{fmt::Display, ops::Deref};

use derive_more::derive::From;
use itertools::Itertools as _;

use crate::physical_expr::{PhysicalSequence, Value as PhysicalValue};
use crate::{
    runtime_type::RuntimePrimType, CachedReader, Error, FieldDescription,
    MethodTable, RuntimeArray, RuntimeMultiDimArray, RuntimeType,
    SymbolicParser, TypedPointer, VirtualMachine,
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
    NumArrayElements(NumArrayElements),
    ArrayExtent(ArrayExtent),
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
    pub indices: Vec<SymbolicExpr>,
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

#[derive(Clone)]
pub struct NumArrayElements {
    pub array: Box<SymbolicExpr>,
}

#[derive(Clone)]
pub struct ArrayExtent {
    pub array: Box<SymbolicExpr>,
    pub dim: Box<SymbolicExpr>,
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

    pub fn downcast(self, ty: SymbolicType) -> Self {
        Downcast {
            obj: Box::new(self),
            ty,
        }
        .into()
    }

    pub fn access_field(self, field: String) -> Self {
        FieldAccess {
            obj: Box::new(self),
            field,
        }
        .into()
    }

    pub fn access_index(self, index: impl Into<SymbolicExpr>) -> Self {
        let index = index.into();
        IndexAccess {
            obj: Box::new(self),
            indices: vec![index],
        }
        .into()
    }

    pub fn access_indices<Iter>(self, indices: Iter) -> Self
    where
        Iter: IntoIterator,
        Iter::Item: Into<SymbolicExpr>,
    {
        let indices = indices.into_iter().map(Into::into).collect();
        IndexAccess {
            obj: Box::new(self),
            indices,
        }
        .into()
    }

    pub fn compile(
        self,
        reader: CachedReader<'_>,
    ) -> Result<VirtualMachine, Error> {
        let expr = self.simplify(reader)?;
        let seq = expr.to_physical_sequence(reader)?;

        let seq = seq.simplify()?;
        let seq = seq.eliminate_common_subexpresssions()?;
        let seq = seq.dead_code_elimination()?;

        let vm = seq.to_virtual_machine()?;

        let vm = vm.remove_unnecessary_restore_value();
        let vm = vm.remove_unnecessary_save_value();
        let vm = vm.remap_temporary_indices();

        Ok(vm)
    }

    pub fn simplify(self, reader: CachedReader<'_>) -> Result<Self, Error> {
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
                let runtime_type = RuntimePrimType::NativeUInt.into();
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
            SymbolicExpr::IndexAccess(IndexAccess { obj, indices }) => {
                let (obj, obj_type) = obj.simplify_and_infer_type(reader)?;
                let indices = indices
                    .into_iter()
                    .map(|index| index.simplify(reader))
                    .collect::<Result<Vec<_>, _>>()?;

                match obj_type {
                    RuntimeType::Array { .. } if indices.len() != 1 => {
                        Err(Error::IncorrectNumberOfIndices {
                            num_provided: indices.len(),
                            num_expected: 1,
                        })
                    }
                    RuntimeType::MultiDimArray { rank, .. }
                        if indices.len() != rank =>
                    {
                        Err(Error::IncorrectNumberOfIndices {
                            num_provided: indices.len(),
                            num_expected: rank,
                        })
                    }
                    RuntimeType::MultiDimArray { element_type, .. }
                    | RuntimeType::Array { element_type } => {
                        let element_type = element_type
                            .ok_or_else(|| {
                                Error::UnexpectedNullMethodTable(format!(
                                    "{obj}"
                                ))
                            })?
                            .deref()
                            .clone();

                        let expr = IndexAccess {
                            obj: Box::new(obj),
                            indices,
                        }
                        .into();

                        Ok((expr, element_type))
                    }

                    other => Err(Error::IndexAccessRequiresArray(other)),
                }
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
                } else if reader
                    .method_table(static_method_table_ptr)?
                    .is_interface()
                    || reader.is_base_of(
                        static_method_table_ptr,
                        target_method_table_ptr,
                    )?
                {
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

            SymbolicExpr::NumArrayElements(NumArrayElements { array }) => {
                let array = array.simplify(reader)?;
                let expr = NumArrayElements {
                    array: Box::new(array),
                }
                .into();
                Ok((expr, RuntimePrimType::U64.into()))
            }
            SymbolicExpr::ArrayExtent(ArrayExtent { array, dim }) => {
                let array = array.simplify(reader)?;
                let dim = dim.simplify(reader)?;
                let expr = ArrayExtent {
                    array: Box::new(array),
                    dim: Box::new(dim),
                }
                .into();
                Ok((expr, RuntimePrimType::U32.into()))
            }

            SymbolicExpr::Tuple(_) => {
                Err(Error::TupleExpressionOnlySupportedAtTopLevel)
            }
        }
    }

    pub(crate) fn to_physical_sequence(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<PhysicalSequence, Error> {
        let mut ops = PhysicalSequence::new();

        match self {
            Self::Tuple(Tuple { items }) => {
                let items = items
                    .iter()
                    .map(|item| item.collect_physical_ops(&mut ops, reader))
                    .collect::<Result<Vec<_>, _>>()?;
                ops.push(items);
            }
            other => {
                other.collect_physical_ops(&mut ops, reader)?;
            }
        }

        Ok(ops)
    }

    pub(crate) fn collect_physical_ops(
        &self,
        ops: &mut PhysicalSequence,
        reader: CachedReader<'_>,
    ) -> Result<PhysicalValue, Error> {
        let (item, _runtime_type) =
            self.collect_physical_ops_and_infer_type(ops, reader)?;

        Ok(item)
    }

    pub(crate) fn collect_physical_ops_and_infer_type(
        &self,
        ops: &mut PhysicalSequence,
        reader: CachedReader<'_>,
    ) -> Result<(PhysicalValue, RuntimeType), Error> {
        match self {
            &SymbolicExpr::Int(value) => {
                let expr = PhysicalValue::Int(value);
                let runtime_type = RuntimePrimType::NativeUInt.into();
                Ok((expr, runtime_type))
            }
            SymbolicExpr::StaticField(static_field) => {
                let runtime_type = static_field.runtime_type(reader)?;
                let location = static_field.location(reader)?;
                let expr = PhysicalValue::Ptr(location);

                let expr = if let Some(prim_type) = runtime_type.storage_type()
                {
                    ops.read_value(expr, prim_type)
                } else {
                    expr
                };

                Ok((expr, runtime_type))
            }
            SymbolicExpr::FieldAccess(FieldAccess { obj, field }) => {
                let (expr, obj_type) =
                    obj.collect_physical_ops_and_infer_type(ops, reader)?;
                let method_table_ptr = obj_type
                    .method_table_for_field_access(|| format!("{obj}"))?;
                let (parent_of_field, field_description) =
                    reader.find_field(method_table_ptr, field.as_str())?;
                let field_type = reader.field_to_runtime_type(
                    parent_of_field,
                    &field_description,
                )?;

                let expr = if matches!(obj_type, RuntimeType::Class { .. }) {
                    ops.add(expr, Pointer::SIZE)
                } else {
                    expr
                };

                let expr = ops.add(expr, field_description.offset());

                let expr = if let Some(prim_type) = field_type.storage_type() {
                    ops.read_value(expr, prim_type)
                } else {
                    expr
                };

                Ok((expr, field_type))
            }
            SymbolicExpr::IndexAccess(IndexAccess { obj, indices }) => {
                let (expr, array_type) =
                    obj.collect_physical_ops_and_infer_type(ops, reader)?;
                let indices = indices
                    .iter()
                    .map(|index| index.collect_physical_ops(ops, reader))
                    .collect::<Result<Vec<_>, _>>()?;

                let (element_type, header_size_bytes, shape): (
                    _,
                    _,
                    Vec<PhysicalValue>,
                ) = match array_type {
                    RuntimeType::Array { element_type } => {
                        let header_size_bytes = RuntimeArray::HEADER_SIZE;
                        let num_elements_ptr = ops.add(expr, Pointer::SIZE);
                        let num_elements = ops
                            .read_value(num_elements_ptr, RuntimePrimType::U64);
                        let shape = vec![num_elements];
                        Ok((element_type, header_size_bytes, shape))
                    }
                    RuntimeType::MultiDimArray { element_type, rank } => {
                        let shape_start =
                            ops.add(expr, RuntimeArray::HEADER_SIZE);
                        let shape = (0..rank)
                            .map(|i| {
                                let extent_ptr = ops.add(
                                    shape_start,
                                    i * RuntimePrimType::U32.size_bytes(),
                                );
                                let extent = ops.read_value(
                                    extent_ptr,
                                    RuntimePrimType::U32,
                                );
                                extent
                            })
                            .collect();
                        let header_size_bytes =
                            RuntimeMultiDimArray::header_size(rank);

                        Ok((element_type, header_size_bytes, shape))
                    }
                    other => Err(Error::IndexAccessRequiresArray(other)),
                }?;

                if shape.len() != indices.len() {
                    return Err(Error::IncorrectNumberOfIndices {
                        num_provided: indices.len(),
                        num_expected: shape.len(),
                    });
                }

                let element_type = element_type
                    .ok_or_else(|| {
                        Error::UnexpectedNullMethodTable(format!("{obj}"))
                    })?
                    .deref()
                    .clone();

                let expr = {
                    let first_element = ops.add(expr, header_size_bytes);
                    let item_offset = {
                        let mut flat_index: PhysicalValue = 0.into();
                        let mut stride: PhysicalValue = 1.into();
                        for (i, dim) in
                            indices.into_iter().zip(shape.into_iter()).rev()
                        {
                            let offset = ops.mul(stride, i);
                            flat_index = ops.add(flat_index, offset);
                            stride = ops.mul(stride, dim);
                        }
                        flat_index
                    };
                    let byte_offset =
                        ops.mul(item_offset, element_type.size_bytes());
                    ops.add(first_element, byte_offset)
                };

                let expr = if let Some(prim_type) = element_type.storage_type()
                {
                    ops.read_value(expr, prim_type)
                } else {
                    expr
                };

                Ok((expr, element_type))
            }
            SymbolicExpr::Downcast(Downcast { obj, ty }) => {
                let (obj, _obj_type) =
                    obj.collect_physical_ops_and_infer_type(ops, reader)?;

                let target_method_table_ptr = ty.method_table(reader)?;
                let expr_type = RuntimeType::Class {
                    method_table: Some(target_method_table_ptr),
                };
                let expr = ops.downcast(obj, target_method_table_ptr);

                Ok((expr, expr_type))
            }
            SymbolicExpr::NumArrayElements(NumArrayElements { array }) => {
                let (array, array_type) =
                    array.collect_physical_ops_and_infer_type(ops, reader)?;
                match array_type {
                    RuntimeType::Array { .. }
                    | RuntimeType::MultiDimArray { .. } => {
                        let num_elements_ptr = ops.add(array, Pointer::SIZE);
                        let expr = ops
                            .read_value(num_elements_ptr, RuntimePrimType::U64);
                        Ok((expr, RuntimePrimType::U64.into()))
                    }

                    other => Err(Error::ArrayLengthRequiresArray(other)),
                }
            }
            SymbolicExpr::ArrayExtent(ArrayExtent { array, dim }) => {
                let (array, array_type) =
                    array.collect_physical_ops_and_infer_type(ops, reader)?;
                let dim = dim.collect_physical_ops(ops, reader)?;

                match array_type {
                    RuntimeType::MultiDimArray { .. } => {
                        // TODO: Assert that `dim < rank`.  Will
                        // require implementing support for runtime
                        // assertions.
                        let dim_offset =
                            ops.mul(dim, RuntimePrimType::U32.size_bytes());
                        let offset =
                            ops.add(dim_offset, RuntimeArray::HEADER_SIZE);
                        let extent_ptr = ops.add(array, offset);
                        let extent =
                            ops.read_value(extent_ptr, RuntimePrimType::U32);
                        Ok((extent, RuntimePrimType::U32.into()))
                    }
                    other => Err(
                        Error::ArrayExtentRequiresMultiDimensionalArray(other),
                    ),
                }
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
            SymbolicExpr::NumArrayElements(expr) => write!(f, "{expr}"),
            SymbolicExpr::ArrayExtent(expr) => write!(f, "{expr}"),

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

impl Display for SymbolicType {
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
        write!(f, "{}[\u{200B}", self.obj)?;

        for (i, index) in self.indices.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{index}")?;
        }

        write!(f, "\u{200B}]")?;
        Ok(())
    }
}
impl Display for NumArrayElements {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\u{200B}.len()", self.array)
    }
}
impl Display for ArrayExtent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\u{200B}.extent({})", self.array, self.dim)
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
