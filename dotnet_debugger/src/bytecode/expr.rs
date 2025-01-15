use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use derive_more::derive::From;
use itertools::Itertools as _;

use iterator_extensions::ResultIteratorExt as _;
use memory_reader::Pointer;

use crate::{
    bytecode::virtual_machine::{Instruction, VMArg},
    runtime_type::RuntimePrimType,
    CachedReader, Error, FieldDescription, MethodTable, OpIndex, RuntimeArray,
    RuntimeMultiDimArray, RuntimePrimValue, RuntimeType, TypedPointer,
    VirtualMachine,
};

#[derive(Default, Clone)]
pub struct SymbolicGraph {
    ops: Vec<SymbolicExpr>,
    outputs: Vec<SymbolicValue>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueToken(pub(crate) usize);

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum SymbolicExpr {
    /// A static member of a class.  These are specified in terms of
    /// the class's name, and the name of the field.
    ///
    /// These are lowered into a pointer to the known location the
    /// field.
    StaticField(StaticField),

    /// Access of an non-static member of a class or struct.
    ///
    /// These are lowered to pointer arithmetic, performed relative to
    /// the location of the class or struct.
    FieldAccess { obj: SymbolicValue, field: String },

    /// Downcast an object to a subclass.  After downcasting, fields
    /// of the subclass may be accessed.
    ///
    /// This is currently lowered to PhysicalDowncast, which has
    /// explicitly implemented in the VM.  In the future, this may
    /// instead be lowered into conditional statements.
    SymbolicDowncast {
        obj: SymbolicValue,
        ty: SymbolicType,
    },

    /// Access an element of an array, or multi-dimensional array.
    ///
    /// These are lowered to pointer arithmetic, to determine the
    /// location of the array element given the location of the array.
    IndexAccess {
        obj: SymbolicValue,
        indices: Vec<SymbolicValue>,
    },

    /// Returns the number of elements of an array, or
    /// multi-dimensional array.
    ///
    /// This is lowered into pointer arithmetic and memory accesses,
    /// to locate and read the number of elements of the array.
    NumArrayElements { array: SymbolicValue },

    /// Returns the number of elements of a multi-dimensional array.
    ///
    /// This is lowered into pointer arithmetic and memory accesses,
    /// to locate and read the shape of the array.
    ArrayExtent {
        array: SymbolicValue,
        dim: SymbolicValue,
    },

    /// Perform addition of the LHS and RHS.
    ///
    /// This operation is used for both pointer arithmetic and numeric
    /// arithmetic.
    Add {
        lhs: SymbolicValue,
        rhs: SymbolicValue,
    },

    /// Perform numeric multiplication of the LHS and RHS.
    Mul {
        lhs: SymbolicValue,
        rhs: SymbolicValue,
    },

    PrimCast {
        value: SymbolicValue,
        prim_type: RuntimePrimType,
    },

    /// Downcast into a subclass.
    ///
    /// If the downcast is successful, the result is the same pointer
    /// as was provided as input.  if the downcast is not successful,
    /// the result is None.
    PhysicalDowncast {
        obj: SymbolicValue,
        ty: TypedPointer<MethodTable>,
    },

    /// Read a value from memory
    ///
    /// Given a pointer to a location in the remote process, read a
    /// value at that location.
    ReadValue {
        ptr: SymbolicValue,
        prim_type: RuntimePrimType,
    },
}

#[derive(PartialEq, Eq, Hash, Clone, Copy, From)]
pub enum SymbolicValue {
    Int(usize),
    Ptr(Pointer),
    Result(OpIndex),
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct SymbolicType {
    /// The full name of the type, including the namespace, if any.
    pub full_name: String,
    pub generics: Vec<SymbolicType>,
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct StaticField {
    pub class: SymbolicType,
    pub field_name: String,
}

#[derive(Clone, Copy)]
pub struct ExprPrinter<'a> {
    graph: &'a SymbolicGraph,
    value: &'a SymbolicValue,
}

impl SymbolicType {
    pub(crate) fn method_table(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        let method_table_ptr = reader
            .method_table_by_name(&self.full_name)?
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

    pub(crate) fn try_prim_type(&self) -> Option<RuntimePrimType> {
        match self.full_name.as_str() {
            "bool" => Some(RuntimePrimType::Bool),
            "char" => Some(RuntimePrimType::Char),
            "u8" => Some(RuntimePrimType::U8),
            "u16" => Some(RuntimePrimType::U16),
            "u32" => Some(RuntimePrimType::U32),
            "u64" => Some(RuntimePrimType::U64),
            "usize" => Some(RuntimePrimType::NativeUInt),
            "i8" => Some(RuntimePrimType::I8),
            "i16" => Some(RuntimePrimType::I16),
            "i32" => Some(RuntimePrimType::I32),
            "i64" => Some(RuntimePrimType::I64),
            "isize" => Some(RuntimePrimType::NativeInt),
            "f32" => Some(RuntimePrimType::F32),
            "f64" => Some(RuntimePrimType::F64),
            _ => None,
        }
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

pub trait Printable<'a> {
    fn top_level_value(self, graph: &'a SymbolicGraph) -> &'a SymbolicValue;
}
impl<'a> Printable<'a> for ValueToken {
    fn top_level_value(self, graph: &'a SymbolicGraph) -> &'a SymbolicValue {
        &graph.outputs[self.0]
    }
}
impl<'a, 'b: 'a> Printable<'a> for &'b SymbolicValue {
    fn top_level_value(self, _: &'a SymbolicGraph) -> &'a SymbolicValue {
        self
    }
}

impl SymbolicGraph {
    pub fn new() -> Self {
        Self {
            ops: Vec::new(),
            outputs: Vec::new(),
        }
    }

    pub fn num_operations(&self) -> usize {
        self.ops.len()
    }

    pub fn num_outputs(&self) -> usize {
        self.outputs.len()
    }

    pub fn parse(
        &mut self,
        text: &str,
        reader: CachedReader,
    ) -> Result<SymbolicValue, Error> {
        let mut parser = super::SymbolicParser::new(text, reader, self);
        parser.parse_expr()
    }

    pub fn push(&mut self, op: impl Into<SymbolicExpr>) -> SymbolicValue {
        let op_index = OpIndex::new(self.ops.len());
        self.ops.push(op.into());
        op_index.into()
    }

    pub fn mark_output(&mut self, value: SymbolicValue) -> ValueToken {
        let index = self.outputs.len();
        self.outputs.push(value);
        ValueToken(index)
    }

    pub fn print<'a>(&'a self, value: impl Printable<'a>) -> ExprPrinter<'a> {
        let value = value.top_level_value(self);
        ExprPrinter { graph: self, value }
    }

    //////////////////////////////////////////////////
    ////          Symbolic Operations              ///
    //////////////////////////////////////////////////

    pub fn static_field(
        &mut self,
        class: impl Into<SymbolicType>,
        field_name: impl Into<String>,
    ) -> SymbolicValue {
        let class = class.into();
        let field_name = field_name.into();
        self.push(StaticField { class, field_name })
    }

    pub fn access_field(
        &mut self,
        obj: impl Into<SymbolicValue>,
        field: impl AsRef<str>,
    ) -> SymbolicValue {
        let mut obj = obj.into();
        let field = field.as_ref();

        for subfield in field.split(".") {
            obj = self.push(SymbolicExpr::FieldAccess {
                obj,
                field: subfield.into(),
            });
        }
        obj
    }

    pub fn access_index(
        &mut self,
        obj: impl Into<SymbolicValue>,
        index: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let obj = obj.into();
        let index = self.prim_cast(index, RuntimePrimType::NativeUInt);
        self.push(SymbolicExpr::IndexAccess {
            obj,
            indices: vec![index],
        })
    }

    pub fn access_indices<Iter>(
        &mut self,
        obj: impl Into<SymbolicValue>,
        indices: Iter,
    ) -> SymbolicValue
    where
        Iter: IntoIterator,
        Iter::Item: Into<SymbolicValue>,
    {
        let obj = obj.into();
        let indices = indices
            .into_iter()
            .map(|index| self.prim_cast(index, RuntimePrimType::NativeUInt))
            .collect();
        self.push(SymbolicExpr::IndexAccess { obj, indices })
    }

    pub fn downcast(
        &mut self,
        obj: impl Into<SymbolicValue>,
        ty: impl Into<SymbolicType>,
    ) -> SymbolicValue {
        let obj = obj.into();
        let ty = ty.into();
        self.push(SymbolicExpr::SymbolicDowncast { obj, ty })
    }

    pub fn num_array_elements(
        &mut self,
        array: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let array = array.into();
        self.push(SymbolicExpr::NumArrayElements { array })
    }

    pub fn array_extent(
        &mut self,
        array: impl Into<SymbolicValue>,
        dim: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let array = array.into();
        let dim = dim.into();
        self.push(SymbolicExpr::ArrayExtent { array, dim })
    }

    //////////////////////////////////////////////////
    ////          Physical Operations              ///
    //////////////////////////////////////////////////

    pub fn add(
        &mut self,
        lhs: impl Into<SymbolicValue>,
        rhs: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push(SymbolicExpr::Add { lhs, rhs })
    }

    pub fn mul(
        &mut self,
        lhs: impl Into<SymbolicValue>,
        rhs: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push(SymbolicExpr::Mul { lhs, rhs })
    }

    pub fn prim_cast(
        &mut self,
        value: impl Into<SymbolicValue>,
        prim_type: RuntimePrimType,
    ) -> SymbolicValue {
        let value = value.into();
        self.push(SymbolicExpr::PrimCast { value, prim_type })
    }

    pub fn physical_downcast(
        &mut self,
        obj: impl Into<SymbolicValue>,
        ty: TypedPointer<MethodTable>,
    ) -> SymbolicValue {
        let obj = obj.into();
        self.push(SymbolicExpr::PhysicalDowncast { obj, ty })
    }

    pub fn read_value(
        &mut self,
        ptr: impl Into<SymbolicValue>,
        prim_type: RuntimePrimType,
    ) -> SymbolicValue {
        let ptr = ptr.into();
        self.push(SymbolicExpr::ReadValue { ptr, prim_type })
    }

    //////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////

    fn iter_ops(&self) -> impl Iterator<Item = (OpIndex, &SymbolicExpr)> + '_ {
        self.ops
            .iter()
            .enumerate()
            .map(|(i, op)| (OpIndex::new(i), op))
    }

    pub fn iter_outputs(
        &self,
    ) -> impl Iterator<Item = (ValueToken, SymbolicValue)> + '_ {
        self.outputs
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, op_index)| (ValueToken(i), op_index))
    }

    fn infer_types(
        &self,
        reader: CachedReader,
    ) -> Result<HashMap<SymbolicValue, RuntimeType>, Error> {
        let mut output = HashMap::<SymbolicValue, RuntimeType>::new();

        for (index, op) in self.iter_ops() {
            op.visit_input_values(|value| {
                let opt_runtime_type: Option<RuntimeType> = match value {
                    SymbolicValue::Int(_) => {
                        Some(RuntimePrimType::NativeUInt.into())
                    }
                    SymbolicValue::Ptr(_) => Some(RuntimePrimType::Ptr.into()),
                    SymbolicValue::Result(_) => None,
                };

                if let Some(ty) = opt_runtime_type {
                    output.entry(value).or_insert(ty);
                }
            });

            let lookup_type =
                |prev_value: &SymbolicValue| -> Result<&RuntimeType, Error> {
                    output.get(prev_value).ok_or_else(|| {
                        let op_index = match prev_value {
                            SymbolicValue::Result(op_index) => op_index,
                            _ => {
                                unreachable!("Handled in .visit_input_values()")
                            }
                        };
                        Error::InvalidReference {
                            from: index,
                            to: *op_index,
                        }
                    })
                };

            let runtime_type = match op {
                SymbolicExpr::StaticField(static_field) => {
                    static_field.runtime_type(reader)?
                }
                SymbolicExpr::FieldAccess { obj, field } => {
                    let obj_type = lookup_type(&(*obj).into())?;

                    let method_table_ptr = obj_type
                        .method_table_for_field_access(|| {
                            format!("{}", self.print(obj))
                        })?;
                    let field_type = reader
                        .find_field_type(method_table_ptr, field.as_str())?;
                    field_type
                }
                SymbolicExpr::IndexAccess {
                    obj: array,
                    indices,
                } => {
                    let array_type = lookup_type(&(*array).into())?;
                    let num_indices = indices.len();

                    match array_type {
                        RuntimeType::Array { .. } if num_indices != 1 => {
                            Err(Error::IncorrectNumberOfIndices {
                                num_provided: num_indices,
                                num_expected: 1,
                            })
                        }
                        RuntimeType::MultiDimArray { rank, .. }
                            if num_indices != *rank =>
                        {
                            return Err(Error::IncorrectNumberOfIndices {
                                num_provided: num_indices,
                                num_expected: *rank,
                            });
                        }
                        RuntimeType::MultiDimArray {
                            method_table: None,
                            ..
                        }
                        | RuntimeType::Array {
                            method_table: None, ..
                        } => {
                            return Err(Error::UnexpectedNullMethodTable(
                                format!("{}", self.print(array)),
                            ));
                        }
                        RuntimeType::MultiDimArray {
                            method_table: Some(ptr),
                            ..
                        }
                        | RuntimeType::Array {
                            method_table: Some(ptr),
                            ..
                        } => {
                            let method_table = reader.method_table(*ptr)?;
                            method_table
                                .array_element_type()
                                .ok_or(Error::ArrayMissingElementType)
                                .and_then(|ptr| reader.runtime_type(ptr))
                        }

                        other => {
                            Err(Error::IndexAccessRequiresArray(other.clone()))
                        }
                    }?
                }
                SymbolicExpr::SymbolicDowncast { ty, .. } => {
                    let method_table = ty.method_table(reader)?;
                    RuntimeType::Class {
                        method_table: Some(method_table),
                    }
                }
                SymbolicExpr::NumArrayElements { .. } => {
                    RuntimePrimType::NativeUInt.into()
                }
                SymbolicExpr::ArrayExtent { .. } => {
                    RuntimePrimType::NativeUInt.into()
                }

                SymbolicExpr::Add { lhs, rhs } => {
                    let lhs_type = lookup_type(lhs)?;
                    let rhs_type = lookup_type(rhs)?;
                    match (lhs_type, rhs_type) {
                        (
                            RuntimeType::Prim(RuntimePrimType::Ptr),
                            RuntimeType::Prim(prim_rhs),
                        ) if prim_rhs.is_integer() => Ok(RuntimePrimType::Ptr),
                        (
                            RuntimeType::Prim(prim_lhs),
                            RuntimeType::Prim(RuntimePrimType::Ptr),
                        ) if prim_lhs.is_integer() => Ok(RuntimePrimType::Ptr),
                        (
                            RuntimeType::Prim(prim_lhs),
                            RuntimeType::Prim(prim_rhs),
                        ) if prim_lhs.is_integer() && prim_rhs.is_integer() => {
                            Ok(RuntimePrimType::NativeUInt)
                        }
                        (other_lhs, other_rhs) => {
                            Err(Error::InvalidOperandsForAddition {
                                lhs: other_lhs.clone(),
                                rhs: other_rhs.clone(),
                            })
                        }
                    }?
                    .into()
                }
                SymbolicExpr::Mul { lhs, rhs } => {
                    let lhs_type = lookup_type(lhs)?;
                    let rhs_type = lookup_type(rhs)?;
                    match (lhs_type, rhs_type) {
                        (
                            RuntimeType::Prim(prim_lhs),
                            RuntimeType::Prim(prim_rhs),
                        ) if prim_lhs.is_integer() && prim_rhs.is_integer() => {
                            Ok(RuntimePrimType::NativeUInt)
                        }
                        (other_lhs, other_rhs) => {
                            Err(Error::InvalidOperandsForMultiplication {
                                lhs: other_lhs.clone(),
                                rhs: other_rhs.clone(),
                            })
                        }
                    }?
                    .into()
                }
                SymbolicExpr::PrimCast { prim_type, .. } => (*prim_type).into(),
                SymbolicExpr::PhysicalDowncast { .. } => {
                    RuntimePrimType::Ptr.into()
                }
                SymbolicExpr::ReadValue { prim_type, .. } => {
                    (*prim_type).into()
                }
            };

            output.insert(index.into(), runtime_type);
        }

        Ok(output)
    }

    //////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////

    pub fn validate(&self) -> Result<(), Error> {
        for (index, op) in self.iter_ops() {
            let mut result = Ok(());
            op.visit_input_values(|input_value| {
                if let SymbolicValue::Result(prev_index) = input_value {
                    if prev_index.0 >= index.0 {
                        result = Err(Error::InvalidReference {
                            from: index,
                            to: prev_index,
                        });
                    }
                }
            });
            result?;
        }

        Ok(())
    }

    fn mark_new_outputs(
        &mut self,
        prev_outputs: &[SymbolicValue],
        remap: &HashMap<OpIndex, SymbolicValue>,
    ) {
        for old_value in prev_outputs {
            let new_value = match old_value {
                SymbolicValue::Result(old_index) => {
                    remap.get(old_index).unwrap_or_else(|| {
                        unreachable!(
                            "Internal error: \
                         All indices should be populated at this point, \
                         but {old_index} did not have a remapped value."
                        )
                    })
                }
                other => other,
            };

            self.mark_output(*new_value);
        }
    }

    pub fn simplify(&self, reader: CachedReader) -> Result<Self, Error> {
        let mut prev_index_lookup: HashMap<OpIndex, SymbolicValue> =
            HashMap::new();
        let old_runtime_types = self.infer_types(reader)?;

        let mut new_runtime_types: HashMap<OpIndex, RuntimeType> =
            HashMap::new();
        let mut builder = Self::new();

        for (old_index, op) in self.iter_ops() {
            let op = op.clone().remap(old_index, &prev_index_lookup)?;
            let mut value = builder.push(op);

            let lookup_type =
                |new_value: SymbolicValue| -> Result<RuntimeType, Error> {
                    match new_value {
                        SymbolicValue::Int(_) => {
                            Ok(RuntimePrimType::NativeUInt.into())
                        }
                        SymbolicValue::Ptr(_) => {
                            Ok(RuntimePrimType::Ptr.into())
                        }
                        SymbolicValue::Result(op_index) => {
                            Ok(new_runtime_types
                                .get(&op_index)
                                .ok_or_else(|| Error::InvalidReference {
                                    from: old_index,
                                    to: op_index,
                                })?
                                .clone())
                        }
                    }
                };

            let mut do_simplify_step = |index: OpIndex| -> Result<
                Option<SymbolicValue>,
                Error,
            > {
                Ok(match &builder[index] {
                    // Remove unnecessary downcasts
                    SymbolicExpr::SymbolicDowncast { obj, ty } => {
                        let obj_type = lookup_type(*obj)?;

                        let static_method_table_ptr =
                            obj_type.method_table_for_downcast()?;
                        let target_method_table_ptr =
                            ty.method_table(reader)?;

                        if reader.is_base_of(
                            target_method_table_ptr,
                            static_method_table_ptr,
                        )? {
                            // Static type is the same, or superclass of
                            // the desired runtime type.  This downcast
                            // can be simplified away.
                            Some(*obj)
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
                            None
                        } else {
                            // Types are in separate hierachies.  This
                            // downcast is illegal.
                            return Err(Error::DowncastRequiresRelatedClasses(
                                format!("{obj_type}"),
                                format!("{ty}"),
                            ));
                        }
                    }

                    // Remove unnecessary PrimCast
                    SymbolicExpr::PrimCast { value, prim_type } => {
                        let value_type = lookup_type(*value)?;

                        if value_type == *prim_type {
                            Some(*value)
                        } else {
                            None
                        }
                    }

                    // Constant-folding, lhs and rhs are known
                    &SymbolicExpr::Add {
                        lhs: SymbolicValue::Int(a),
                        rhs: SymbolicValue::Int(b),
                    } => Some(SymbolicValue::Int(a + b)),

                    &SymbolicExpr::Mul {
                        lhs: SymbolicValue::Int(a),
                        rhs: SymbolicValue::Int(b),
                    } => Some(SymbolicValue::Int(a * b)),

                    // lhs + 0 => lhs
                    &SymbolicExpr::Add {
                        lhs,
                        rhs: SymbolicValue::Int(0),
                    } => Some(lhs),

                    // 0 + rhs => rhs
                    &SymbolicExpr::Add {
                        lhs: SymbolicValue::Int(0),
                        rhs,
                    } => Some(rhs),

                    // 0 * rhs => 0
                    SymbolicExpr::Mul {
                        rhs: SymbolicValue::Int(0),
                        ..
                    } => Some(SymbolicValue::Int(0)),

                    // lhs * 0 => 0
                    SymbolicExpr::Mul {
                        lhs: SymbolicValue::Int(0),
                        ..
                    } => Some(SymbolicValue::Int(0)),

                    // lhs * 1 => lhs
                    &SymbolicExpr::Mul {
                        lhs,
                        rhs: SymbolicValue::Int(1),
                    } => Some(lhs),

                    // 1 * rhs => rhs
                    &SymbolicExpr::Mul {
                        lhs: SymbolicValue::Int(1),
                        rhs,
                    } => Some(rhs),

                    // const + rhs => rhs + const
                    &SymbolicExpr::Add {
                        lhs: lhs @ SymbolicValue::Int(_),
                        rhs,
                    } => Some(builder.add(rhs, lhs)),

                    // const * rhs => rhs * const
                    &SymbolicExpr::Mul {
                        lhs: lhs @ SymbolicValue::Int(_),
                        rhs,
                    } => Some(builder.mul(rhs, lhs)),

                    // lhs + a + b => lhs + (a+b)
                    &SymbolicExpr::Add {
                        lhs: SymbolicValue::Result(lhs),
                        rhs: SymbolicValue::Int(b),
                    } => match &builder[lhs] {
                        &SymbolicExpr::Add {
                            lhs,
                            rhs: SymbolicValue::Int(a),
                        } => Some(builder.add(lhs, a + b)),
                        _ => None,
                    },

                    // lhs * a * b => lhs * (a*b)
                    &SymbolicExpr::Mul {
                        lhs: SymbolicValue::Result(lhs),
                        rhs: SymbolicValue::Int(b),
                    } => match &builder[lhs] {
                        &SymbolicExpr::Mul {
                            lhs,
                            rhs: SymbolicValue::Int(a),
                        } => Some(builder.mul(lhs, a * b)),
                        _ => None,
                    },

                    _ => None,
                })
            };

            while let SymbolicValue::Result(index) = value {
                if let Some(simplified) = do_simplify_step(index)? {
                    value = simplified;
                } else {
                    break;
                }
            }

            prev_index_lookup.insert(old_index, value);
            if let SymbolicValue::Result(new_index) = value {
                let runtime_type = old_runtime_types
                    .get(&SymbolicValue::Result(old_index))
                    .expect("All indices should have known type");
                new_runtime_types.insert(new_index, runtime_type.clone());
            }
        }

        builder.mark_new_outputs(&self.outputs, &prev_index_lookup);

        Ok(builder)
    }

    pub fn dead_code_elimination(self) -> Result<Self, Error> {
        let mut prev_index_lookup: HashMap<OpIndex, SymbolicValue> =
            HashMap::new();
        let mut builder = Self::new();

        let reachable = {
            let mut to_visit: Vec<_> = self
                .outputs
                .iter()
                .filter_map(|value| value.as_op_index())
                .collect();
            let mut reachable: HashSet<_> = to_visit.iter().cloned().collect();

            while let Some(visiting) = to_visit.pop() {
                self[visiting].visit_input_values(|upstream| {
                    if let SymbolicValue::Result(upstream) = upstream {
                        if !reachable.contains(&upstream) {
                            reachable.insert(upstream);
                            to_visit.push(upstream);
                        }
                    }
                });
            }

            reachable
        };

        for (prev_index, op) in self.iter_ops() {
            if reachable.contains(&prev_index) {
                let op = op.clone().remap(prev_index, &prev_index_lookup)?;
                let new_index = builder.push(op);
                prev_index_lookup.insert(prev_index, new_index);
            }
        }

        builder.mark_new_outputs(&self.outputs, &prev_index_lookup);

        Ok(builder)
    }

    pub fn eliminate_common_subexpresssions(self) -> Result<Self, Error> {
        let mut builder = Self::new();

        let mut prev_index_lookup: HashMap<OpIndex, SymbolicValue> =
            HashMap::new();
        let mut dedup_lookup: HashMap<SymbolicExpr, SymbolicValue> =
            HashMap::new();

        for (prev_index, op) in self.iter_ops() {
            let new_op = op.clone().remap(prev_index, &prev_index_lookup)?;

            let new_index = if let Some(new_index) = dedup_lookup.get(&new_op) {
                *new_index
            } else {
                let new_index = builder.push(new_op.clone());
                dedup_lookup.insert(new_op, new_index);
                new_index
            };
            prev_index_lookup.insert(prev_index, new_index);
        }

        builder.mark_new_outputs(&self.outputs, &prev_index_lookup);

        Ok(builder)
    }

    //////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////

    pub fn to_physical_graph(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<Self, Error> {
        let runtime_type_lookup = self.infer_types(reader)?;

        let mut builder = Self::new();
        let mut prev_value_lookup = HashMap::<OpIndex, SymbolicValue>::new();

        for (symbolic_index, op) in self.iter_ops() {
            let physical_index = op.to_physical_expr(
                reader,
                self,
                &mut builder,
                symbolic_index,
                &prev_value_lookup,
                &runtime_type_lookup,
            )?;
            prev_value_lookup.insert(symbolic_index, physical_index);
        }

        for (symbolic_token, symbolic_output) in self.iter_outputs() {
            let physical_output = match symbolic_output {
                SymbolicValue::Result(op_index) => prev_value_lookup
                    .get(&op_index)
                    .expect("All outputs should be processed by this point")
                    .clone(),
                other => other,
            };
            let output_token = builder.mark_output(physical_output);
            assert!(symbolic_token == output_token);
        }

        Ok(builder)
    }

    pub fn to_virtual_machine(&self) -> Result<VirtualMachine, Error> {
        let output_indices: HashMap<OpIndex, ValueToken> = self
            .iter_outputs()
            .filter_map(|(output, value)| match value {
                SymbolicValue::Result(op_index) => Some((op_index, output)),
                _ => None,
            })
            .collect();

        let num_outputs = self.outputs.len();

        let mut instructions = Vec::new();
        let mut currently_stored: HashMap<OpIndex, usize> = HashMap::new();
        let mut next_free_index = num_outputs;

        macro_rules! value_to_register {
            ($arg:expr) => {
                match $arg {
                    &SymbolicValue::Int(value) => Instruction::Const {
                        value: RuntimePrimValue::NativeUInt(value),
                    },
                    &SymbolicValue::Ptr(ptr) => Instruction::Const {
                        value: RuntimePrimValue::Ptr(ptr),
                    },
                    SymbolicValue::Result(op_index) => {
                        let index = *currently_stored.get(&op_index).expect(
                            "Internal error, \
                             {op_index} not located anywhere",
                        );
                        Instruction::RestoreValue { index }
                    }
                }
            };
        }

        macro_rules! value_to_arg {
            ($arg:expr) => {
                match $arg {
                    &SymbolicValue::Int(value) => {
                        VMArg::Const(RuntimePrimValue::NativeUInt(value))
                    }
                    &SymbolicValue::Ptr(ptr) => {
                        VMArg::Const(RuntimePrimValue::Ptr(ptr))
                    }
                    SymbolicValue::Result(op_index) => {
                        let index = *currently_stored.get(&op_index).expect(
                            "Internal error, \
                             {op_index} not located anywhere",
                        );
                        VMArg::SavedValue { index }
                    }
                }
            };
        }

        for (op_index, op) in self.iter_ops() {
            match op {
                SymbolicExpr::Add { lhs, rhs } => {
                    instructions.push(value_to_register!(lhs));
                    instructions.push(Instruction::Add(value_to_arg!(rhs)));
                }
                SymbolicExpr::Mul { lhs, rhs } => {
                    instructions.push(value_to_register!(lhs));
                    instructions.push(Instruction::Mul(value_to_arg!(rhs)));
                }
                SymbolicExpr::PrimCast { value, prim_type } => {
                    instructions.push(value_to_register!(value));
                    instructions.push(Instruction::PrimCast(*prim_type));
                }
                &SymbolicExpr::PhysicalDowncast { obj, ty } => {
                    let obj_instruction = match obj {
                        SymbolicValue::Ptr(ptr) => Instruction::Const {
                            value: RuntimePrimValue::Ptr(ptr),
                        },
                        SymbolicValue::Result(op_index) => {
                            let index =
                                *currently_stored.get(&op_index).expect(
                                    "Internal error, \
                                     {op_index} not located anywhere",
                                );
                            Instruction::RestoreValue { index }
                        }
                        SymbolicValue::Int(value) => panic!(
                            "LHS of downcast should be a pointer, \
                             but was instead {value}"
                        ),
                    };
                    instructions.push(obj_instruction);
                    instructions.push(Instruction::Downcast { ty });
                }
                &SymbolicExpr::ReadValue { ptr, prim_type } => {
                    let obj_instruction = match ptr {
                        SymbolicValue::Ptr(ptr) => Instruction::Const {
                            value: RuntimePrimValue::Ptr(ptr),
                        },
                        SymbolicValue::Result(op_index) => {
                            let index =
                                *currently_stored.get(&op_index).expect(
                                    "Internal error, \
                                     {op_index} not located anywhere",
                                );
                            Instruction::RestoreValue { index }
                        }
                        SymbolicValue::Int(value) => panic!(
                            "LHS of ReadValue should be a pointer, \
                             but was instead {value}"
                        ),
                    };
                    instructions.push(obj_instruction);
                    instructions.push(Instruction::Read { ty: prim_type });
                }
                symbolic @ (SymbolicExpr::StaticField(_)
                | SymbolicExpr::FieldAccess { .. }
                | SymbolicExpr::SymbolicDowncast { .. }
                | SymbolicExpr::IndexAccess { .. }
                | SymbolicExpr::NumArrayElements { .. }
                | SymbolicExpr::ArrayExtent { .. }) => {
                    return Err(Error::SymbolicExpressionRequiresLowering(
                        symbolic.clone(),
                    ));
                }
            }

            let register_index = output_indices
                .get(&op_index)
                .map(|token| token.0)
                .unwrap_or_else(|| {
                    let index = next_free_index;
                    next_free_index += 1;
                    index
                });
            instructions.push(Instruction::SaveValue {
                index: register_index,
            });
            currently_stored.insert(op_index, register_index);
        }

        for (output, value) in self.iter_outputs() {
            let constant: Option<RuntimePrimValue> = match value {
                SymbolicValue::Result(_) => None,
                SymbolicValue::Int(value) => Some(value.into()),
                SymbolicValue::Ptr(ptr) => Some(ptr.into()),
            };
            if let Some(value) = constant {
                instructions.push(Instruction::Const { value });
                instructions.push(Instruction::SaveValue { index: output.0 });
            }
        }

        Ok(VirtualMachine::new(instructions, num_outputs))
    }

    pub fn compile(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<VirtualMachine, Error> {
        let expr = self.clone();

        // Symbolic expressions, in terms of class/field names.
        let expr = expr.eliminate_common_subexpresssions()?;
        expr.validate()?;
        let expr = expr.dead_code_elimination()?;
        expr.validate()?;
        let expr = expr.simplify(reader)?;
        expr.validate()?;

        // Physical expressions, in terms of pointer/offsets.
        let seq = expr.to_physical_graph(reader)?;
        seq.validate()?;
        let seq = seq.simplify(reader)?;
        seq.validate()?;
        let seq = seq.eliminate_common_subexpresssions()?;
        seq.validate()?;
        let seq = seq.dead_code_elimination()?;

        // Virtual machine, in terms of sequential operations.
        let vm = seq.to_virtual_machine()?;
        let vm = vm.remove_unnecessary_restore_value();
        let vm = vm.remove_unnecessary_save_value();
        let vm = vm.remap_temporary_indices();

        Ok(vm)
    }
}

impl SymbolicValue {
    fn as_op_index(self) -> Option<OpIndex> {
        match self {
            SymbolicValue::Result(op_index) => Some(op_index),
            _ => None,
        }
    }
}

impl SymbolicExpr {
    fn remap(
        self,
        current_index: OpIndex,
        map: &HashMap<OpIndex, SymbolicValue>,
    ) -> Result<Self, Error> {
        let remap =
            |prev_value: SymbolicValue| -> Result<SymbolicValue, Error> {
                match prev_value {
                    SymbolicValue::Result(prev_index) => Ok(map
                        .get(&prev_index)
                        .cloned()
                        .ok_or_else(|| Error::InvalidReference {
                            from: current_index,
                            to: prev_index,
                        })?
                        .into()),
                    other => Ok(other),
                }
            };

        let new_op = match self {
            static_field @ SymbolicExpr::StaticField(_) => static_field,
            SymbolicExpr::FieldAccess { obj, field } => {
                let obj = remap(obj)?;
                SymbolicExpr::FieldAccess { obj, field }
            }
            SymbolicExpr::IndexAccess { obj, indices } => {
                let obj = remap(obj.into())?;
                let indices = indices
                    .into_iter()
                    .map(|index| remap(index.into()))
                    .collect::<Result<_, _>>()?;
                SymbolicExpr::IndexAccess { obj, indices }
            }
            SymbolicExpr::PrimCast { value, prim_type } => {
                let value = remap(value)?;
                SymbolicExpr::PrimCast { value, prim_type }
            }
            SymbolicExpr::SymbolicDowncast { obj, ty } => {
                let obj = remap(obj)?;
                SymbolicExpr::SymbolicDowncast { obj, ty }.into()
            }
            SymbolicExpr::NumArrayElements { array } => {
                let array = remap(array)?;
                SymbolicExpr::NumArrayElements { array }
            }
            SymbolicExpr::ArrayExtent { array, dim } => {
                let array = remap(array)?;
                let dim = remap(dim)?;
                SymbolicExpr::ArrayExtent { array, dim }
            }
            SymbolicExpr::Add { lhs, rhs } => {
                let lhs = remap(lhs)?;
                let rhs = remap(rhs)?;
                SymbolicExpr::Add { lhs, rhs }
            }
            SymbolicExpr::Mul { lhs, rhs } => {
                let lhs = remap(lhs)?;
                let rhs = remap(rhs)?;
                SymbolicExpr::Mul { lhs, rhs }
            }
            SymbolicExpr::PhysicalDowncast { obj, ty } => {
                let obj = remap(obj)?;
                SymbolicExpr::PhysicalDowncast { obj, ty }
            }
            SymbolicExpr::ReadValue { ptr, prim_type } => {
                let ptr = remap(ptr)?;
                SymbolicExpr::ReadValue { ptr, prim_type }
            }
        };

        Ok(new_op)
    }

    fn visit_input_values(&self, mut callback: impl FnMut(SymbolicValue)) {
        match self {
            SymbolicExpr::StaticField(_) => {}
            SymbolicExpr::FieldAccess { obj, .. } => {
                callback(*obj);
            }
            SymbolicExpr::IndexAccess { obj, indices } => {
                callback(*obj);
                indices.iter().for_each(|index| callback(*index));
            }
            SymbolicExpr::PrimCast { value, .. } => callback(*value),
            SymbolicExpr::SymbolicDowncast { obj, .. } => callback(*obj),
            SymbolicExpr::NumArrayElements { array } => {
                callback(*array);
            }
            SymbolicExpr::ArrayExtent { array, dim } => {
                callback(*array);
                callback(*dim);
            }
            SymbolicExpr::Add { lhs, rhs } | SymbolicExpr::Mul { lhs, rhs } => {
                callback(*lhs);
                callback(*rhs)
            }

            SymbolicExpr::PhysicalDowncast { obj, .. } => {
                callback(*obj);
            }
            SymbolicExpr::ReadValue { ptr, .. } => {
                callback(*ptr);
            }
        }
    }

    fn to_physical_expr(
        &self,
        reader: CachedReader<'_>,
        original: &SymbolicGraph,
        builder: &mut SymbolicGraph,
        symbolic_index: OpIndex,
        prev_value_lookup: &HashMap<OpIndex, SymbolicValue>,
        runtime_type_lookup: &HashMap<SymbolicValue, RuntimeType>,
    ) -> Result<SymbolicValue, Error> {
        let lookup_prev = |sym_value: &SymbolicValue| -> Result<_, Error> {
            let phys_value = match *sym_value {
                SymbolicValue::Result(old_index) => prev_value_lookup
                    .get(&old_index)
                    .ok_or_else(|| Error::InvalidReference {
                        from: symbolic_index,
                        to: old_index,
                    })?
                    .clone(),
                other => other,
            };
            let runtime_type = runtime_type_lookup
                .get(sym_value)
                .ok_or_else(|| Error::InferredTypeNotFound(*sym_value))?;

            Ok((phys_value, runtime_type))
        };

        macro_rules! read_value_if_required {
            ($ptr:expr, $runtime_type:expr) => {
                if let Some(prim_type) = $runtime_type.storage_type() {
                    // The majority of fields should be read out after
                    // their location has been determined.
                    builder.read_value($ptr, prim_type)
                } else {
                    // The exception are ValueType fields.  These
                    // require additional FieldAccess operations to
                    // locate the primitive types within the composite
                    // ValueType, and must be kept as a pointer until
                    // then.
                    $ptr
                }
            };
        }

        match self {
            SymbolicExpr::StaticField(static_field) => {
                let runtime_type = static_field.runtime_type(reader)?;
                let ptr = static_field.location(reader)?;

                let ptr = SymbolicValue::Ptr(ptr);
                let expr = read_value_if_required!(ptr, runtime_type);

                Ok(expr)
            }
            SymbolicExpr::FieldAccess { obj, field } => {
                let (ptr, obj_type) = lookup_prev(obj)?;

                let method_table_ptr =
                    obj_type.method_table_for_field_access(|| {
                        format!("{}", original.print(obj))
                    })?;
                let (parent_of_field, field_description) =
                    reader.find_field(method_table_ptr, field.as_str())?;
                let field_type = reader.field_to_runtime_type(
                    parent_of_field,
                    &field_description,
                )?;

                // The `field_description.offset()` is relative to the
                // location of the first data member, regardless of
                // whether the object is a Class or ValueType
                // instance.  However, Class instances have an
                // additional pointer to their method table, prior to
                // the first data member.
                let ptr = if matches!(obj_type, RuntimeType::Class { .. }) {
                    builder.add(ptr, Pointer::SIZE)
                } else {
                    ptr
                };

                let ptr = builder.add(ptr, field_description.offset());
                let value = read_value_if_required!(ptr, field_type);

                Ok(value)
            }
            SymbolicExpr::IndexAccess { obj, indices } => {
                let (array, array_type) = lookup_prev(obj)?;

                let indices = indices
                    .iter()
                    .map(|index| lookup_prev(index).map(|(value, _)| value))
                    .collect::<Result<Vec<_>, _>>()?;

                let (element_type, component_size) = match array_type {
                    RuntimeType::Array { method_table, .. }
                    | RuntimeType::MultiDimArray { method_table, .. } => {
                        let method_table = method_table.ok_or_else(|| {
                            Error::UnexpectedNullMethodTable(format!(
                                "{}",
                                original.print(obj)
                            ))
                        })?;
                        let method_table = reader.method_table(method_table)?;
                        let component_size = method_table
                            .component_size()
                            .ok_or(Error::ArrayMissingComponentSize)?;
                        let element_type = method_table
                            .array_element_type()
                            .ok_or(Error::ArrayMissingElementType)
                            .and_then(|ptr| reader.runtime_type(ptr))?;

                        Ok((element_type, component_size))
                    }
                    other => {
                        Err(Error::IndexAccessRequiresArray(other.clone()))
                    }
                }?;

                let (header_size_bytes, shape) = match array_type {
                    RuntimeType::Array { .. } => {
                        let header_size_bytes = RuntimeArray::HEADER_SIZE;
                        let num_elements_ptr =
                            builder.add(array, Pointer::SIZE);
                        let num_elements = builder
                            .read_value(num_elements_ptr, RuntimePrimType::U64);
                        let num_elements = builder.prim_cast(
                            num_elements,
                            RuntimePrimType::NativeUInt,
                        );
                        let shape = vec![num_elements];
                        Ok((header_size_bytes, shape))
                    }
                    RuntimeType::MultiDimArray { rank, .. } => {
                        let rank = *rank;

                        let shape_start =
                            builder.add(array, RuntimeArray::HEADER_SIZE);
                        let shape = (0..rank)
                            .map(|i| {
                                let extent_ptr = builder.add(
                                    shape_start,
                                    i * RuntimePrimType::U32.size_bytes(),
                                );
                                let extent = builder.read_value(
                                    extent_ptr,
                                    RuntimePrimType::U32,
                                );
                                builder.prim_cast(
                                    extent,
                                    RuntimePrimType::NativeUInt,
                                )
                            })
                            .collect();
                        let header_size_bytes =
                            RuntimeMultiDimArray::header_size(rank);

                        Ok((header_size_bytes, shape))
                    }
                    other => {
                        Err(Error::IndexAccessRequiresArray(other.clone()))
                    }
                }?;

                if shape.len() != indices.len() {
                    return Err(Error::IncorrectNumberOfIndices {
                        num_provided: indices.len(),
                        num_expected: shape.len(),
                    });
                }

                let ptr = {
                    let first_element = builder.add(array, header_size_bytes);
                    let byte_offset = {
                        let strides = {
                            let mut strides = Vec::new();
                            let mut cum_prod: SymbolicValue =
                                component_size.into();
                            for dim in shape.into_iter().rev() {
                                strides.push(cum_prod);
                                cum_prod = builder.mul(cum_prod, dim);
                            }
                            strides.reverse();
                            strides
                        };

                        let mut total_offset: SymbolicValue = 0.into();
                        for (stride, index) in
                            strides.into_iter().zip(indices.into_iter())
                        {
                            let axis_offset = builder.mul(stride, index);
                            total_offset =
                                builder.add(total_offset, axis_offset);
                        }
                        total_offset
                    };
                    builder.add(first_element, byte_offset)
                };

                let value = read_value_if_required!(ptr, element_type);

                Ok(value)
            }
            SymbolicExpr::SymbolicDowncast { obj, ty } => {
                let (obj, obj_type) = lookup_prev(obj)?;

                let static_method_table_ptr =
                    obj_type.method_table_for_downcast()?;
                let target_method_table_ptr = ty.method_table(reader)?;
                let is_valid_downcast = reader
                    .method_table(static_method_table_ptr)?
                    .is_interface()
                    || reader.is_base_of(
                        target_method_table_ptr,
                        static_method_table_ptr,
                    )?
                    || reader.is_base_of(
                        static_method_table_ptr,
                        target_method_table_ptr,
                    )?;
                if !is_valid_downcast {
                    // Types are in separate hierachies.  This
                    // downcast is illegal.
                    return Err(Error::DowncastRequiresRelatedClasses(
                        format!("{obj_type}"),
                        format!("{ty}"),
                    ));
                }

                let target_method_table_ptr = ty.method_table(reader)?;
                let expr =
                    builder.physical_downcast(obj, target_method_table_ptr);

                Ok(expr)
            }
            SymbolicExpr::PrimCast { value, prim_type } => {
                let (value, _) = lookup_prev(value)?;
                let expr = builder.prim_cast(value, *prim_type);

                Ok(expr)
            }
            SymbolicExpr::NumArrayElements { array } => {
                let (array, array_type) = lookup_prev(array)?;
                match array_type {
                    RuntimeType::Array { .. }
                    | RuntimeType::MultiDimArray { .. } => {
                        let num_elements_ptr =
                            builder.add(array, Pointer::SIZE);
                        let expr = builder
                            .read_value(num_elements_ptr, RuntimePrimType::U64);
                        let expr = builder
                            .prim_cast(expr, RuntimePrimType::NativeUInt);
                        Ok(expr)
                    }

                    other => {
                        Err(Error::ArrayLengthRequiresArray(other.clone()))
                    }
                }
            }
            SymbolicExpr::ArrayExtent { array, dim } => {
                let (array, array_type) = lookup_prev(array)?;
                let dim = lookup_prev(dim)?.0;

                match array_type {
                    RuntimeType::MultiDimArray { .. } => {
                        // TODO: Assert that `dim < rank`.  Will
                        // require implementing support for runtime
                        // assertions.
                        let dim_offset =
                            builder.mul(dim, RuntimePrimType::U32.size_bytes());
                        let offset =
                            builder.add(dim_offset, RuntimeArray::HEADER_SIZE);
                        let extent_ptr = builder.add(array, offset);
                        let extent = builder
                            .read_value(extent_ptr, RuntimePrimType::U32);
                        let extent = builder
                            .prim_cast(extent, RuntimePrimType::NativeUInt);
                        Ok(extent)
                    }
                    other => {
                        Err(Error::ArrayExtentRequiresMultiDimensionalArray(
                            other.clone(),
                        ))
                    }
                }
            }
            other @ (SymbolicExpr::Add { .. }
            | SymbolicExpr::Mul { .. }
            | SymbolicExpr::PhysicalDowncast { .. }
            | SymbolicExpr::ReadValue { .. }) => {
                Ok(builder.push(other.clone()))
            }
        }
    }
}

impl Display for SymbolicExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolicExpr::StaticField(expr) => write!(f, "{expr}"),
            SymbolicExpr::FieldAccess { obj, field } => {
                write!(f, "{obj}\u{200B}.{field}")
            }
            SymbolicExpr::IndexAccess { obj, indices } => {
                write!(f, "{obj}[\u{200B}")?;

                for (i, index) in indices.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{index}")?;
                }

                write!(f, "\u{200B}]")
            }
            SymbolicExpr::SymbolicDowncast { obj, ty } => {
                write!(f, "{obj}.as<\u{200B}{ty}\u{200B}>()")
            }
            SymbolicExpr::NumArrayElements { array } => {
                write!(f, "{array}\u{200B}.len()")
            }
            SymbolicExpr::ArrayExtent { array, dim } => {
                write!(f, "{array}\u{200B}.extent({dim})")
            }

            // TODO: Support Add/Mul/PhysicalDowncast/ReadValue in the
            // parser.
            SymbolicExpr::Add { lhs, rhs } => write!(f, "{lhs} + {rhs}"),
            SymbolicExpr::Mul { lhs, rhs } => write!(f, "{lhs}*{rhs}"),
            SymbolicExpr::PhysicalDowncast { obj, ty } => {
                write!(f, "{obj}.downcast({ty})")
            }
            SymbolicExpr::PrimCast { value, prim_type } => {
                write!(f, "{value}.as::<{prim_type}>()")
            }
            SymbolicExpr::ReadValue { ptr, prim_type } => {
                write!(f, "{ptr}.read::<{prim_type}>()")
            }
        }
    }
}

impl<'a> ExprPrinter<'a> {
    fn with_value(self, value: &'a SymbolicValue) -> ExprPrinter<'a> {
        ExprPrinter {
            graph: self.graph,
            value,
        }
    }
}

impl<'a> Display for ExprPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let expr = match self.value {
            SymbolicValue::Int(int) => {
                return write!(f, "{int}");
            }
            SymbolicValue::Ptr(ptr) => {
                return write!(f, "{ptr}");
            }
            SymbolicValue::Result(op_index) => &self.graph[*op_index],
        };

        match expr {
            SymbolicExpr::StaticField(static_field) => {
                write!(f, "{static_field}")
            }
            SymbolicExpr::FieldAccess { obj, field } => {
                let obj = self.with_value(obj);
                write!(f, "{obj}\u{200B}.{field}")
            }
            SymbolicExpr::IndexAccess { obj, indices } => {
                let obj = self.with_value(obj);
                write!(f, "{obj}[\u{200B}")?;

                for (i, index) in indices.iter().enumerate() {
                    let index = self.with_value(index);
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{index}")?;
                }

                write!(f, "\u{200B}]")
            }
            SymbolicExpr::SymbolicDowncast { obj, ty } => {
                let obj = self.with_value(obj);
                write!(f, "{obj}.as<\u{200B}{ty}\u{200B}>()")
            }
            SymbolicExpr::NumArrayElements { array } => {
                let array = self.with_value(array);
                write!(f, "{array}\u{200B}.len()")
            }
            SymbolicExpr::ArrayExtent { array, dim } => {
                let array = self.with_value(array);
                let dim = self.with_value(dim);
                write!(f, "{array}\u{200B}.extent({dim})")
            }
            SymbolicExpr::Add { lhs, rhs } => {
                let lhs = self.with_value(lhs);
                let rhs = self.with_value(rhs);
                write!(f, "{lhs} + {rhs}")
            }
            SymbolicExpr::Mul { lhs, rhs } => {
                let lhs = self.with_value(lhs);
                let rhs = self.with_value(rhs);
                write!(f, "{lhs}*{rhs}")
            }
            SymbolicExpr::PhysicalDowncast { obj, ty } => {
                let obj = self.with_value(obj);
                write!(f, "{obj}.downcast({ty})")
            }
            SymbolicExpr::PrimCast { value, prim_type } => {
                let value = self.with_value(value);
                write!(f, "{value}.as::<{prim_type}>()")
            }
            SymbolicExpr::ReadValue { ptr, prim_type } => {
                let ptr = self.with_value(ptr);
                write!(f, "{ptr}.read::<{prim_type}>()")
            }
        }
    }
}

impl<T> From<T> for SymbolicType
where
    T: Into<String>,
{
    fn from(full_name: T) -> Self {
        Self {
            full_name: full_name.into(),
            generics: Vec::new(),
        }
    }
}

impl Display for SymbolicValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SymbolicValue::Int(value) => write!(f, "{value}"),
            SymbolicValue::Ptr(ptr) => write!(f, "{ptr}"),
            SymbolicValue::Result(op_index) => write!(f, "{op_index}"),
        }
    }
}

impl Display for SymbolicType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.full_name)?;

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

impl From<StaticField> for SymbolicExpr {
    fn from(static_field: StaticField) -> Self {
        SymbolicExpr::StaticField(static_field)
    }
}

impl Display for StaticField {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}\u{200B}.{}", self.class, self.field_name)
    }
}

impl Display for SymbolicGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output_lookup: HashMap<_, _> = self
            .outputs
            .iter()
            .cloned()
            .enumerate()
            .filter_map(|(i, value)| {
                value.as_op_index().map(|op_index| (op_index, i))
            })
            .collect();

        for (index, op) in self.iter_ops() {
            write!(f, "{index} ")?;
            if let Some(i) = output_lookup.get(&index) {
                write!(f, "(output #{i}) ")?;
            }
            writeln!(f, "<- {op}")?;
        }
        Ok(())
    }
}

impl std::ops::Index<OpIndex> for SymbolicGraph {
    type Output = SymbolicExpr;

    fn index(&self, index: OpIndex) -> &Self::Output {
        &self.ops[index.0]
    }
}
