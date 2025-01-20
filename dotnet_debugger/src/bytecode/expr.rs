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
    CachedReader, Error, FieldDescription, MethodTable, OpIndex,
    RuntimePrimValue, RuntimeType, TypedPointer, VirtualMachine,
};

use super::{graph_rewrite::Analysis, GraphRewrite, TypeInference};

#[derive(Default, Clone)]
pub struct SymbolicGraph {
    ops: Vec<SymbolicExpr>,
    outputs: Vec<SymbolicValue>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueToken(pub(crate) usize);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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

    /// Cast a pointer to another pointer type.
    PointerCast { ptr: SymbolicValue, ty: RuntimeType },

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

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, From)]
pub enum SymbolicValue {
    Int(usize),
    Ptr(Pointer),
    Result(OpIndex),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct SymbolicType {
    /// The full name of the type, including the namespace, if any.
    pub full_name: String,
    pub generics: Vec<SymbolicType>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    pub(crate) fn method_table<'a>(
        &self,
        reader: impl Into<CachedReader<'a>>,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        let reader = reader.into();

        let method_table_ptr = reader
            .method_table_by_name(&self.full_name)?
            .ok_or_else(|| {
                Error::UnexpectedNullMethodTable(format!("{}", self))
            })
            .unwrap()
            //?
            ;

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

    pub(crate) fn runtime_type(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<RuntimeType, Error> {
        let (base_method_table_ptr, field) =
            self.method_table_and_field(reader)?;

        let base_type =
            reader.field_to_runtime_type(base_method_table_ptr, &field)?;

        Ok(base_type)
    }

    pub(crate) fn location(
        &self,
        reader: CachedReader<'_>,
    ) -> Result<Pointer, Error> {
        let (base_method_table_ptr, field) =
            self.method_table_and_field(reader)?;

        let method_table = reader.method_table(base_method_table_ptr)?;
        let module = reader.runtime_module(method_table.module())?;
        let location =
            field.location(module, crate::FieldContainer::Static, &reader)?;

        Ok(location)
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

    pub fn parse(&mut self, text: &str) -> Result<SymbolicValue, Error> {
        let mut parser = super::SymbolicParser::new(text, self);
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

    pub fn pointer_cast(
        &mut self,
        ptr: impl Into<SymbolicValue>,
        ty: RuntimeType,
    ) -> SymbolicValue {
        let ptr = ptr.into();
        self.push(SymbolicExpr::PointerCast { ptr, ty })
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

    //////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////

    pub fn validate(&self, reader: CachedReader<'_>) -> Result<(), Error> {
        let type_inference = TypeInference::new(reader);

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

            type_inference.infer_type(self, index.into())?;
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

    pub fn simplify(&self, reader: CachedReader<'_>) -> Result<Self, Error> {
        let analysis = Analysis::new(reader);
        let rewriter = super::RemoveUnusedDowncast(&analysis)
            .then(super::ConstantFold)
            .then(super::RemoveUnusedPrimcast(&analysis))
            .apply_recursively();
        self.rewrite(rewriter)
    }

    pub fn rewrite(&self, rewriter: impl GraphRewrite) -> Result<Self, Error> {
        let mut prev_index_lookup: HashMap<OpIndex, SymbolicValue> =
            HashMap::new();
        let mut builder = Self::new();

        for (old_index, op) in self.iter_ops() {
            let op = op.clone().remap(old_index, &prev_index_lookup)?;

            let value = rewriter
                .rewrite_expr(&mut builder, &op)?
                .unwrap_or_else(|| builder.push(op));
            prev_index_lookup.insert(old_index, value);
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
            ($arg:expr) => {{
                let vm_arg: VMArg = match $arg {
                    &SymbolicValue::Int(value) => {
                        RuntimePrimValue::NativeUInt(value).into()
                    }

                    &SymbolicValue::Ptr(ptr) => {
                        RuntimePrimValue::Ptr(ptr).into()
                    }

                    SymbolicValue::Result(op_index) => {
                        let index = *currently_stored.get(&op_index).expect(
                            "Internal error, \
                             {op_index} not located anywhere",
                        );
                        VMArg::SavedValue { index }
                    }
                };
                Instruction::LoadToRegister(vm_arg)
            }};
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
                SymbolicExpr::PhysicalDowncast { obj, ty } => {
                    instructions.push(value_to_register!(obj));
                    instructions.push(Instruction::Downcast { ty: *ty });
                }
                SymbolicExpr::ReadValue { ptr, prim_type } => {
                    instructions.push(value_to_register!(ptr));
                    instructions.push(Instruction::Read { ty: *prim_type });
                }
                SymbolicExpr::PointerCast { ptr, .. } => {
                    instructions.push(value_to_register!(ptr));
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
                instructions.push(Instruction::LoadToRegister(value.into()));
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

        // The display of static fields is done similar to C#, which
        // uses the same syntax for specifying static fields, and for
        // specifying namespaces.
        //
        // For example, `A.B.C.D` could be class `B` within namespace
        // `A`, which has a static field `C`, and a subfield `D`.
        // Alternatively, it could be class `C` within namespace
        // `A.B`, which has a static field `C`.
        //
        // Within the CLR, this ambiguity is handled by explicitly
        // specifying both the name and the namespace as separate
        // fields, but the printed-out format (and C# itself) is
        // ambiguous without additional information.  Here, it is
        // handled while lowering, by identifying how many elements
        // are required to produce some "namespace.name" of a valid
        // class.
        let expr = expr.rewrite(
            super::IdentifyStaticField(&Analysis::new(reader))
                .apply_recursively(),
        )?;

        let expr = expr.dead_code_elimination()?;
        expr.validate(reader)?;
        let expr = expr.eliminate_common_subexpresssions()?;
        expr.validate(reader)?;

        let analysis = Analysis::new(reader);
        let rewriter = super::ConstantFold
            .then(super::RemoveUnusedDowncast(&analysis))
            .then(super::RemoveUnusedPrimcast(&analysis))
            .then(super::LowerSymbolicExpr(&analysis))
            .then(super::RemoveUnusedPointerCast)
            .apply_recursively();

        let expr = expr.rewrite(rewriter)?;
        expr.validate(reader)?;

        let expr = expr.dead_code_elimination()?;
        expr.validate(reader)?;
        let expr = expr.eliminate_common_subexpresssions()?;
        expr.validate(reader)?;

        // Virtual machine, in terms of sequential operations.
        let vm = expr.to_virtual_machine()?;
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
    pub(crate) fn try_remap(
        &self,
        map: &HashMap<OpIndex, SymbolicValue>,
    ) -> Option<Self> {
        let remap = |value: &SymbolicValue| -> Option<SymbolicValue> {
            match value {
                SymbolicValue::Result(index) => map.get(&index).cloned(),
                _ => None,
            }
        };

        match self {
            SymbolicExpr::StaticField(_) => None,
            SymbolicExpr::FieldAccess { obj, field } => {
                remap(obj).map(|obj| SymbolicExpr::FieldAccess {
                    obj,
                    field: field.clone(),
                })
            }
            SymbolicExpr::IndexAccess { obj, indices } => {
                let opt_obj = remap(obj);
                let requires_remap = opt_obj.is_some()
                    || indices.iter().any(|index| remap(index).is_some());
                requires_remap.then(|| {
                    let obj = opt_obj.unwrap_or_else(|| *obj);
                    let indices = indices
                        .iter()
                        .map(|index| remap(index).unwrap_or_else(|| *index))
                        .collect::<Vec<_>>();
                    SymbolicExpr::IndexAccess { obj, indices }
                })
            }
            SymbolicExpr::PrimCast { value, prim_type } => {
                remap(value).map(|value| SymbolicExpr::PrimCast {
                    value,
                    prim_type: prim_type.clone(),
                })
            }
            SymbolicExpr::SymbolicDowncast { obj, ty } => {
                remap(obj).map(|obj| SymbolicExpr::SymbolicDowncast {
                    obj,
                    ty: ty.clone(),
                })
            }
            SymbolicExpr::NumArrayElements { array } => remap(array)
                .map(|array| SymbolicExpr::NumArrayElements { array }),
            SymbolicExpr::ArrayExtent { array, dim } => {
                let opt_array = remap(array);
                let opt_dim = remap(dim);
                let requires_remap = opt_array.is_some() || opt_dim.is_some();
                (requires_remap).then(|| {
                    let array = opt_array.unwrap_or_else(|| *array);
                    let dim = opt_array.unwrap_or_else(|| *dim);
                    SymbolicExpr::ArrayExtent { array, dim }
                })
            }
            SymbolicExpr::PointerCast { ptr, ty } => {
                remap(ptr).map(|ptr| SymbolicExpr::PointerCast {
                    ptr,
                    ty: ty.clone(),
                })
            }
            SymbolicExpr::Add { lhs, rhs } => {
                let opt_lhs = remap(lhs);
                let opt_rhs = remap(rhs);
                let requires_remap = opt_lhs.is_some() || opt_rhs.is_some();
                requires_remap.then(|| {
                    let lhs = opt_lhs.unwrap_or_else(|| *lhs);
                    let rhs = opt_rhs.unwrap_or_else(|| *rhs);
                    SymbolicExpr::Add { lhs, rhs }
                })
            }
            SymbolicExpr::Mul { lhs, rhs } => {
                let opt_lhs = remap(lhs);
                let opt_rhs = remap(rhs);
                let requires_remap = opt_lhs.is_some() || opt_rhs.is_some();
                requires_remap.then(|| {
                    let lhs = opt_lhs.unwrap_or_else(|| *lhs);
                    let rhs = opt_rhs.unwrap_or_else(|| *rhs);
                    SymbolicExpr::Mul { lhs, rhs }
                })
            }
            SymbolicExpr::PhysicalDowncast { obj, ty } => {
                remap(obj).map(|obj| SymbolicExpr::PhysicalDowncast {
                    obj,
                    ty: ty.clone(),
                })
            }
            SymbolicExpr::ReadValue { ptr, prim_type } => {
                remap(ptr).map(|ptr| SymbolicExpr::ReadValue {
                    ptr,
                    prim_type: prim_type.clone(),
                })
            }
        }
    }

    pub(crate) fn remap(
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
            SymbolicExpr::PointerCast { ptr, ty } => {
                let ptr = remap(ptr)?;
                SymbolicExpr::PointerCast { ptr, ty }
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

    pub(crate) fn visit_input_values(
        &self,
        mut callback: impl FnMut(SymbolicValue),
    ) {
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
            SymbolicExpr::PointerCast { ptr, .. } => callback(*ptr),
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

            SymbolicExpr::PointerCast { ptr, ty } => {
                write!(f, "{ptr}\u{200B}.ptr_cast({ty})")
            }

            // TODO: Support Add/Mul/PhysicalDowncast/ReadValue in the
            // parser.
            SymbolicExpr::Add { lhs, rhs } => write!(f, "{lhs} + {rhs}"),
            SymbolicExpr::Mul { lhs, rhs } => write!(f, "{lhs}*{rhs}"),
            SymbolicExpr::PhysicalDowncast { obj, ty } => {
                write!(f, "{obj}.downcast({ty})")
            }
            SymbolicExpr::PrimCast { value, prim_type } => {
                write!(f, "{value}.prim_cast::<{prim_type}>()")
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
            SymbolicExpr::PointerCast { ptr, ty } => {
                let ptr = self.with_value(ptr);
                write!(f, "{ptr}\u{200B}.ptr_cast<{ty}>()")
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
                write!(f, "{value}.prim_cast::<{prim_type}>()")
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
