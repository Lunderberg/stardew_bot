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
    ops: Vec<Expr>,
    outputs: Vec<SymbolicValue>,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueToken(pub(crate) usize);

#[derive(Clone)]
pub struct Expr {
    pub(crate) kind: ExprKind,
    pub(crate) name: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
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

pub struct GraphPrinter<'a> {
    graph: &'a SymbolicGraph,

    /// If true, expand all expressions into non-nested expressions.
    /// If false, only expand expressions if necessary for unambiguous
    /// representation of the underlying expression.
    ///
    /// As an example of this ambiguity, consider the expression `(x +
    /// y) + (x + y)`.  Since `(x + y)` shows up multiple times, it
    /// could be evaluated either once or twice.  When using
    /// `GraphPrinter`, all subexpressions on the right-hand side of
    /// an assignment are distinct.
    ///
    /// Two evaluations, printed with `expand_all_expressions: true`
    ///     let _0 = x + y;
    ///     let _1 = x + y;
    ///     let _2 = _0 + _1;
    ///
    /// Two evaluations, printed with `expand_all_expressions: false`
    ///     let _2 = (x + y) + (x + y);
    ///
    /// One evaluation, printed with `expand_all_expressions: true`
    ///     let _0 = x + y;
    ///     let _1 = _0 + _0;
    ///
    /// One evaluation, printed with `expand_all_expressions: false`.
    /// The expression is still expanded into two separate
    /// assignments, to avoid ambiguity with the earlier case.
    ///     let _0 = x + y;
    ///     let _1 = _0 + _0;
    expand_all_expressions: bool,

    /// The subgraph to be printed.  If it contains a value, only
    /// nodes that are used by the specified value will be printed.
    /// Otherwise, all nodes will be printed.
    root_subgraph_node: Option<SymbolicValue>,

    /// If true, insert zero-width spaces at locations that would be
    /// convenient for line breaks to be inserted.  If false, do not
    /// insert the zero-width spaces.
    insert_zero_width_space_at_breakpoint: bool,
}

#[derive(Clone, Copy)]
pub struct ExprPrinter<'a> {
    graph: &'a SymbolicGraph,
    value: SymbolicValue,
    is_top_level: bool,
    inline_expr: &'a [bool],
    requires_name_prefix: &'a [bool],
    insert_zero_width_space_at_breakpoint: bool,
}

#[derive(Clone, Copy)]
struct TypePrinter<'a> {
    ty: &'a SymbolicType,
    insert_zero_width_space_at_breakpoint: bool,
}

#[derive(Clone, Copy)]
struct IndexPrinter<'a> {
    graph: &'a SymbolicGraph,
    index: OpIndex,
    requires_name_prefix: bool,
}

#[derive(Clone, Copy)]
enum DisplayInlineExpr<'a> {
    All,
    Some(&'a HashSet<OpIndex>),
    None,
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
            "ptr" | "Ptr" => Some(RuntimePrimType::Ptr),
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

pub trait Printable {
    fn top_level_value(self, graph: &SymbolicGraph) -> SymbolicValue;
}
impl Printable for ValueToken {
    fn top_level_value(self, graph: &SymbolicGraph) -> SymbolicValue {
        graph.outputs[self.0]
    }
}
impl Printable for SymbolicValue {
    fn top_level_value(self, _: &SymbolicGraph) -> SymbolicValue {
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

    pub fn push(&mut self, op: impl Into<Expr>) -> SymbolicValue {
        let op_index = OpIndex::new(self.ops.len());
        self.ops.push(op.into());
        op_index.into()
    }

    pub fn name(&mut self, value: SymbolicValue, name: impl Into<String>) {
        match value {
            SymbolicValue::Result(op_index) => {
                self.ops[op_index.0].name = Some(name.into());
            }
            SymbolicValue::Int(_) | SymbolicValue::Ptr(_) => {
                // Currently, constants are stored in-line at their
                // point-of-use, and can't be named.  If constants are
                // ever moved to be represented as their own nodes,
                // then they should be name-able.
            }
        }
    }

    pub fn mark_output(&mut self, value: SymbolicValue) -> ValueToken {
        let index = self.outputs.len();
        self.outputs.push(value);
        ValueToken(index)
    }

    pub fn printer<'a>(&'a self) -> GraphPrinter<'a> {
        GraphPrinter {
            graph: self,
            expand_all_expressions: false,
            root_subgraph_node: None,
            insert_zero_width_space_at_breakpoint: false,
        }
    }

    pub fn print<'a>(&'a self, value: impl Printable) -> GraphPrinter<'a> {
        let value = value.top_level_value(self);
        GraphPrinter {
            graph: self,
            root_subgraph_node: Some(value),
            expand_all_expressions: false,
            insert_zero_width_space_at_breakpoint: false,
        }
    }

    pub fn is_equivalent_to(&self, other: &Self) -> bool {
        self.ops.len() == other.ops.len()
            && self.outputs.len() == other.outputs.len()
            && self
                .ops
                .iter()
                .zip(other.ops.iter())
                .all(|(a, b)| a.kind == b.kind)
            && self
                .outputs
                .iter()
                .zip(other.outputs.iter())
                .all(|(a, b)| a == b)
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
            obj = self.push(ExprKind::FieldAccess {
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
        self.push(ExprKind::IndexAccess {
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
        self.push(ExprKind::IndexAccess { obj, indices })
    }

    pub fn downcast(
        &mut self,
        obj: impl Into<SymbolicValue>,
        ty: impl Into<SymbolicType>,
    ) -> SymbolicValue {
        let obj = obj.into();
        let ty = ty.into();
        self.push(ExprKind::SymbolicDowncast { obj, ty })
    }

    pub fn num_array_elements(
        &mut self,
        array: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let array = array.into();
        self.push(ExprKind::NumArrayElements { array })
    }

    pub fn array_extent(
        &mut self,
        array: impl Into<SymbolicValue>,
        dim: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let array = array.into();
        let dim = dim.into();
        self.push(ExprKind::ArrayExtent { array, dim })
    }

    pub fn pointer_cast(
        &mut self,
        ptr: impl Into<SymbolicValue>,
        ty: RuntimeType,
    ) -> SymbolicValue {
        let ptr = ptr.into();
        self.push(ExprKind::PointerCast { ptr, ty })
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
        self.push(ExprKind::Add { lhs, rhs })
    }

    pub fn mul(
        &mut self,
        lhs: impl Into<SymbolicValue>,
        rhs: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let lhs = lhs.into();
        let rhs = rhs.into();
        self.push(ExprKind::Mul { lhs, rhs })
    }

    pub fn prim_cast(
        &mut self,
        value: impl Into<SymbolicValue>,
        prim_type: RuntimePrimType,
    ) -> SymbolicValue {
        let value = value.into();
        self.push(ExprKind::PrimCast { value, prim_type })
    }

    pub fn physical_downcast(
        &mut self,
        obj: impl Into<SymbolicValue>,
        ty: TypedPointer<MethodTable>,
    ) -> SymbolicValue {
        let obj = obj.into();
        self.push(ExprKind::PhysicalDowncast { obj, ty })
    }

    pub fn read_value(
        &mut self,
        ptr: impl Into<SymbolicValue>,
        prim_type: RuntimePrimType,
    ) -> SymbolicValue {
        let ptr = ptr.into();
        self.push(ExprKind::ReadValue { ptr, prim_type })
    }

    //////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////

    fn iter_ops(&self) -> impl Iterator<Item = (OpIndex, &Expr)> + '_ {
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
            let opt_remapped = op.kind.try_remap(&prev_index_lookup);
            let kind = opt_remapped.as_ref().unwrap_or(&op.kind);

            let value = rewriter
                .rewrite_expr(&mut builder, kind)?
                .unwrap_or_else(|| {
                    let expr: Expr = if let Some(remapped) = opt_remapped {
                        remapped.into()
                    } else {
                        op.clone()
                    };
                    builder.push(expr)
                });
            prev_index_lookup.insert(old_index, value);
        }

        builder.mark_new_outputs(&self.outputs, &prev_index_lookup);

        Ok(builder)
    }

    fn reachable(
        &self,
        initial: impl IntoIterator<Item = SymbolicValue>,
    ) -> Vec<bool> {
        let mut to_visit: Vec<_> = initial
            .into_iter()
            .filter_map(|value| value.as_op_index())
            .collect();

        let mut reachable = vec![false; self.ops.len()];
        for index in &to_visit {
            reachable[index.0] = true;
        }

        while let Some(visiting) = to_visit.pop() {
            self[visiting].visit_input_values(|upstream| {
                if let SymbolicValue::Result(upstream) = upstream {
                    if !reachable[upstream.0] {
                        reachable[upstream.0] = true;
                        to_visit.push(upstream);
                    }
                }
            })
        }

        reachable
    }

    pub fn dead_code_elimination(self) -> Result<Self, Error> {
        let mut prev_index_lookup: HashMap<OpIndex, SymbolicValue> =
            HashMap::new();
        let mut builder = Self::new();

        let reachable = self.reachable(self.outputs.iter().cloned());

        for (prev_index, op) in self.iter_ops() {
            if reachable[prev_index.0] {
                let kind = op
                    .kind
                    .try_remap(&prev_index_lookup)
                    .unwrap_or_else(|| op.kind.clone());
                let new_index = builder.push(kind);
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
        let mut dedup_lookup: HashMap<ExprKind, SymbolicValue> = HashMap::new();

        for (prev_index, op) in self.iter_ops() {
            let new_kind = op
                .kind
                .try_remap(&prev_index_lookup)
                .unwrap_or_else(|| op.kind.clone());

            let new_index = if let Some(new_index) = dedup_lookup.get(&new_kind)
            {
                *new_index
            } else {
                let new_index = builder.push(new_kind.clone());
                dedup_lookup.insert(new_kind, new_index);
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
            match op.as_ref() {
                ExprKind::Add { lhs, rhs } => {
                    instructions.push(value_to_register!(lhs));
                    instructions.push(Instruction::Add(value_to_arg!(rhs)));
                }
                ExprKind::Mul { lhs, rhs } => {
                    instructions.push(value_to_register!(lhs));
                    instructions.push(Instruction::Mul(value_to_arg!(rhs)));
                }
                ExprKind::PrimCast { value, prim_type } => {
                    instructions.push(value_to_register!(value));
                    instructions.push(Instruction::PrimCast(*prim_type));
                }
                ExprKind::PhysicalDowncast { obj, ty } => {
                    instructions.push(value_to_register!(obj));
                    instructions.push(Instruction::Downcast { ty: *ty });
                }
                ExprKind::ReadValue { ptr, prim_type } => {
                    instructions.push(value_to_register!(ptr));
                    instructions.push(Instruction::Read { ty: *prim_type });
                }
                ExprKind::PointerCast { ptr, .. } => {
                    instructions.push(value_to_register!(ptr));
                }
                symbolic @ (ExprKind::StaticField(_)
                | ExprKind::FieldAccess { .. }
                | ExprKind::SymbolicDowncast { .. }
                | ExprKind::IndexAccess { .. }
                | ExprKind::NumArrayElements { .. }
                | ExprKind::ArrayExtent { .. }) => {
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

impl Expr {
    pub(crate) fn visit_input_values(
        &self,
        callback: impl FnMut(SymbolicValue),
    ) {
        self.kind.visit_input_values(callback)
    }
}

impl ExprKind {
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
            ExprKind::StaticField(_) => None,
            ExprKind::FieldAccess { obj, field } => {
                remap(obj).map(|obj| ExprKind::FieldAccess {
                    obj,
                    field: field.clone(),
                })
            }
            ExprKind::IndexAccess { obj, indices } => {
                let opt_obj = remap(obj);
                let requires_remap = opt_obj.is_some()
                    || indices.iter().any(|index| remap(index).is_some());
                requires_remap.then(|| {
                    let obj = opt_obj.unwrap_or_else(|| *obj);
                    let indices = indices
                        .iter()
                        .map(|index| remap(index).unwrap_or_else(|| *index))
                        .collect::<Vec<_>>();
                    ExprKind::IndexAccess { obj, indices }
                })
            }
            ExprKind::PrimCast { value, prim_type } => {
                remap(value).map(|value| ExprKind::PrimCast {
                    value,
                    prim_type: prim_type.clone(),
                })
            }
            ExprKind::SymbolicDowncast { obj, ty } => {
                remap(obj).map(|obj| ExprKind::SymbolicDowncast {
                    obj,
                    ty: ty.clone(),
                })
            }
            ExprKind::NumArrayElements { array } => {
                remap(array).map(|array| ExprKind::NumArrayElements { array })
            }
            ExprKind::ArrayExtent { array, dim } => {
                let opt_array = remap(array);
                let opt_dim = remap(dim);
                let requires_remap = opt_array.is_some() || opt_dim.is_some();
                (requires_remap).then(|| {
                    let array = opt_array.unwrap_or_else(|| *array);
                    let dim = opt_array.unwrap_or_else(|| *dim);
                    ExprKind::ArrayExtent { array, dim }
                })
            }
            ExprKind::PointerCast { ptr, ty } => {
                remap(ptr).map(|ptr| ExprKind::PointerCast {
                    ptr,
                    ty: ty.clone(),
                })
            }
            ExprKind::Add { lhs, rhs } => {
                let opt_lhs = remap(lhs);
                let opt_rhs = remap(rhs);
                let requires_remap = opt_lhs.is_some() || opt_rhs.is_some();
                requires_remap.then(|| {
                    let lhs = opt_lhs.unwrap_or_else(|| *lhs);
                    let rhs = opt_rhs.unwrap_or_else(|| *rhs);
                    ExprKind::Add { lhs, rhs }
                })
            }
            ExprKind::Mul { lhs, rhs } => {
                let opt_lhs = remap(lhs);
                let opt_rhs = remap(rhs);
                let requires_remap = opt_lhs.is_some() || opt_rhs.is_some();
                requires_remap.then(|| {
                    let lhs = opt_lhs.unwrap_or_else(|| *lhs);
                    let rhs = opt_rhs.unwrap_or_else(|| *rhs);
                    ExprKind::Mul { lhs, rhs }
                })
            }
            ExprKind::PhysicalDowncast { obj, ty } => {
                remap(obj).map(|obj| ExprKind::PhysicalDowncast {
                    obj,
                    ty: ty.clone(),
                })
            }
            ExprKind::ReadValue { ptr, prim_type } => {
                remap(ptr).map(|ptr| ExprKind::ReadValue {
                    ptr,
                    prim_type: prim_type.clone(),
                })
            }
        }
    }

    pub(crate) fn visit_input_values(
        &self,
        mut callback: impl FnMut(SymbolicValue),
    ) {
        match self {
            ExprKind::StaticField(_) => {}
            ExprKind::FieldAccess { obj, .. } => {
                callback(*obj);
            }
            ExprKind::IndexAccess { obj, indices } => {
                callback(*obj);
                indices.iter().for_each(|index| callback(*index));
            }
            ExprKind::PrimCast { value, .. } => callback(*value),
            ExprKind::SymbolicDowncast { obj, .. } => callback(*obj),
            ExprKind::NumArrayElements { array } => {
                callback(*array);
            }
            ExprKind::ArrayExtent { array, dim } => {
                callback(*array);
                callback(*dim);
            }
            ExprKind::PointerCast { ptr, .. } => callback(*ptr),
            ExprKind::Add { lhs, rhs } | ExprKind::Mul { lhs, rhs } => {
                callback(*lhs);
                callback(*rhs)
            }

            ExprKind::PhysicalDowncast { obj, .. } => {
                callback(*obj);
            }
            ExprKind::ReadValue { ptr, .. } => {
                callback(*ptr);
            }
        }
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::StaticField(StaticField { class, field_name }) => {
                write!(f, "{class}\u{200B}.{field_name}")
            }
            ExprKind::FieldAccess { obj, field } => {
                write!(f, "{obj}\u{200B}.{field}")
            }
            ExprKind::IndexAccess { obj, indices } => {
                write!(f, "{obj}[\u{200B}")?;

                for (i, index) in indices.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{index}")?;
                }

                write!(f, "\u{200B}]")
            }
            ExprKind::SymbolicDowncast { obj, ty } => {
                write!(f, "{obj}.as::<\u{200B}{ty}\u{200B}>()")
            }
            ExprKind::NumArrayElements { array } => {
                write!(f, "{array}\u{200B}.len()")
            }
            ExprKind::ArrayExtent { array, dim } => {
                write!(f, "{array}\u{200B}.extent({dim})")
            }

            ExprKind::PointerCast { ptr, ty } => {
                write!(f, "{ptr}\u{200B}.ptr_cast::<{ty}>()")
            }

            // TODO: Support Add/Mul/PhysicalDowncast/ReadValue in the
            // parser.
            ExprKind::Add { lhs, rhs } => write!(f, "{lhs} + {rhs}"),
            ExprKind::Mul { lhs, rhs } => write!(f, "{lhs}*{rhs}"),
            ExprKind::PhysicalDowncast { obj, ty } => {
                write!(f, "{obj}.downcast({ty})")
            }
            ExprKind::PrimCast { value, prim_type } => {
                write!(f, "{value}.prim_cast::<{prim_type}>()")
            }
            ExprKind::ReadValue { ptr, prim_type } => {
                write!(f, "{ptr}.read::<{prim_type}>()")
            }
        }
    }
}

impl<'a> GraphPrinter<'a> {
    pub fn expand_all_expressions(self) -> Self {
        Self {
            expand_all_expressions: true,
            ..self
        }
    }

    pub fn insert_zero_width_space_at_breakpoint(self) -> Self {
        Self {
            insert_zero_width_space_at_breakpoint: true,
            ..self
        }
    }

    fn display(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let output_lookup: HashMap<_, _> = self
            .graph
            .outputs
            .iter()
            .cloned()
            .enumerate()
            .filter_map(|(i, value)| {
                value.as_op_index().map(|op_index| (op_index, i))
            })
            .collect();

        let reachable = if let Some(root_node) = self.root_subgraph_node {
            self.graph.reachable(std::iter::once(root_node))
        } else {
            vec![true; self.graph.ops.len()]
        };

        let inline_expr: Vec<bool> = if self.expand_all_expressions {
            vec![false; self.graph.ops.len()]
        } else {
            let mut num_usage = vec![0; self.graph.ops.len()];
            self.graph
                .iter_ops()
                .filter(|(index, _)| reachable[index.0])
                .for_each(|(_, op)| {
                    op.visit_input_values(|input_value| {
                        if let SymbolicValue::Result(prev_index) = input_value {
                            num_usage[prev_index.0] += 1;
                        }
                    })
                });
            num_usage
                .into_iter()
                .zip(self.graph.ops.iter())
                .map(|(count, op)| count == 1 && op.name.is_none())
                .collect()
        };

        let requires_name_prefix = {
            let mut requires_name_prefix = vec![false; self.graph.ops.len()];
            let mut name_lookup: HashMap<&str, OpIndex> = HashMap::new();
            for (index, op) in self.graph.iter_ops() {
                if let Some(name) = op.name.as_ref().map(|name| name.as_str()) {
                    if let Some(prev_index) = name_lookup.get(name) {
                        requires_name_prefix[index.0] = true;
                        requires_name_prefix[prev_index.0] = true;
                    } else {
                        name_lookup.insert(name, index);
                    }
                }
            }

            requires_name_prefix
        };

        let make_expr_printer = |value: SymbolicValue| ExprPrinter {
            graph: self.graph,
            value,
            is_top_level: true,
            inline_expr: &inline_expr,
            requires_name_prefix: &requires_name_prefix,
            insert_zero_width_space_at_breakpoint: self
                .insert_zero_width_space_at_breakpoint,
        };

        for (index, _op) in self.graph.iter_ops() {
            if reachable[index.0]
                && !inline_expr[index.0]
                && self.root_subgraph_node != Some(SymbolicValue::Result(index))
            {
                let index_printer = IndexPrinter {
                    graph: self.graph,
                    index,
                    requires_name_prefix: requires_name_prefix[index.0],
                };
                let expr_printer = make_expr_printer(index.into());

                write!(fmt, "let {index_printer} ")?;
                if let Some(i) = output_lookup.get(&index) {
                    write!(fmt, "(output #{i}) ")?;
                }
                writeln!(fmt, "= {expr_printer};")?;
            }
        }

        if let Some(root_node) = self.root_subgraph_node {
            let expr_printer = make_expr_printer(root_node);
            write!(fmt, "{expr_printer}")?;
        }

        Ok(())
    }
}

impl<'a> Display for GraphPrinter<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(fmt)
    }
}

impl<'a> ExprPrinter<'a> {
    fn with_value(self, value: SymbolicValue) -> Self {
        Self {
            value,
            is_top_level: false,
            ..self
        }
    }
}

struct MaybeZeroWidthSpace(bool);
impl Display for MaybeZeroWidthSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 {
            write!(f, "\u{200B}")
        } else {
            Ok(())
        }
    }
}

impl<'a> Display for IndexPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let index = self.index.0;
        if let Some(name) = self.graph[self.index].name.as_ref() {
            if self.requires_name_prefix {
                write!(f, "_{index}_{name}")
            } else {
                write!(f, "{name}")
            }
        } else {
            write!(f, "_{index}")
        }
    }
}

impl<'a> Display for ExprPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_index = match self.value {
            SymbolicValue::Int(int) => {
                return write!(f, "{int}");
            }
            SymbolicValue::Ptr(ptr) => {
                return write!(f, "{ptr}");
            }
            SymbolicValue::Result(op_index) => op_index,
        };

        let display_full_expr =
            self.is_top_level || self.inline_expr[op_index.0];
        if !display_full_expr {
            let index = IndexPrinter {
                graph: self.graph,
                index: op_index,
                requires_name_prefix: self.requires_name_prefix[op_index.0],
            };
            return write!(f, "{index}");
        }

        let sep =
            MaybeZeroWidthSpace(self.insert_zero_width_space_at_breakpoint);

        match self.graph[op_index].as_ref() {
            ExprKind::StaticField(StaticField { class, field_name }) => {
                write!(f, "{class}{sep}.{field_name}")
            }
            ExprKind::FieldAccess { obj, field } => {
                let obj = self.with_value(*obj);
                write!(f, "{obj}{sep}.{field}")
            }
            ExprKind::IndexAccess { obj, indices } => {
                let obj = self.with_value(*obj);
                write!(f, "{obj}[{sep}")?;

                for (i, index) in indices.iter().enumerate() {
                    let index = self.with_value(*index);
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{index}")?;
                }

                write!(f, "{sep}]")
            }
            ExprKind::SymbolicDowncast { obj, ty } => {
                let obj = self.with_value(*obj);
                let ty = TypePrinter {
                    ty,
                    insert_zero_width_space_at_breakpoint: self
                        .insert_zero_width_space_at_breakpoint,
                };
                write!(f, "{obj}.as::<{sep}{ty}{sep}>()")
            }
            ExprKind::NumArrayElements { array } => {
                let array = self.with_value(*array);
                write!(f, "{array}{sep}.len()")
            }
            ExprKind::ArrayExtent { array, dim } => {
                let array = self.with_value(*array);
                let dim = self.with_value(*dim);
                write!(f, "{array}{sep}.extent({dim})")
            }
            ExprKind::PointerCast { ptr, ty } => {
                let ptr = self.with_value(*ptr);
                write!(f, "{ptr}{sep}.ptr_cast::<{ty}>()")
            }
            ExprKind::Add { lhs, rhs } => {
                let lhs = self.with_value(*lhs);
                let rhs = self.with_value(*rhs);
                write!(f, "{lhs} + {rhs}")
            }
            ExprKind::Mul { lhs, rhs } => {
                let lhs = self.with_value(*lhs);
                let rhs = self.with_value(*rhs);
                write!(f, "{lhs}*{rhs}")
            }
            ExprKind::PhysicalDowncast { obj, ty } => {
                let obj = self.with_value(*obj);
                write!(f, "{obj}.downcast::<{ty}>()")
            }
            ExprKind::PrimCast { value, prim_type } => {
                let value = self.with_value(*value);
                write!(f, "{value}.prim_cast::<{prim_type}>()")
            }
            ExprKind::ReadValue { ptr, prim_type } => {
                let ptr = self.with_value(*ptr);
                write!(f, "{ptr}.read::<{prim_type}>()")
            }
        }
    }
}

impl From<ExprKind> for Expr {
    fn from(kind: ExprKind) -> Self {
        Expr { kind, name: None }
    }
}

impl From<StaticField> for Expr {
    fn from(value: StaticField) -> Self {
        let kind: ExprKind = value.into();
        kind.into()
    }
}

impl AsRef<ExprKind> for Expr {
    fn as_ref(&self) -> &ExprKind {
        &self.kind
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

impl<'a> Display for TypePrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let SymbolicType {
            full_name,
            generics,
        } = &self.ty;
        let sep =
            MaybeZeroWidthSpace(self.insert_zero_width_space_at_breakpoint);

        write!(f, "{full_name}")?;

        if !generics.is_empty() {
            write!(f, "<{sep}")?;
            for (i, generic) in generics.iter().enumerate() {
                if i > 0 {
                    write!(f, ", {sep}")?;
                }
                write!(f, "{generic}")?;
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}

impl Display for SymbolicType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let printer = TypePrinter {
            ty: self,
            insert_zero_width_space_at_breakpoint: false,
        };
        write!(f, "{printer}")
    }
}

impl From<StaticField> for ExprKind {
    fn from(static_field: StaticField) -> Self {
        ExprKind::StaticField(static_field)
    }
}

impl Display for SymbolicGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.printer())
    }
}

impl std::ops::Index<OpIndex> for SymbolicGraph {
    type Output = Expr;

    fn index(&self, index: OpIndex) -> &Self::Output {
        &self.ops[index.0]
    }
}
