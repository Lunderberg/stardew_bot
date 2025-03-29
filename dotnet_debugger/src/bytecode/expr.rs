use std::{
    cmp::Reverse,
    collections::{BinaryHeap, HashMap, HashSet},
    fmt::Display,
};

use derive_more::derive::From;
use itertools::Itertools as _;

use iterator_extensions::ResultIteratorExt as _;
use memory_reader::Pointer;

use crate::{
    bytecode::virtual_machine::{
        FunctionIndex, Instruction, StackIndex, VMArg,
    },
    runtime_type::{FunctionType, RuntimePrimType},
    CachedReader, Error, FieldDescription, MethodTable, OpIndex,
    RuntimePrimValue, RuntimeType, TypedPointer, VirtualMachine,
};

use super::{
    graph_rewrite::Analysis, native_function::WrappedNativeFunction,
    virtual_machine::VirtualMachineBuilder, ExposedNativeFunction,
    GraphRewrite, NativeFunction, TypeInference,
};

#[derive(Default, Clone)]
pub struct SymbolicGraph {
    ops: Vec<Expr>,
    extern_funcs: Vec<OpIndex>,
}

#[derive(Clone)]
pub struct Expr {
    pub(crate) kind: ExprKind,
    pub(crate) name: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ExprKind {
    /// A variable argument to a function.
    FunctionArg(RuntimeType),

    /// A function definition
    Function {
        /// The parameters of the function.  Each parameter must be an
        /// instance of ExprKind::FunctionArg.
        params: Vec<SymbolicValue>,
        output: SymbolicValue,
    },

    /// A call into a function.
    FunctionCall {
        /// The callee.  Should point to either an instance of
        /// `ExprKind::Function`, or `ExprKind::NativeFunction`.
        func: SymbolicValue,

        /// The arguments to be used to call the function.
        args: Vec<SymbolicValue>,
    },

    /// A tuple of values.  Currently just used as a function's return
    /// type, in cases where multiple values are returned.
    Tuple(Vec<SymbolicValue>),

    /// A native function, exposed to be used as part of the
    /// expression.
    NativeFunction(ExposedNativeFunction),

    /// An iterator that starts at zero, has `extent` elements, each
    /// increasing by one.
    Range { extent: SymbolicValue },

    /// Map the elements of an iterator, using a mapping function.
    Map {
        /// The iterator whose elements should be mapped.
        iterator: SymbolicValue,

        /// The mapping function to apply.  Should have signature
        /// `Fn(ItemA) -> ItemB`
        map: SymbolicValue,
    },

    /// Perform a reduction along an iterator.
    Reduce {
        /// The initial value of the reduction.
        initial: SymbolicValue,

        /// The iterator over which the reduction should be performed.
        iterator: SymbolicValue,

        /// The reduction function.  Should have signature
        /// `Fn(TResult, Item) -> TResult`
        reduction: SymbolicValue,
    },

    /// Perform a reduction along an iterator.
    SimpleReduce {
        /// The initial value of the reduction.
        initial: SymbolicValue,

        /// The number of iterations over which the reduction should
        /// be performed.
        extent: SymbolicValue,

        /// The reduction function.  Should have signature
        /// `Fn(TResult, usize) -> TResult`
        reduction: SymbolicValue,
    },

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

    /// Check if a value is well-defined.
    IsSome(SymbolicValue),

    /// Perform a conditional statement
    IfElse {
        /// The condition.
        condition: SymbolicValue,

        /// The value if the condition is true.
        if_branch: SymbolicValue,

        /// The value if the condition is false.
        else_branch: SymbolicValue,
    },

    /// Returns true if the operands are equal, false otherwise
    Equal {
        lhs: SymbolicValue,
        rhs: SymbolicValue,
    },

    /// Returns false if the operands are equal, true otherwise
    NotEqual {
        lhs: SymbolicValue,
        rhs: SymbolicValue,
    },

    /// Returns true if the left-hand side is less than the right-hand
    /// side, false otherwise
    LessThan {
        lhs: SymbolicValue,
        rhs: SymbolicValue,
    },

    /// Returns true if the left-hand side is greater than the
    /// right-hand side, false otherwise
    GreaterThan {
        lhs: SymbolicValue,
        rhs: SymbolicValue,
    },

    /// Returns true if the left-hand side is less than or equal to
    /// the right-hand side, false otherwise
    LessThanOrEqual {
        lhs: SymbolicValue,
        rhs: SymbolicValue,
    },

    /// Returns true if the left-hand side is greater than or equal to
    /// the right-hand side, false otherwise
    GreaterThanOrEqual {
        lhs: SymbolicValue,
        rhs: SymbolicValue,
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

    /// Perform numeric multiplication of the LHS and RHS.
    Div {
        lhs: SymbolicValue,
        rhs: SymbolicValue,
    },

    /// Determine the Perform numeric multiplication of the LHS and RHS.
    ///
    /// When handling negative operands, the sign of the result is
    /// taken from the denominator.  This follows standard
    /// mathematical conventions for the modulo operator, and always
    /// produces an output between zero (inclusive) and the
    /// denominator (exclusive).  (This convention is the same as is used for Python.)  , and is *NOT* the convention u C
    /// convention, which uses the sign of the numerator for the sign
    /// of the output.
    Mod {
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

    /// Read a .NET string
    ///
    /// Given a location of a .NET string, produces a Rust-native
    /// String.
    ReadString { ptr: SymbolicValue },
}

#[derive(Debug, PartialEq, Eq, Hash, Clone, Copy, From)]
pub enum SymbolicValue {
    Bool(bool),
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

pub struct GraphComparison<'a> {
    lhs: &'a SymbolicGraph,
    rhs: &'a SymbolicGraph,
    order_dependent: bool,
    compare_names: bool,
}

pub struct SymbolicGraphCompiler<'a, 'b> {
    graph: &'a SymbolicGraph,
    show_steps: bool,
    reader: Option<CachedReader<'b>>,
    optimize_symbolic_graph: bool,
}

#[derive(Debug, Clone, Copy)]
struct LastUsage {
    /// The expression in which the expression is used.
    usage_point: OpIndex,

    /// The expression that was used.
    expr_used: OpIndex,
}

/// Indicates which scope contains an operation
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Scope {
    /// The operation is contained within the global scope.
    Global,

    /// The operation is contained within the body of a function.  A
    /// function's body is identified as the set of all expressions
    /// that depend on at least one of the function's parameters, and
    /// are depended on by the function's output.
    Function(OpIndex),

    /// The operation is contained within the `if` branch of a
    /// conditional.  A conditional branch's body is identified as the
    /// set of all expressions that contribute to the branch's value,
    /// and only contribute to the parent scope's value through the
    /// branch.
    IfBranch(OpIndex),

    /// The operation is contained within the `else` branch of a
    /// conditional.  See also, `Scope::IfBranch`.
    ElseBranch(OpIndex),
}

/// The result of analyzing a function
#[derive(Debug)]
#[allow(dead_code)]
struct ScopeInfo {
    /// The index of the function definition, which will always be a
    /// `ExprKind::Function`.  If `None`, refers to expressions that
    /// are outside the scope of any function definition.
    scope: Scope,

    /// An ordered list of expressions that are part of the body of
    /// the function.  A function's body is defined as all the set of
    /// all expressions such that the expression depends on a function
    /// parameter, and the function output depends on the expression.
    body: Vec<OpIndex>,

    /// Variables from outside of the function's body that are used by
    /// the function.
    enclosed: Vec<OpIndex>,

    /// The enclosing scope that contains this function.  Note: For
    /// Scope::Global, this field contains Scope::Global.
    parent_scope: Scope,
}

/// Helper struct for collecting VM instructions
struct ExpressionTranslator<'a> {
    /// The graph being translated
    graph: &'a SymbolicGraph,

    /// The instructions being collected.
    instructions: &'a mut VirtualMachineBuilder,

    instructions_by_scope: &'a HashMap<Scope, Vec<OpIndex>>,

    /// Indicates the last time that an expression is used.
    last_usage: &'a [LastUsage],

    num_outputs: usize,
    next_free_index: &'a mut usize,
    dead_indices: &'a mut BinaryHeap<Reverse<usize>>,
    native_functions: &'a mut Vec<ExposedNativeFunction>,
    native_function_lookup: &'a mut HashMap<OpIndex, FunctionIndex>,

    reserved_outputs: &'a mut HashMap<OpIndex, StackIndex>,

    currently_stored: &'a mut HashMap<OpIndex, VMArg>,

    previously_consumed: HashSet<OpIndex>,

    show_steps: bool,
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

macro_rules! binary_op {
    ($name:ident, $variant:ident) => {
        pub fn $name(
            &mut self,
            lhs: impl Into<SymbolicValue>,
            rhs: impl Into<SymbolicValue>,
        ) -> SymbolicValue {
            let lhs = lhs.into();
            let rhs = rhs.into();
            self.push(ExprKind::$variant { lhs, rhs })
        }
    };
}

impl SymbolicGraph {
    pub fn new() -> Self {
        Self {
            ops: Vec::new(),
            extern_funcs: Vec::new(),
        }
    }

    pub fn num_operations(&self) -> usize {
        self.ops.len()
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

    pub fn is_reserved_name(name: &str) -> bool {
        name.starts_with('_')
            && name
                .chars()
                .skip(1)
                .next()
                .map(|c| c.is_ascii_digit())
                .unwrap_or(true)
    }

    pub fn name(
        &mut self,
        value: SymbolicValue,
        name: impl Into<String>,
    ) -> Result<(), Error> {
        match value {
            SymbolicValue::Result(op_index) => {
                let name = name.into();
                if Self::is_reserved_name(name.as_str()) {
                    return Err(Error::AttemptedUseOfReservedName(name));
                }
                self.ops[op_index.0].name = Some(name);
            }
            SymbolicValue::Bool(_)
            | SymbolicValue::Int(_)
            | SymbolicValue::Ptr(_) => {
                // Currently, constants are stored in-line at their
                // point-of-use, and can't be named.  If constants are
                // ever moved to be represented as their own nodes,
                // then they should be name-able.
            }
        }
        Ok(())
    }

    pub fn mark_extern_func(
        &mut self,
        value: impl Into<SymbolicValue>,
    ) -> Result<(), Error> {
        let index = value
            .into()
            .as_op_index()
            .ok_or(Error::AttemptedToMarkNonFunctionAsExternFunc)?;

        let node = &self[index];

        if !matches!(node.kind, ExprKind::Function { .. }) {
            return Err(Error::AttemptedToMarkNonFunctionAsExternFunc);
        }

        if node.name.is_none() {
            return Err(Error::ExternalFunctionMustBeNamed);
        }

        self.extern_funcs.push(index);
        Ok(())
    }

    pub fn graph_comparison<'a>(
        &'a self,
        rhs: &'a Self,
    ) -> GraphComparison<'a> {
        GraphComparison {
            lhs: self,
            rhs,
            order_dependent: false,
            compare_names: false,
        }
    }

    //////////////////////////////////////////////////
    ////          Symbolic Operations              ///
    //////////////////////////////////////////////////

    pub fn function_arg(
        &mut self,
        ty: impl Into<RuntimeType>,
    ) -> SymbolicValue {
        let ty = ty.into();
        self.push(ExprKind::FunctionArg(ty))
    }

    pub fn function_def(
        &mut self,
        params: Vec<SymbolicValue>,
        output: SymbolicValue,
    ) -> SymbolicValue {
        self.push(ExprKind::Function { params, output })
    }

    pub fn function_call(
        &mut self,
        func: SymbolicValue,
        args: Vec<SymbolicValue>,
    ) -> SymbolicValue {
        self.push(ExprKind::FunctionCall { func, args })
    }

    pub fn range(&mut self, extent: impl Into<SymbolicValue>) -> SymbolicValue {
        let extent = extent.into();
        self.push(ExprKind::Range { extent })
    }

    pub fn map(
        &mut self,
        iterator: SymbolicValue,
        map: SymbolicValue,
    ) -> SymbolicValue {
        self.push(ExprKind::Map { iterator, map })
    }

    pub fn reduce(
        &mut self,
        initial: impl Into<SymbolicValue>,
        iterator: SymbolicValue,
        reduction: SymbolicValue,
    ) -> SymbolicValue {
        let initial = initial.into();
        self.push(ExprKind::Reduce {
            initial,
            iterator,
            reduction,
        })
    }

    pub fn simple_reduce(
        &mut self,
        initial: impl Into<SymbolicValue>,
        extent: impl Into<SymbolicValue>,
        reduction: SymbolicValue,
    ) -> SymbolicValue {
        let initial = initial.into();
        let extent = extent.into();
        self.push(ExprKind::SimpleReduce {
            initial,
            extent,
            reduction,
        })
    }

    pub fn tuple(&mut self, elements: Vec<SymbolicValue>) -> SymbolicValue {
        self.push(ExprKind::Tuple(elements))
    }

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
        self.access_indices(obj, [index])
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
            .map(|index| {
                let index = index.into();
                // TODO: Move this into a legalization step instead of
                // being applied during the graph construction.
                match index {
                    SymbolicValue::Int(_) => index,
                    _ => self.prim_cast(index, RuntimePrimType::NativeUInt),
                }
            })
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

    pub fn native_function<Func, ArgList>(
        &mut self,
        func: Func,
    ) -> SymbolicValue
    where
        WrappedNativeFunction<Func, ArgList>: NativeFunction,
        WrappedNativeFunction<Func, ArgList>: 'static,
    {
        let wrapped = WrappedNativeFunction::new(func);
        self.push(ExprKind::NativeFunction(wrapped.into()))
    }

    pub fn raw_native_function(
        &mut self,
        func: ExposedNativeFunction,
    ) -> SymbolicValue {
        self.push(ExprKind::NativeFunction(func))
    }

    pub fn is_some(
        &mut self,
        value: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let value = value.into();
        self.push(ExprKind::IsSome(value))
    }

    pub fn if_else(
        &mut self,
        condition: impl Into<SymbolicValue>,
        if_branch: impl Into<SymbolicValue>,
        else_branch: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let condition = condition.into();
        let if_branch = if_branch.into();
        let else_branch = else_branch.into();
        self.push(ExprKind::IfElse {
            condition,
            if_branch,
            else_branch,
        })
    }

    binary_op! {equal, Equal}
    binary_op! {not_equal, NotEqual}
    binary_op! {less_than, LessThan}
    binary_op! {greater_than, GreaterThan}
    binary_op! {less_than_or_equal, LessThanOrEqual}
    binary_op! {greater_than_or_equal, GreaterThanOrEqual}

    binary_op! {add, Add}
    binary_op! {mul, Mul}
    binary_op! {div, Div}
    binary_op! {modulo, Mod}

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

    pub fn read_string(
        &mut self,
        ptr: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let ptr = ptr.into();
        self.push(ExprKind::ReadString { ptr })
    }

    //////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////

    pub(crate) fn iter_extern_funcs(
        &self,
    ) -> impl DoubleEndedIterator<Item = OpIndex> + '_ {
        self.extern_funcs.iter().cloned()
    }

    pub(crate) fn iter_ops(
        &self,
    ) -> impl DoubleEndedIterator<Item = (OpIndex, &Expr)> + '_ {
        self.ops
            .iter()
            .enumerate()
            .map(|(i, op)| (OpIndex::new(i), op))
    }

    //////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////

    fn validate_types(
        &self,
        reader: Option<CachedReader<'_>>,
    ) -> Result<(), Error> {
        let type_inference = TypeInference::new(reader);

        for (index, _) in self.iter_ops() {
            type_inference.infer_type(self, index.into())?;
        }

        Ok(())
    }

    fn validate_only_back_references(&self) -> Result<(), Error> {
        for (index, op) in self.iter_ops() {
            let mut result = Ok(());
            op.visit_reachable_nodes(|input_value| {
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

    fn validate_unique_parameter_owner_among_reachable_functions(
        &self,
    ) -> Result<(), Error> {
        let iter_functions = self
            .reachable(self.extern_funcs.iter().map(|index| (*index).into()))
            .into_iter()
            .enumerate()
            .filter(|(_, reachable)| *reachable)
            .map(|(i, _)| OpIndex::new(i))
            .filter_map(|index| match &self[index].kind {
                ExprKind::Function { params, .. } => Some((index, params)),
                _ => None,
            })
            .flat_map(|(func_index, params)| {
                params.iter().map(move |param| (func_index, *param))
            })
            .filter_map(|(func_index, param_value)| match param_value {
                SymbolicValue::Result(param_index) => {
                    Some((func_index, param_index))
                }
                _ => None,
            });

        let mut param_to_owner: HashMap<OpIndex, OpIndex> = HashMap::new();
        for (func_index, param_index) in iter_functions {
            if let Some(prev_func) = param_to_owner.get(&param_index) {
                return Err(Error::MultipleFunctionsOwnSameParam {
                    param: param_index,
                    first_owner: *prev_func,
                    second_owner: func_index,
                });
            } else {
                param_to_owner.insert(param_index, func_index);
            }
        }

        Ok(())
    }

    fn validate_all_parameters_defined(&self) -> Result<(), Error> {
        let reachable = self
            .reachable(self.extern_funcs.iter().map(|index| (*index).into()));

        let defined_params: HashSet<OpIndex> = reachable
            .iter()
            .cloned()
            .enumerate()
            .filter(|(_, reachable)| *reachable)
            .map(|(i, _)| OpIndex::new(i))
            .filter_map(|index| match &self[index].kind {
                ExprKind::Function { params, .. } => Some(params),
                _ => None,
            })
            .flatten()
            .filter_map(|param| param.as_op_index())
            .collect();

        reachable
            .iter()
            .cloned()
            .enumerate()
            .filter(|(_, reachable)| *reachable)
            .map(|(i, _)| OpIndex::new(i))
            .filter(|index| {
                matches!(self[*index].kind, ExprKind::FunctionArg(_))
            })
            .try_for_each(|param_index| {
                if defined_params.contains(&param_index) {
                    Ok(())
                } else {
                    Err(Error::UseOfUndefinedParam { param: param_index })
                }
            })
    }

    pub fn validate(
        &self,
        reader: Option<CachedReader<'_>>,
    ) -> Result<(), Error> {
        self.validate_types(reader)?;
        self.validate_only_back_references()?;
        self.validate_unique_parameter_owner_among_reachable_functions()?;
        self.validate_all_parameters_defined()?;

        Ok(())
    }

    /// Collect the subgraph of all nodes between the specified
    ///
    /// Returns the set of all nodes such that there exists a path
    /// from at least one of the inputs to the node, and from the node
    /// to at least one of the outputs.  The set is inclusive, and
    /// will include all input nodes that can reach an output node,
    /// and all output nodes that can be reached from an input node.
    ///
    /// This is used when inspecting a function.  For example,
    /// identifying the set of nodes that should be printed within the
    /// body of a function.  Alternatively, the set of nodes which
    /// need to be rewritten when inlining a function.
    pub fn collect_subgraph(
        &self,
        inputs: impl IntoIterator<Item = SymbolicValue>,
        outputs: impl IntoIterator<Item = SymbolicValue>,
    ) -> Vec<OpIndex> {
        let inputs: Vec<OpIndex> = inputs
            .into_iter()
            .filter_map(|input| input.as_op_index())
            .collect();

        if inputs.is_empty() {
            // Early return in the case that there are no inputs.
            // This can occur for nullary functions.
            return vec![];
        };

        let earliest_input = inputs
            .iter()
            .map(|index| index.0)
            .min()
            .expect("Only empty sequences can return None");

        // Step 1, walk backwards from the outputs to the inputs.  The
        // filter on `earliest_input` reduces the number of nodes that
        // must be inspected.  Because nodes may only ever reference
        // nodes that appear earlier in the operation list, a node
        // that appears prior to the earliest input may not depend on
        // any inputs.
        let used_by_outputs: Vec<OpIndex> = {
            let mut to_visit = Vec::<OpIndex>::new();
            let mut used_by_outputs = HashSet::<OpIndex>::new();

            macro_rules! mark {
                ($value:expr) => {
                    $value
                        .as_op_index()
                        .filter(|index| index.0 >= earliest_input)
                        .filter(|index| !used_by_outputs.contains(index))
                        .into_iter()
                        .for_each(|index| {
                            to_visit.push(index);
                            used_by_outputs.insert(index);
                        })
                };
            }

            for output in outputs {
                mark!(output);
            }

            while let Some(visiting) = to_visit.pop() {
                self[visiting].visit_reachable_nodes(|value| mark!(value));
            }

            used_by_outputs
                .into_iter()
                .sorted_by_key(|index| index.0)
                .collect()
        };

        // Step 2, walk forward from the inputs to the outputs.  This
        // only considers nodes that were found earlier when walking
        // from the outputs.
        let mut depend_on_inputs: HashSet<OpIndex> =
            inputs.into_iter().collect();
        let mut subgraph: Vec<OpIndex> =
            depend_on_inputs.iter().cloned().collect();

        for index in used_by_outputs {
            let mut uses_input = false;
            self[index].visit_reachable_nodes(|value| {
                if let Some(upstream) = value.as_op_index() {
                    if depend_on_inputs.contains(&upstream) {
                        uses_input = true;
                    }
                }
            });
            if uses_input {
                subgraph.push(index);
                depend_on_inputs.insert(index);
            }
        }
        subgraph
    }

    #[allow(dead_code)]
    fn analyze_scopes(&self) -> Vec<ScopeInfo> {
        let iter_scopes = self.iter_ops().flat_map(|(index, op)| {
            let (a, b) = match op.kind {
                ExprKind::Function { .. } => {
                    (Some(Scope::Function(index)), None)
                }
                ExprKind::IfElse { .. } => (
                    Some(Scope::IfBranch(index)),
                    Some(Scope::ElseBranch(index)),
                ),
                _ => (None, None),
            };
            a.into_iter().chain(b)
        });

        let mut info: Vec<_> = std::iter::once(Scope::Global)
            .chain(iter_scopes)
            .map(|scope| ScopeInfo {
                scope,
                parent_scope: Scope::Global,
                body: vec![],
                enclosed: vec![],
            })
            .collect();

        let mut scope_lookup: HashMap<Scope, &mut ScopeInfo> = info
            .iter_mut()
            .map(|func_info| (func_info.scope, func_info))
            .collect();

        let scope = self.operation_scope();

        scope
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, scope)| (OpIndex::new(i), scope))
            .for_each(|(op_index, scope)| {
                if let Some(func_info) = scope_lookup.get_mut(&scope) {
                    func_info.body.push(op_index);
                }
            });

        for func in info.iter_mut() {
            if let Some(OpIndex(i)) = func.scope.op_index() {
                func.parent_scope = scope[i];
            }
        }

        for func in info.iter_mut() {
            let mut enclosed: HashSet<OpIndex> = HashSet::new();
            let mut do_visit = |value| {
                if let SymbolicValue::Result(prev_index) = value {
                    if scope[prev_index.0] != func.scope {
                        enclosed.insert(prev_index);
                    }
                }
            };

            if let Scope::Function(index) = func.scope {
                match &self[index].kind {
                    ExprKind::Function { output, .. } => {
                        do_visit(*output);
                    }
                    _ => {
                        unreachable!(
                            "Index {index} collected as function, \
                             but did not reference a function."
                        );
                    }
                }
            }
            for op in func.body.iter().cloned() {
                self[op].kind.visit_reachable_nodes(&mut do_visit);
            }

            func.enclosed =
                enclosed.into_iter().sorted_by_key(|op| op.0).collect();
        }

        info
    }

    /// For each expression, determine which function owns it.
    ///
    /// `Scope::Function(func_index)`: The expression uses at least
    /// one parameter from the function declared at `func_index`, and
    /// the expression is used by the function's output.
    ///
    /// `Scope::IfBranch(if_else_index)`: The expression is used by
    /// the if branch of the `ExprKind::IfElse` declared at
    /// `if_else_index`, and is not used by any other
    pub(crate) fn operation_scope(&self) -> Vec<Scope> {
        let mut scope = vec![Scope::Global; self.ops.len()];

        // Step 1: Visit each function in reverse order of
        // declaration.  For each function, mark all expressions that
        // are part of the subgraph defined by the function parameters
        // and output.  This handles nested scopes, since inner-scoped
        // functions will be visited after their containing scope.
        self.iter_ops()
            .rev()
            .for_each(|(func_index, op)| match &op.kind {
                ExprKind::Function { params, output } => {
                    self.collect_subgraph(
                        params.iter().cloned(),
                        Some(*output),
                    )
                    .into_iter()
                    .for_each(|index| {
                        scope[index.0] = Scope::Function(func_index);
                    });
                }
                _ => {}
            });

        // Step 2: Update the scope with If/Else branches.  When
        // walking along the dominator graph, a node will either
        // encounter an if/else branch, or its containing function.
        let dominators = self.dominators();
        for i in 0..self.ops.len() {
            let op_index = OpIndex(i);

            match self[op_index].kind {
                ExprKind::FunctionArg(_) => {
                    continue;
                }
                _ => {}
            }

            let mut dominator = Some(op_index);
            while let Some(dom_index) = dominator {
                let Some(next_dom) = dominators[dom_index.0] else {
                    break;
                };
                match self[next_dom].kind {
                    ExprKind::Function { .. } => {
                        break;
                    }
                    ExprKind::IfElse {
                        if_branch: SymbolicValue::Result(if_index),
                        ..
                    } if if_index == dom_index
                        && scope[if_index.0] == scope[i] =>
                    {
                        scope[i] = Scope::IfBranch(next_dom);
                        break;
                    }
                    ExprKind::IfElse {
                        else_branch: SymbolicValue::Result(else_index),
                        ..
                    } if else_index == dom_index
                        && scope[else_index.0] == scope[i] =>
                    {
                        scope[i] = Scope::ElseBranch(next_dom);
                        break;
                    }
                    ExprKind::IfElse { .. } => {
                        break;
                    }
                    _ => {}
                }

                dominator = Some(next_dom);
            }
        }

        scope
    }

    fn last_usage(&self) -> Vec<LastUsage> {
        let all_scopes = self.operation_scope();

        let operations_by_scope: HashMap<Scope, Vec<OpIndex>> = all_scopes
            .iter()
            .cloned()
            .enumerate()
            .rev()
            .map(|(i, scope)| (scope, OpIndex::new(i)))
            .into_group_map();

        let collector = LastUsageCollector {
            graph: self,
            operations_by_scope: &operations_by_scope,
        };

        let mut last_usage = Vec::<LastUsage>::new();
        let mut encountered = HashSet::<OpIndex>::new();

        collector.walk_tree(
            &mut encountered,
            &mut last_usage,
            collector.iter_scope(Scope::Global),
        );

        last_usage
            .into_iter()
            .filter(|last_usage| match &self[last_usage.expr_used].kind {
                ExprKind::Function { .. } => false,
                ExprKind::FunctionArg(_) => false,
                ExprKind::NativeFunction(_) => false,
                _ => true,
            })
            .sorted_by_key(|last_usage| {
                (last_usage.usage_point, last_usage.expr_used)
            })
            .collect()
    }

    /// Using Lengauer-Tarjan algorithm, find dominators in the graph
    /// mapping outputs to inputs.
    ///
    /// Returns a list of indices.  Element `i` of the returned vector
    /// specifies the location of the immediate dominator of operation
    /// `i`.  If element `i` of the returned vector is `None`, then
    /// operation `i` is a root node.
    ///
    /// https://dl.acm.org/doi/pdf/10.1145/357062.357071
    pub(crate) fn dominators(&self) -> Vec<Option<OpIndex>> {
        let mut parent = vec![0; self.ops.len() + 1];
        let mut dfs_order_to_op_order = vec![0; self.ops.len() + 1];
        let mut op_order_to_dfs_order = vec![0; self.ops.len()];

        {
            let mut visited: HashSet<usize> = HashSet::new();
            let mut to_visit: Vec<(usize, usize)> = self
                .extern_funcs
                .iter()
                .map(|op_index| (op_index.0 + 1, 0))
                .collect();
            let mut i_dfs = 0;

            while let Some((i_op, this_parent)) = to_visit.pop() {
                if visited.contains(&i_op) {
                    continue;
                }

                i_dfs += 1;
                dfs_order_to_op_order[i_dfs] = i_op - 1;
                op_order_to_dfs_order[i_op - 1] = i_dfs;
                parent[i_dfs] = this_parent;

                let mut do_visit = |value| {
                    if let SymbolicValue::Result(index) = value {
                        to_visit.push((index.0 + 1, i_dfs));
                    }
                };

                match &self[OpIndex(i_op - 1)].kind {
                    ExprKind::Function { output, .. } => do_visit(*output),
                    other => other.visit_reachable_nodes(do_visit),
                }

                visited.insert(i_op);
            }
        }

        let mut succ: Vec<Vec<usize>> =
            (0..self.ops.len() + 1).map(|_| Vec::new()).collect();
        let mut pred: Vec<Vec<usize>> =
            (0..self.ops.len() + 1).map(|_| Vec::new()).collect();
        for (i_op, i_dfs_src) in
            op_order_to_dfs_order.iter().cloned().enumerate()
        {
            let i_op = OpIndex(i_op);
            let mut do_visit = |value| {
                if let SymbolicValue::Result(index) = value {
                    let i_dfs_dest = op_order_to_dfs_order[index.0];
                    succ[i_dfs_src].push(i_dfs_dest);
                    pred[i_dfs_dest].push(i_dfs_src);
                }
            };

            match &self[i_op].kind {
                ExprKind::Function { output, .. } => do_visit(*output),
                other => other.visit_reachable_nodes(do_visit),
            }
        }

        let mut semi: Vec<usize> = (0..self.ops.len() + 1).collect();
        let mut idom = vec![0usize; self.ops.len() + 1];
        let mut ancestor = vec![0usize; self.ops.len() + 1];
        let mut best: Vec<usize> = (0..self.ops.len() + 1).collect();
        let mut bucket: Vec<Vec<usize>> =
            (0..self.ops.len() + 1).map(|_| Vec::new()).collect();

        fn compress(
            ancestor: &mut [usize],
            semi: &[usize],
            best: &mut [usize],
            v: usize,
        ) {
            let a = ancestor[v];
            if ancestor[a] == 0 {
                return;
            }
            compress(ancestor, semi, best, a);

            if semi[best[v]] > semi[best[a]] {
                best[v] = best[a];
            }

            ancestor[v] = ancestor[a];
        }
        let eval = |ancestor: &mut [usize],
                    semi: &[usize],
                    best: &mut [usize],
                    v: usize|
         -> usize {
            if ancestor[v] == 0 {
                v
            } else {
                compress(ancestor, semi, best, v);
                best[v]
            }
        };

        for w in (1..=self.ops.len()).rev() {
            let p = parent[w];

            // Step 2
            for v in pred[w].iter().cloned() {
                let u = eval(&mut ancestor, &semi, &mut best, v);
                if semi[w] > semi[u] {
                    semi[w] = semi[u];
                }
            }
            bucket[semi[w]].push(w);
            assert!(bucket[semi[w]].len() < self.ops.len() * 10);
            ancestor[w] = p;

            // Step 3
            for v in bucket[p].iter().cloned() {
                let u = eval(&mut ancestor, &semi, &mut best, v);
                let new_idom = if semi[u] < semi[v] { u } else { p };
                idom[v] = new_idom;
            }
            bucket[p].clear();
        }

        // Step 4
        for w in 2..=self.ops.len() {
            if idom[w] != semi[w] {
                idom[w] = idom[idom[w]];
            }
        }
        idom[1] = 0;

        let idom_by_op: Vec<Option<OpIndex>> = (0..self.ops.len())
            .map(|i| {
                let dfs_order = op_order_to_dfs_order[i];
                let idom_dfs = idom[dfs_order];
                (idom_dfs > 0).then(|| {
                    let idom_op = dfs_order_to_op_order[idom_dfs];
                    OpIndex::new(idom_op)
                })
            })
            .collect();

        idom_by_op
    }

    pub fn simplify<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<Self, Error> {
        let analysis = Analysis::new(reader);
        let rewriter = super::RemoveUnusedDowncast(&analysis)
            .then(super::ConstantFold)
            .then(super::RemoveUnusedPrimcast(&analysis))
            .apply_recursively();
        self.rewrite(rewriter)
    }

    fn remap_extern_funcs(
        &self,
        lookup: &HashMap<OpIndex, SymbolicValue>,
    ) -> Result<Vec<OpIndex>, Error> {
        self.extern_funcs
            .iter()
            .map(|old_index| {
                let new_value = lookup
                    .get(old_index)
                    .expect("All operations have been visited");
                let new_index = new_value
                    .as_op_index()
                    .ok_or(Error::AttemptedToMarkNonFunctionAsExternFunc)?;
                Ok(new_index)
            })
            .collect()
    }

    pub fn rewrite(&self, rewriter: impl GraphRewrite) -> Result<Self, Error> {
        let mut prev_index_lookup: HashMap<OpIndex, SymbolicValue> =
            HashMap::new();
        let mut builder = Self::new();

        for (old_index, op) in self.iter_ops() {
            let opt_remapped = op.kind.try_remap(&prev_index_lookup);
            let kind = opt_remapped.as_ref().unwrap_or(&op.kind);

            let opt_value = rewriter.rewrite_expr(&mut builder, kind)?;
            let value = opt_value.unwrap_or_else(|| {
                let expr: Expr = if let Some(remapped) = opt_remapped {
                    remapped.into()
                } else {
                    op.clone()
                };
                builder.push(expr)
            });

            // If the pre-rewrite value had a name, then copy it to
            // the post-rewrite value.  This should only be applied if
            // the post-rewrite value does not already have a name.
            // For example, in `let y = x+0`, both `x` and `y` are
            // named values.  When `y` gets replaced with `x`, it
            // should *NOT* cause `x` to be renamed in earlier parts
            // of the function.
            if let Some(name) = &op.name {
                if let SymbolicValue::Result(new_index) = value {
                    if builder[new_index].name.is_none() {
                        builder.name(value, name)?;
                    }
                }
            }
            prev_index_lookup.insert(old_index, value);
        }

        builder.extern_funcs = self.remap_extern_funcs(&prev_index_lookup)?;

        Ok(builder)
    }

    pub fn rewrite_subtree(
        &mut self,
        rewriter: impl GraphRewrite,
        indices: impl Iterator<Item = OpIndex>,
        rewrites_applied: &mut HashMap<OpIndex, SymbolicValue>,
    ) -> Result<(), Error> {
        for old_index in indices {
            let op = self[old_index].clone();
            let opt_remapped = op.kind.try_remap(rewrites_applied);
            let kind = opt_remapped.as_ref().unwrap_or(&op.kind);

            let value =
                rewriter.rewrite_expr(self, kind)?.unwrap_or_else(|| {
                    let expr: Expr = if let Some(remapped) = opt_remapped {
                        Expr {
                            kind: remapped,
                            name: op.name,
                        }
                    } else {
                        op
                    };
                    self.push(expr)
                });
            rewrites_applied.insert(old_index, value);
        }

        Ok(())
    }

    pub(crate) fn reachable(
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
            self[visiting].visit_reachable_nodes(|upstream| {
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

        let reachable =
            self.reachable(self.extern_funcs.iter().cloned().map(Into::into));

        for (prev_index, op) in self.iter_ops() {
            if reachable[prev_index.0] {
                let kind = op
                    .kind
                    .try_remap(&prev_index_lookup)
                    .unwrap_or_else(|| op.kind.clone());
                let new_index = builder.push(kind);
                if let Some(name) = &op.name {
                    builder.name(new_index, name)?;
                }

                prev_index_lookup.insert(prev_index, new_index);
            }
        }

        builder.extern_funcs = self.remap_extern_funcs(&prev_index_lookup)?;

        Ok(builder)
    }

    pub fn eliminate_common_subexpressions(self) -> Result<Self, Error> {
        let mut builder = Self::new();

        let mut prev_index_lookup: HashMap<OpIndex, SymbolicValue> =
            HashMap::new();
        let mut dedup_lookup: HashMap<ExprKind, SymbolicValue> = HashMap::new();
        let mut functions_returning_rust_native: HashSet<SymbolicValue> =
            HashSet::new();

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
                if let Some(name) = &op.name {
                    builder.name(new_index, name)?;
                }

                if let ExprKind::NativeFunction(func) = &new_kind {
                    // Track fucntions
                    let RuntimeType::Function(FunctionType { output, .. }) =
                        func.signature()?
                    else {
                        panic!(
                            "Internal error, \
                                    NativeFunction should have function type."
                        )
                    };

                    if matches!(
                        output.as_ref(),
                        RuntimeType::Unknown | RuntimeType::Rust(_)
                    ) {
                        functions_returning_rust_native.insert(new_index);
                    }
                }

                let can_dedup = match &new_kind {
                    ExprKind::FunctionArg(_) => {
                        // Temporary workaround.  The long-term fix is
                        // to make update the hashing so that
                        // FunctionArg have structural equality when
                        // encountering points of definition, but
                        // reference equality when encountering points
                        // of use (that haven't already been defined,
                        // that is).
                        false
                    }
                    ExprKind::FunctionCall { func, .. } => {
                        !functions_returning_rust_native.contains(func)
                    }

                    _ => true,
                };

                if can_dedup {
                    dedup_lookup.insert(new_kind, new_index);
                }

                new_index
            };
            prev_index_lookup.insert(prev_index, new_index);
        }

        builder.extern_funcs = self.remap_extern_funcs(&prev_index_lookup)?;

        Ok(builder)
    }

    //////////////////////////////////////////////////////////////////
    //////////////////////////////////////////////////////////////////

    pub fn to_virtual_machine(
        &self,
        show_steps: bool,
    ) -> Result<VirtualMachine, Error> {
        let mut builder = VirtualMachine::builder();

        assert!(!self.extern_funcs.is_empty());

        if self.extern_funcs.len() > 1 {
            todo!("Handle VMs with multiple functions");
        }

        let main_func_index = self.extern_funcs[0];
        let main_func = &self[main_func_index];

        let ExprKind::Function { params, output } = &main_func.kind else {
            panic!(
                "Internal error, \
                 `extern_funcs` should only point to functions."
            )
        };
        let output = *output;

        if !params.is_empty() {
            todo!("Handle extern functions with parameters");
        }

        let outputs = match output {
            SymbolicValue::Result(op_index) => match &self[op_index].kind {
                ExprKind::Tuple(values) => values.clone(),
                _ => vec![output],
            },
            _ => vec![output],
        };
        let num_outputs = outputs.len();

        let mut next_free_index = num_outputs;
        let mut native_functions: Vec<ExposedNativeFunction> = Vec::new();
        let mut native_function_lookup =
            HashMap::<OpIndex, FunctionIndex>::new();

        let mut output_lookup: HashMap<OpIndex, StackIndex> = {
            let mut output_lookup = HashMap::new();
            for (i, output) in outputs.iter().cloned().enumerate() {
                let stack_index = StackIndex(i);
                match output {
                    SymbolicValue::Result(op_index) => {
                        output_lookup.insert(op_index, stack_index);
                    }
                    SymbolicValue::Bool(value) => {
                        let value = RuntimePrimValue::Bool(value).into();
                        builder.push(Instruction::Copy {
                            value,
                            output: stack_index,
                        });
                    }
                    SymbolicValue::Int(value) => {
                        let value = RuntimePrimValue::NativeUInt(value).into();
                        builder.push(Instruction::Copy {
                            value,
                            output: stack_index,
                        });
                    }
                    SymbolicValue::Ptr(ptr) => {
                        let value = RuntimePrimValue::Ptr(ptr).into();
                        builder.push(Instruction::Copy {
                            value,
                            output: stack_index,
                        });
                    }
                }
            }
            output_lookup
        };

        let scope = self.operation_scope();

        let last_usage = self.last_usage();

        let mut dead_indices = BinaryHeap::new();
        let mut currently_stored = HashMap::new();

        let operations_by_scope = scope
            .iter()
            .cloned()
            .enumerate()
            .map(|(i, scope)| (scope, OpIndex::new(i)))
            .into_group_map();

        let iter_op_indices = scope
            .iter()
            .enumerate()
            .filter(|(_, scope)| **scope == Scope::Global)
            .map(|(i, _)| OpIndex::new(i))
            .filter(|index| {
                !matches!(self[*index].kind, ExprKind::Function { .. })
            });

        let mut translater = ExpressionTranslator {
            graph: self,
            instructions: &mut builder,
            instructions_by_scope: &operations_by_scope,
            last_usage: &last_usage,
            num_outputs,
            next_free_index: &mut next_free_index,
            dead_indices: &mut dead_indices,
            native_functions: &mut native_functions,
            native_function_lookup: &mut native_function_lookup,
            reserved_outputs: &mut output_lookup,
            currently_stored: &mut currently_stored,
            previously_consumed: HashSet::new(),
            show_steps,
        };
        translater.translate(iter_op_indices)?;

        builder = builder.num_outputs(outputs.len());
        for native_func in native_functions.into_iter() {
            builder = builder.with_raw_native_function(native_func);
        }

        Ok(builder.build())
    }

    pub fn compiler<'a, 'b>(&'a self) -> SymbolicGraphCompiler<'a, 'b> {
        SymbolicGraphCompiler::from_graph(self)
    }

    pub fn compile<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<VirtualMachine, Error> {
        SymbolicGraphCompiler::from_graph(self)
            .with_reader(reader)
            .compile()
    }
}

struct LastUsageCollector<'a> {
    graph: &'a SymbolicGraph,
    operations_by_scope: &'a HashMap<Scope, Vec<OpIndex>>,
}

impl<'a> LastUsageCollector<'a> {
    fn iter_scope(&self, scope: Scope) -> impl Iterator<Item = OpIndex> + '_ {
        self.operations_by_scope
            .get(&scope)
            .into_iter()
            .flatten()
            .cloned()
    }

    fn walk_tree(
        &self,
        encountered: &mut HashSet<OpIndex>,
        last_usage: &mut Vec<LastUsage>,
        to_visit: impl Iterator<Item = OpIndex>,
    ) {
        for visiting in to_visit {
            macro_rules! mark_value {
                ($node_set:expr, $value:expr) => {
                    if let SymbolicValue::Result(index) = $value {
                        if !$node_set.contains(&index) {
                            last_usage.push(LastUsage {
                                usage_point: visiting,
                                expr_used: index,
                            });
                            $node_set.insert(index);
                        }
                    }
                };
            }

            match &self.graph[visiting].kind {
                ExprKind::Function { output, .. } => {
                    self.walk_tree(
                        encountered,
                        last_usage,
                        self.iter_scope(Scope::Function(visiting)),
                    );
                    mark_value!(encountered, *output);
                }

                ExprKind::IfElse { condition, .. } => {
                    mark_value!(encountered, *condition);

                    let mut encountered_if: HashSet<OpIndex> =
                        encountered.clone();
                    self.walk_tree(
                        &mut encountered_if,
                        last_usage,
                        self.iter_scope(Scope::IfBranch(visiting)),
                    );

                    let mut encountered_else: HashSet<OpIndex> =
                        encountered.clone();
                    self.walk_tree(
                        &mut encountered_else,
                        last_usage,
                        self.iter_scope(Scope::ElseBranch(visiting)),
                    );

                    if let Some(start_of_else) =
                        self.iter_scope(Scope::ElseBranch(visiting)).next()
                    {
                        for index in &encountered_if {
                            if encountered.contains(index) {
                                if !encountered_else.contains(index) {
                                    last_usage.push(LastUsage {
                                        usage_point: start_of_else,
                                        expr_used: *index,
                                    })
                                }
                            }
                        }
                    }

                    if let Some(start_of_if) =
                        self.iter_scope(Scope::IfBranch(visiting)).next()
                    {
                        for index in &encountered_else {
                            if encountered.contains(index) {
                                if !encountered_if.contains(index) {
                                    last_usage.push(LastUsage {
                                        usage_point: start_of_if,
                                        expr_used: *index,
                                    });
                                }
                            }
                        }
                    }

                    encountered_if
                        .into_iter()
                        .chain(encountered_else)
                        .for_each(|index| {
                            encountered.insert(index);
                        });
                }
                other => other.visit_reachable_nodes(|value| {
                    mark_value!(encountered, value)
                }),
            }
        }
    }
}

impl Scope {
    fn op_index(&self) -> Option<OpIndex> {
        match self {
            Scope::Global => None,
            Scope::Function(op_index) => Some(*op_index),
            Scope::IfBranch(op_index) => Some(*op_index),
            Scope::ElseBranch(op_index) => Some(*op_index),
        }
    }

    #[allow(dead_code)]
    fn printer<'a>(
        &'a self,
        graph: &'a SymbolicGraph,
    ) -> impl std::fmt::Display + 'a {
        return Printer { scope: self, graph };

        struct Printer<'a> {
            scope: &'a Scope,
            graph: &'a SymbolicGraph,
        }
        impl<'a> std::fmt::Display for Printer<'a> {
            fn fmt(
                &self,
                fmt: &mut std::fmt::Formatter<'_>,
            ) -> std::fmt::Result {
                match self.scope {
                    Scope::Global => write!(fmt, "(global)")?,
                    Scope::Function(op_index) => {
                        if let Some(name) = &self.graph[*op_index].name {
                            write!(fmt, "Function '{name}' at {op_index}")?;
                        } else {
                            write!(fmt, "Function at {op_index}")?;
                        }
                    }
                    Scope::IfBranch(op_index) => {
                        if let Some(name) = &self.graph[*op_index].name {
                            write!(fmt, "If-branch of '{name}' at {op_index}")?;
                        } else {
                            write!(fmt, "If-branch at {op_index}")?;
                        }
                    }
                    Scope::ElseBranch(op_index) => {
                        if let Some(name) = &self.graph[*op_index].name {
                            write!(
                                fmt,
                                "Else-branch of '{name}' at {op_index}"
                            )?;
                        } else {
                            write!(fmt, "Else-branch at {op_index}")?;
                        }
                    }
                }
                Ok(())
            }
        }
    }
}

impl<'a, 'b> SymbolicGraphCompiler<'a, 'b> {
    pub(crate) fn from_graph(graph: &'a SymbolicGraph) -> Self {
        let show_steps = std::env::var("SHOW_STEPS")
            .map(|var| {
                if var.is_empty() {
                    false
                } else if var.eq_ignore_ascii_case("true") {
                    true
                } else if let Ok(value) = var.parse::<usize>() {
                    value > 0
                } else {
                    false
                }
            })
            .unwrap_or(false);

        Self {
            graph,
            show_steps,
            reader: None,
            optimize_symbolic_graph: true,
        }
    }

    pub fn show_each_step(self, show_steps: bool) -> Self {
        Self { show_steps, ..self }
    }

    pub fn with_reader(
        self,
        reader: impl Into<Option<CachedReader<'b>>>,
    ) -> Self {
        Self {
            reader: reader.into(),
            ..self
        }
    }

    pub fn disable_optimizations(self) -> Self {
        Self {
            optimize_symbolic_graph: false,
            ..self
        }
    }

    pub fn compile(self) -> Result<VirtualMachine, Error> {
        let reader = self.reader;
        let expr = self.graph;

        if self.show_steps {
            println!("----------- Initial Graph --------------\n{expr}");
        }
        expr.validate_only_back_references()?;
        expr.validate_unique_parameter_owner_among_reachable_functions()?;
        expr.validate_all_parameters_defined()?;

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
        expr.validate_only_back_references()?;
        expr.validate_unique_parameter_owner_among_reachable_functions()?;
        expr.validate_all_parameters_defined()?;

        if self.show_steps {
            println!(
                "----------- After IdentifyStaticField --------------\n{expr}"
            );
        }

        let expr = expr.dead_code_elimination()?;
        expr.validate(reader)?;

        if self.show_steps {
            println!("----------- After DCE --------------\n{expr}");
        }
        let expr = expr.eliminate_common_subexpressions()?;
        expr.validate(reader)?;
        if self.show_steps {
            println!("----------- After CSE --------------\n{expr}");
        }

        let expr = if self.optimize_symbolic_graph {
            let analysis = Analysis::new(reader);
            let rewriter = super::ConstantFold
                .then(super::RemoveUnusedDowncast(&analysis))
                .then(super::RemoveUnusedPrimcast(&analysis))
                .then(super::LowerSymbolicExpr(&analysis))
                .then(super::RemoveUnusedPointerCast)
                .then(super::InlineFunctionCalls)
                .then(super::MergeRangeReduceToSimpleReduce)
                .then(super::InlineIteratorMap)
                .apply_recursively();

            expr.rewrite(rewriter)?
        } else {
            let analysis = Analysis::new(reader);
            let rewriter = super::LowerSymbolicExpr(&analysis)
                .then(super::InlineFunctionCalls)
                .then(super::MergeRangeReduceToSimpleReduce)
                .then(super::InlineIteratorMap)
                .apply_recursively();

            expr.rewrite(rewriter)?
        };

        expr.validate(reader)?;

        if self.show_steps {
            println!("----------- After Simplifcations --------------\n{expr}");
        }

        let expr = expr.dead_code_elimination()?;
        expr.validate(reader)?;

        if self.show_steps {
            println!("----------- After DCE --------------\n{expr}");
        }

        let expr = expr.eliminate_common_subexpressions()?;
        expr.validate(reader)?;

        if self.show_steps {
            println!("----------- After CSE --------------\n{expr}");
        }

        // Virtual machine, in terms of sequential operations.
        let vm = expr.to_virtual_machine(self.show_steps)?;

        if self.show_steps {
            println!("----------- As VM --------------\n{vm}");
        }

        let vm = vm.simplify();
        if self.show_steps {
            println!("----------- VM (simplified) --------------\n{vm}");
        }

        Ok(vm)
    }
}

struct LocalIndexPrinter<'a> {
    index: OpIndex,
    name: Option<&'a str>,
}
impl<'a> LocalIndexPrinter<'a> {
    fn new(index: OpIndex, graph: &'a SymbolicGraph) -> Self {
        Self {
            index,
            name: graph[index].name.as_ref().map(|string| string.as_str()),
        }
    }
}
impl<'a> std::fmt::Display for LocalIndexPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(name) = self.name {
            write!(f, "'{name}' {}", self.index)
        } else {
            write!(f, "{}", self.index)
        }
    }
}

impl ExpressionTranslator<'_> {
    fn value_to_arg(&self, value: &SymbolicValue) -> Result<VMArg, Error> {
        match value {
            &SymbolicValue::Bool(value) => {
                Ok(VMArg::Const(RuntimePrimValue::Bool(value)))
            }
            &SymbolicValue::Int(value) => {
                Ok(VMArg::Const(RuntimePrimValue::NativeUInt(value)))
            }
            &SymbolicValue::Ptr(ptr) => {
                Ok(VMArg::Const(RuntimePrimValue::Ptr(ptr)))
            }
            SymbolicValue::Result(op_index)
                if self.previously_consumed.contains(&op_index) =>
            {
                Err(Error::AttemptedUseOfConsumedValue)
            }
            SymbolicValue::Result(op_index) => Ok(self
                .currently_stored
                .get(&op_index)
                .unwrap_or_else(|| {
                    panic!(
                        "Internal error, \
                         {op_index} was not previously translated.  \
                         This op is expression {}",
                        self.graph[*op_index].kind
                    )
                })
                .clone()),
        }
    }

    fn alloc_index(&mut self) -> StackIndex {
        self.dead_indices
            .pop()
            .map(|Reverse(i)| StackIndex(i))
            .unwrap_or_else(|| {
                let index = *self.next_free_index;
                *self.next_free_index += 1;
                StackIndex(index)
            })
    }

    fn free_dead_indices(&mut self, op_index: OpIndex) {
        let first_index = self.last_usage.partition_point(|last_usage| {
            last_usage.usage_point.0 < op_index.0
        });
        self.last_usage[first_index..]
            .iter()
            .take_while(|last_usage| last_usage.usage_point == op_index)
            .for_each(|last_usage| {
                let Some(old_value) =
                    self.currently_stored.remove(&last_usage.expr_used)
                else {
                    panic!(
                        "Internal error: \
                         Last usage of {} occurred after {}, \
                         (expr '{}' used in '{}') \
                         but {} was not actually defined.  \
                         Currently, [{}] are defined.",
                        last_usage.expr_used,
                        last_usage.usage_point,
                        self.graph[last_usage.expr_used].kind,
                        self.graph[last_usage.usage_point].kind,
                        last_usage.expr_used,
                        self.currently_stored
                            .iter()
                            .map(|(op_index, _)| *op_index)
                            .sorted()
                            .map(|op_index| format!("{op_index}"))
                            .join(", "),
                    )
                };

                if let VMArg::SavedValue(StackIndex(old_index)) = old_value {
                    self.dead_indices.push(Reverse(old_index));
                }
            });
    }

    fn get_output_index(&mut self, op_index: OpIndex) -> StackIndex {
        self.reserved_outputs
            .get(&op_index)
            .cloned()
            .unwrap_or_else(|| self.alloc_index())
    }

    fn translate(
        &mut self,
        instructions: impl Iterator<Item = OpIndex>,
    ) -> Result<(), Error> {
        for op_index in instructions {
            let op = &self.graph[op_index];
            let expr_name = LocalIndexPrinter::new(op_index, self.graph);

            macro_rules! push_annotated {
                ($inst:expr $(,)?) => {{
                    self.instructions.push($inst)
                }};

                ($inst:expr, $annot:expr $(,)?) => {{
                    let inst = self.instructions.push($inst);
                    if self.show_steps {
                        self.instructions.annotate(inst, $annot);
                    }
                    inst
                }};
            }

            macro_rules! handle_binary_op {
                ($variant:ident, $lhs:expr, $rhs:expr) => {{
                    let lhs = self.value_to_arg($lhs)?;
                    let rhs = self.value_to_arg($rhs)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::$variant {
                            lhs,
                            rhs,
                            output: op_output,
                        },
                        format!("evaluate {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }};
            }

            match op.as_ref() {
                ExprKind::Function { .. } => {
                    unreachable!(
                        "Function calls should be inlined, \
                         and should only be encountered in SimpleReduce.  \
                         But at index {op_index}, encountered function named {:?}",
                        op.name
                    )
                }
                ExprKind::FunctionArg(_) => {
                    assert!(self.currently_stored.contains_key(&op_index));
                }
                ExprKind::Tuple(_) => {
                    // Eventually I should put something here, but I
                    // think this will just barely work for the
                    // current use cases.  A tuple of function outputs
                    // is assigned the appropriate StackIndex values
                    // for each tuple element.  When encountering
                    // those operations, their output was written to
                    // the appropriate output index.  So now, when
                    // encountering the tuple itself, I just
                    // do...nothing.
                    //
                    // This will break horribly if tuples are used in
                    // any other context, but thankfully I don't yet
                    // support any other contexts.
                }
                ExprKind::FunctionCall { func, args } => {
                    let Some(func) = func.as_op_index() else {
                        panic!("Internal error, callee must be function")
                    };

                    if let Some(&native_func_index) =
                        self.native_function_lookup.get(&func)
                    {
                        let mut vm_args = Vec::new();
                        for arg in args {
                            vm_args.push(self.value_to_arg(arg)?);
                        }

                        let mutates_first_argument = self.native_functions
                            [native_func_index.0]
                            .mutates_first_argument();

                        if mutates_first_argument {
                            // The function mutates its input
                            // argument.  No output index is required.
                            // However, the `currently_stored` lookup
                            // should be updated to no longer contain
                            // the first argument.
                            let first_arg_op = match &args[0] {
                                SymbolicValue::Result(op_index) => *op_index,
                                SymbolicValue::Bool(_) |SymbolicValue::Int(_) |
                                SymbolicValue::Ptr(_) => todo!(
                                    "Attempted mutation of const SymbolicValue.  \
                                     Should handle this case earlier using \
                                     a new ExprKind to represent mutable constants."
                                ),

                            };
                            let first_arg_loc = match &vm_args[0] {
                                VMArg::SavedValue(stack_index) => *stack_index,
                                VMArg::Const(_) => todo!(
                                    "Attempted mutation of VMArg::Const.  \
                                     Should handle this case earlier using \
                                     a new ExprKind to represent mutable constants."
                                ),
                            };

                            push_annotated!(
                                Instruction::NativeFunctionCall {
                                    index: native_func_index,
                                    args: vm_args,
                                    output: None,
                                },
                                format!("produce {expr_name}"),
                            );

                            self.currently_stored.remove(&first_arg_op);
                            self.previously_consumed.insert(first_arg_op);

                            let op_output = self.get_output_index(op_index);
                            if op_output.0 < self.num_outputs {
                                push_annotated!(
                                    Instruction::Swap(first_arg_loc, op_output),
                                    format!(
                                        "swap {expr_name} \
                                         to mandatory output loc {op_output}"
                                    ),
                                );
                                self.currently_stored
                                    .insert(op_index, op_output.into());
                            } else {
                                self.currently_stored
                                    .insert(op_index, first_arg_loc.into());
                            }
                        } else {
                            // The function produces an output value, to
                            // be stored in the output index.
                            let op_output = self.get_output_index(op_index);
                            push_annotated!(
                                Instruction::NativeFunctionCall {
                                    index: native_func_index,
                                    args: vm_args,
                                    output: Some(op_output),
                                },
                                format!("produce {expr_name}"),
                            );
                            self.currently_stored
                                .insert(op_index, op_output.into());
                        }
                    } else {
                        todo!(
                            "Handle IR-defined function calls in the VM.  \
                             Until then, all functions should be inlined."
                        )
                    }
                }

                ExprKind::Range { .. } => panic!(
                    "All Range expressions should be simplified \
                     to ExprKind::SimpleReduce"
                ),
                ExprKind::Map { .. } => panic!(
                    "All Map expressions should be simplified \
                     to ExprKind::SimpleReduce"
                ),
                ExprKind::Reduce { .. } => panic!(
                    "All Reduce expressions should be simplified \
                     to ExprKind::SimpleReduce"
                ),
                ExprKind::SimpleReduce {
                    initial,
                    extent,
                    reduction,
                } => {
                    let initial = self.value_to_arg(initial)?;
                    let extent = self.value_to_arg(extent)?;

                    let op_output = self.get_output_index(op_index);

                    match initial {
                        VMArg::Const(_) => {
                            push_annotated!(
                                Instruction::Copy {
                                    value: initial,
                                    output: op_output,
                                },
                                format!(
                                    "copy initial value \
                                     of reduction {expr_name}"
                                ),
                            );
                        }
                        VMArg::SavedValue(stack_index) => {
                            push_annotated!(
                                Instruction::Swap(stack_index, op_output,),
                                format!(
                                    "move initial value \
                                     of reduction {expr_name}"
                                ),
                            );
                        }
                    }

                    let loop_iter = self.alloc_index();
                    push_annotated!(
                        Instruction::Copy {
                            value: 0usize.into(),
                            output: loop_iter,
                        },
                        format!("initialize loop iterator for {expr_name}"),
                    );

                    let loop_start = self.instructions.current_index();

                    let stop_loop_condition = self.alloc_index();
                    push_annotated!(
                        Instruction::GreaterThanOrEqual {
                            lhs: loop_iter.into(),
                            rhs: extent,
                            output: stop_loop_condition,
                        },
                        format!("loop condition of {expr_name}"),
                    );

                    // Placeholder for the conditional jump to break
                    // out of the loop.  To be updated after the loop
                    // body is generated, when we know the destination
                    // index of the jump.
                    let jump_to_end_instruction_index = push_annotated!(
                        Instruction::NoOp,
                        format!("loop for {expr_name} terminated"),
                    );

                    let reduction_index = match reduction {
                        &SymbolicValue::Result(op_index) => op_index,
                        _ => todo!(
                            "Better error message \
                             when SimpleReduce points to non-function"
                        ),
                    };

                    match &self.graph[reduction_index].kind {
                        ExprKind::Function { params, output } => {
                            if params.len() != 2 {
                                todo!("Better error message for invalid reduction function");
                            }
                            let accumulator = match params[0] {
                                SymbolicValue::Result(op_index) => op_index,
                                _ => todo!("Better error message for ill-formed function"),
                            };
                            let index = match params[1] {
                                SymbolicValue::Result(op_index) => op_index,
                                _ => todo!("Better error message for ill-formed function"),
                            };

                            self.currently_stored
                                .insert(accumulator, op_output.into());
                            self.currently_stored
                                .insert(index, loop_iter.into());

                            let iter_body = self
                                .instructions_by_scope
                                .get(&Scope::Function(reduction_index))
                                .into_iter()
                                .flatten()
                                .cloned()
                                .filter(|&op| {
                                    !matches!(
                                        self.graph[op].kind,
                                        ExprKind::Function { .. }
                                    )
                                });
                            let iter_body: Box<dyn Iterator<Item = OpIndex>> =
                                Box::new(iter_body);

                            self.translate(iter_body)?;

                            match self.value_to_arg(output)? {
                                VMArg::Const(value) => {
                                    push_annotated!(
                                        Instruction::Copy {
                                            value: value.into(),
                                            output: op_output,
                                        },
                                        format!("copy output of {expr_name}"),
                                    );
                                }
                                VMArg::SavedValue(body_output) => {
                                    push_annotated!(
                                        Instruction::Swap(
                                            body_output,
                                            op_output,
                                        ),
                                        format!("move output of {expr_name}"),
                                    );
                                }
                            }

                            self.currently_stored.remove(&accumulator);
                            self.currently_stored.remove(&index);
                        }
                        ExprKind::NativeFunction(_) => {
                            let native_func_index = self
                                .native_function_lookup
                                .get(&reduction_index)
                                .expect(
                                    "Internal error, \
                                     function should have \
                                     already been encountered.",
                                );
                            let mutates_first_argument = self.native_functions
                                [native_func_index.0]
                                .mutates_first_argument();

                            let func_output = if mutates_first_argument {
                                None
                            } else {
                                Some(op_output)
                            };

                            push_annotated!(
                                Instruction::NativeFunctionCall {
                                    index: *native_func_index,
                                    args: vec![
                                        op_output.into(),
                                        loop_iter.into(),
                                    ],
                                    output: func_output,
                                },
                                format!(
                                    "native call to reduce into {expr_name}"
                                ),
                            );
                        }
                        _ => todo!(
                            "Better error message \
                             when SimpleReduce points to non-function"
                        ),
                    }

                    push_annotated!(
                        Instruction::Add {
                            lhs: loop_iter.into(),
                            rhs: 1usize.into(),
                            output: loop_iter,
                        },
                        format!("increment loop iterator for {expr_name}"),
                    );

                    push_annotated!(
                        Instruction::ConditionalJump {
                            cond: RuntimePrimValue::Bool(true).into(),
                            dest: loop_start,
                        },
                        format!("jump to beginning of {expr_name}"),
                    );

                    let after_loop = self.instructions.current_index();
                    self.instructions.update(
                        jump_to_end_instruction_index,
                        Instruction::ConditionalJump {
                            cond: stop_loop_condition.into(),
                            dest: after_loop,
                        },
                    );

                    self.free_dead_indices(op_index);

                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::NativeFunction(func) => {
                    let func_index = FunctionIndex(self.native_functions.len());
                    self.native_functions.push(func.clone());
                    self.native_function_lookup.insert(op_index, func_index);
                }

                ExprKind::IsSome(value) => {
                    let value = self.value_to_arg(value)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::IsSome {
                            value,
                            output: op_output,
                        },
                        format!("evaluate {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }

                ExprKind::IfElse {
                    condition,
                    if_branch,
                    else_branch,
                } => {
                    let condition = self.value_to_arg(condition)?;

                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    let mut cached_currently_stored =
                        self.currently_stored.clone();

                    let jump_to_if_branch_index = push_annotated!(
                        Instruction::NoOp,
                        format!("jump to if branch of {expr_name}"),
                    );

                    match else_branch {
                        &SymbolicValue::Result(else_index) => {
                            if let Some(existing) =
                                self.currently_stored.get(&else_index)
                            {
                                push_annotated!(
                                    Instruction::Copy {
                                        value: *existing,
                                        output: op_output
                                    },
                                    "move existing else-branch of {expr_name} \
                                     to output of if/else"
                                );
                            } else {
                                self.reserved_outputs
                                    .insert(else_index, op_output);
                                self.translate(
                                    self.instructions_by_scope
                                        .get(&Scope::ElseBranch(op_index))
                                        .into_iter()
                                        .flatten()
                                        .cloned(),
                                )?;
                                self.reserved_outputs.remove(&else_index);
                                std::mem::swap(
                                    self.currently_stored,
                                    &mut cached_currently_stored,
                                );
                            }
                        }
                        other => {
                            push_annotated!(
                                Instruction::Copy {
                                    value: other
                                        .as_prim_value()
                                        .expect(
                                            "Only SymbolicValue::Result \
                                             should return None",
                                        )
                                        .into(),
                                    output: op_output,
                                },
                                format!(
                                    "copy else-branch output \
                                     to {expr_name}"
                                ),
                            );
                        }
                    }

                    let jump_to_branch_end_index = push_annotated!(
                        Instruction::NoOp,
                        format!(
                            "after else branch {expr_name}, \
                             skip the if branch"
                        ),
                    );

                    self.instructions.update(
                        jump_to_if_branch_index,
                        Instruction::ConditionalJump {
                            cond: condition,
                            dest: self.instructions.current_index(),
                        },
                    );

                    match if_branch {
                        &SymbolicValue::Result(if_index) => {
                            if let Some(existing) =
                                self.currently_stored.get(&if_index)
                            {
                                push_annotated!(
                                    Instruction::Copy {
                                        value: *existing,
                                        output: op_output
                                    },
                                    "move existing if-branch of {expr_name} \
                                     to output of if/else"
                                );
                            } else {
                                self.reserved_outputs
                                    .insert(if_index, op_output);
                                self.translate(
                                    self.instructions_by_scope
                                        .get(&Scope::IfBranch(op_index))
                                        .into_iter()
                                        .flatten()
                                        .cloned(),
                                )?;
                                self.reserved_outputs.remove(&if_index);
                            }
                        }
                        other => {
                            push_annotated!(
                                Instruction::Copy {
                                    value: other
                                        .as_prim_value()
                                        .expect(
                                            "Only SymbolicValue::Result \
                                             should return None",
                                        )
                                        .into(),
                                    output: op_output,
                                },
                                format!(
                                    "copy if-branch output \
                                     to {expr_name}"
                                ),
                            );
                        }
                    }

                    self.instructions.update(
                        jump_to_branch_end_index,
                        Instruction::ConditionalJump {
                            cond: true.into(),
                            dest: self.instructions.current_index(),
                        },
                    );

                    *self.currently_stored = cached_currently_stored
                        .into_iter()
                        .filter(|(op_index, _)| {
                            self.currently_stored.contains_key(op_index)
                        })
                        .collect();

                    self.currently_stored.insert(op_index, op_output.into());
                }

                ExprKind::Equal { lhs, rhs } => {
                    handle_binary_op!(Equal, lhs, rhs)
                }
                ExprKind::NotEqual { lhs, rhs } => {
                    handle_binary_op!(NotEqual, lhs, rhs)
                }
                ExprKind::GreaterThan { lhs, rhs } => {
                    handle_binary_op!(GreaterThan, lhs, rhs)
                }
                ExprKind::LessThan { lhs, rhs } => {
                    handle_binary_op!(LessThan, lhs, rhs)
                }
                ExprKind::GreaterThanOrEqual { lhs, rhs } => {
                    handle_binary_op!(GreaterThanOrEqual, lhs, rhs)
                }
                ExprKind::LessThanOrEqual { lhs, rhs } => {
                    handle_binary_op!(LessThanOrEqual, lhs, rhs)
                }

                ExprKind::Add { lhs, rhs } => handle_binary_op!(Add, lhs, rhs),
                ExprKind::Mul { lhs, rhs } => handle_binary_op!(Mul, lhs, rhs),
                ExprKind::Div { lhs, rhs } => handle_binary_op!(Div, lhs, rhs),
                ExprKind::Mod { lhs, rhs } => handle_binary_op!(Mod, lhs, rhs),

                ExprKind::PrimCast { value, prim_type } => {
                    let value = self.value_to_arg(value)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::PrimCast {
                            value,
                            prim_type: *prim_type,
                            output: op_output,
                        },
                        format!("eval {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::PhysicalDowncast { obj, ty } => {
                    let obj = self.value_to_arg(obj)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::Downcast {
                            obj,
                            subtype: *ty,
                            output: op_output,
                        },
                        format!("eval {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::ReadValue { ptr, prim_type } => {
                    let ptr = self.value_to_arg(ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::Read {
                            ptr,
                            prim_type: *prim_type,
                            output: op_output,
                        },
                        format!("eval {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::ReadString { ptr } => {
                    let ptr = self.value_to_arg(ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::ReadString {
                            ptr,
                            output: op_output,
                        },
                        format!("eval {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::PointerCast { ptr, .. } => {
                    let ptr = self.value_to_arg(ptr)?;
                    self.free_dead_indices(op_index);
                    let op_output = self.get_output_index(op_index);
                    push_annotated!(
                        Instruction::Copy {
                            value: ptr,
                            output: op_output,
                        },
                        format!("eval {expr_name}"),
                    );
                    self.currently_stored.insert(op_index, ptr);
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
        }

        Ok(())
    }
}

impl SymbolicValue {
    pub(crate) fn as_op_index(self) -> Option<OpIndex> {
        match self {
            SymbolicValue::Result(op_index) => Some(op_index),
            _ => None,
        }
    }

    pub(crate) fn as_prim_value(self) -> Option<RuntimePrimValue> {
        match self {
            SymbolicValue::Bool(val) => Some(RuntimePrimValue::Bool(val)),
            SymbolicValue::Int(val) => Some(RuntimePrimValue::NativeUInt(val)),
            SymbolicValue::Ptr(ptr) => Some(RuntimePrimValue::Ptr(ptr)),
            SymbolicValue::Result(_) => None,
        }
    }
}

impl Expr {
    pub(crate) fn visit_reachable_nodes(
        &self,
        callback: impl FnMut(SymbolicValue),
    ) {
        self.kind.visit_reachable_nodes(callback)
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

        let remap_or_no_op = |value: &SymbolicValue| -> SymbolicValue {
            remap(value).unwrap_or_else(|| *value)
        };

        let vec_requires_remap = |slice: &[SymbolicValue]| -> bool {
            slice.iter().any(|item| match item {
                SymbolicValue::Result(op_index) => map.contains_key(op_index),
                SymbolicValue::Bool(_)
                | SymbolicValue::Int(_)
                | SymbolicValue::Ptr(_) => false,
            })
        };

        macro_rules! handle_binary_op {
            ($variant:ident, $lhs:expr, $rhs:expr) => {{
                let lhs = $lhs;
                let rhs = $rhs;
                let opt_lhs = remap(lhs);
                let opt_rhs = remap(rhs);
                let requires_remap = opt_lhs.is_some() || opt_rhs.is_some();
                requires_remap.then(|| {
                    let lhs = opt_lhs.unwrap_or_else(|| *lhs);
                    let rhs = opt_rhs.unwrap_or_else(|| *rhs);
                    ExprKind::$variant { lhs, rhs }
                })
            }};
        }

        match self {
            ExprKind::NativeFunction(_)
            | ExprKind::FunctionArg(_)
            | ExprKind::StaticField(_) => None,
            ExprKind::Function { params, output } => {
                let opt_output = remap(output);
                let requires_remap =
                    opt_output.is_some() || vec_requires_remap(&params);
                requires_remap.then(|| {
                    let params = params.iter().map(remap_or_no_op).collect();
                    let output = opt_output.unwrap_or_else(|| *output);
                    ExprKind::Function { params, output }
                })
            }
            ExprKind::FunctionCall { func, args } => {
                let opt_func = remap(func);
                let requires_remap =
                    opt_func.is_some() || vec_requires_remap(args);
                requires_remap.then(|| {
                    let func = opt_func.unwrap_or_else(|| *func);
                    let args = args.iter().map(remap_or_no_op).collect();
                    ExprKind::FunctionCall { func, args }
                })
            }
            ExprKind::Range { extent } => {
                remap(extent).map(|extent| ExprKind::Range { extent })
            }
            ExprKind::Map { iterator, map } => {
                let opt_iterator = remap(iterator);
                let opt_map = remap(map);
                (opt_iterator.is_some() || opt_map.is_some()).then(|| {
                    let iterator = opt_iterator.unwrap_or_else(|| *iterator);
                    let map = opt_map.unwrap_or_else(|| *map);
                    ExprKind::Map { iterator, map }
                })
            }
            ExprKind::Reduce {
                initial,
                iterator,
                reduction,
            } => {
                let opt_initial = remap(initial);
                let opt_iterator = remap(iterator);
                let opt_reduction = remap(reduction);
                (opt_initial.is_some()
                    || opt_iterator.is_some()
                    || opt_reduction.is_some())
                .then(|| {
                    let initial = opt_initial.unwrap_or_else(|| *initial);
                    let iterator = opt_iterator.unwrap_or_else(|| *iterator);
                    let reduction = opt_reduction.unwrap_or_else(|| *reduction);
                    ExprKind::Reduce {
                        initial,
                        iterator,
                        reduction,
                    }
                })
            }
            ExprKind::SimpleReduce {
                initial,
                extent,
                reduction,
            } => {
                let opt_initial = remap(initial);
                let opt_extent = remap(extent);
                let opt_reduction = remap(reduction);
                (opt_initial.is_some()
                    || opt_extent.is_some()
                    || opt_reduction.is_some())
                .then(|| {
                    let initial = opt_initial.unwrap_or_else(|| *initial);
                    let extent = opt_extent.unwrap_or_else(|| *extent);
                    let reduction = opt_reduction.unwrap_or_else(|| *reduction);
                    ExprKind::SimpleReduce {
                        initial,
                        extent,
                        reduction,
                    }
                })
            }
            ExprKind::Tuple(elements) => {
                let requires_remap = vec_requires_remap(elements);
                requires_remap.then(|| {
                    let elements =
                        elements.iter().map(remap_or_no_op).collect();
                    ExprKind::Tuple(elements)
                })
            }
            ExprKind::FieldAccess { obj, field } => {
                remap(obj).map(|obj| ExprKind::FieldAccess {
                    obj,
                    field: field.clone(),
                })
            }
            ExprKind::IndexAccess { obj, indices } => {
                let opt_obj = remap(obj);
                let requires_remap =
                    opt_obj.is_some() || vec_requires_remap(&indices);
                requires_remap.then(|| {
                    let obj = opt_obj.unwrap_or_else(|| *obj);
                    let indices = indices.iter().map(remap_or_no_op).collect();
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
            ExprKind::IsSome(value) => {
                remap(value).map(|value| ExprKind::IsSome(value))
            }

            ExprKind::IfElse {
                condition,
                if_branch,
                else_branch,
            } => {
                let opt_condition = remap(condition);
                let opt_if_branch = remap(if_branch);
                let opt_else_branch = remap(else_branch);
                let requires_remap = opt_condition.is_some()
                    || opt_if_branch.is_some()
                    || opt_else_branch.is_some();
                requires_remap.then(|| {
                    let condition = opt_condition.unwrap_or_else(|| *condition);
                    let if_branch = opt_if_branch.unwrap_or_else(|| *if_branch);
                    let else_branch =
                        opt_else_branch.unwrap_or_else(|| *else_branch);
                    ExprKind::IfElse {
                        condition,
                        if_branch,
                        else_branch,
                    }
                })
            }

            ExprKind::Equal { lhs, rhs } => handle_binary_op!(Equal, lhs, rhs),
            ExprKind::NotEqual { lhs, rhs } => {
                handle_binary_op!(NotEqual, lhs, rhs)
            }
            ExprKind::GreaterThan { lhs, rhs } => {
                handle_binary_op!(GreaterThan, lhs, rhs)
            }
            ExprKind::LessThan { lhs, rhs } => {
                handle_binary_op!(LessThan, lhs, rhs)
            }
            ExprKind::GreaterThanOrEqual { lhs, rhs } => {
                handle_binary_op!(GreaterThanOrEqual, lhs, rhs)
            }
            ExprKind::LessThanOrEqual { lhs, rhs } => {
                handle_binary_op!(LessThanOrEqual, lhs, rhs)
            }

            ExprKind::Add { lhs, rhs } => handle_binary_op!(Add, lhs, rhs),
            ExprKind::Mul { lhs, rhs } => handle_binary_op!(Mul, lhs, rhs),
            ExprKind::Div { lhs, rhs } => handle_binary_op!(Div, lhs, rhs),
            ExprKind::Mod { lhs, rhs } => handle_binary_op!(Mod, lhs, rhs),

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
            ExprKind::ReadString { ptr } => {
                remap(ptr).map(|ptr| ExprKind::ReadString { ptr })
            }
        }
    }

    pub(crate) fn visit_reachable_nodes(
        &self,
        mut callback: impl FnMut(SymbolicValue),
    ) {
        match self {
            ExprKind::NativeFunction(_)
            | ExprKind::FunctionArg(_)
            | ExprKind::StaticField(_) => {}
            ExprKind::Function { params, output } => {
                params.iter().for_each(|param| callback(*param));
                callback(*output);
            }
            ExprKind::FunctionCall { func, args } => {
                callback(*func);
                args.iter().for_each(|arg| callback(*arg));
            }
            ExprKind::Range { extent } => {
                callback(*extent);
            }
            ExprKind::Map { iterator, map } => {
                callback(*iterator);
                callback(*map);
            }
            ExprKind::Reduce {
                initial,
                iterator,
                reduction,
            } => {
                callback(*initial);
                callback(*iterator);
                callback(*reduction);
            }
            ExprKind::SimpleReduce {
                initial,
                extent,
                reduction,
            } => {
                callback(*initial);
                callback(*extent);
                callback(*reduction);
            }
            ExprKind::Tuple(elements) => {
                elements.iter().for_each(|element| callback(*element));
            }
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
            ExprKind::IsSome(value) => callback(*value),
            ExprKind::IfElse {
                condition,
                if_branch,
                else_branch,
            } => {
                callback(*condition);
                callback(*if_branch);
                callback(*else_branch);
            }

            ExprKind::Equal { lhs, rhs }
            | ExprKind::NotEqual { lhs, rhs }
            | ExprKind::GreaterThan { lhs, rhs }
            | ExprKind::LessThan { lhs, rhs }
            | ExprKind::GreaterThanOrEqual { lhs, rhs }
            | ExprKind::LessThanOrEqual { lhs, rhs }
            | ExprKind::Add { lhs, rhs }
            | ExprKind::Mul { lhs, rhs }
            | ExprKind::Div { lhs, rhs }
            | ExprKind::Mod { lhs, rhs } => {
                callback(*lhs);
                callback(*rhs);
            }

            ExprKind::PhysicalDowncast { obj, .. } => {
                callback(*obj);
            }
            ExprKind::ReadValue { ptr, .. }
            | ExprKind::ReadString { ptr, .. } => {
                callback(*ptr);
            }
        }
    }

    pub(crate) fn op_name(&self) -> &'static str {
        match self {
            ExprKind::FunctionArg { .. } => "FunctionArg",
            ExprKind::Function { .. } => "Function",
            ExprKind::FunctionCall { .. } => "FunctionCall",
            ExprKind::Tuple { .. } => "Tuple",
            ExprKind::NativeFunction { .. } => "NativeFunction",
            ExprKind::Range { .. } => "Range",
            ExprKind::Map { .. } => "Map",
            ExprKind::Reduce { .. } => "Reduce",
            ExprKind::SimpleReduce { .. } => "SimpleReduce",
            ExprKind::StaticField { .. } => "StaticField",
            ExprKind::FieldAccess { .. } => "FieldAccess",
            ExprKind::SymbolicDowncast { .. } => "SymbolicDowncast",
            ExprKind::IndexAccess { .. } => "IndexAccess",
            ExprKind::NumArrayElements { .. } => "NumArrayElements",
            ExprKind::ArrayExtent { .. } => "ArrayExtent",
            ExprKind::PointerCast { .. } => "PointerCast",
            ExprKind::IsSome { .. } => "IsSome",
            ExprKind::IfElse { .. } => "IfElse",
            ExprKind::Equal { .. } => "Equal",
            ExprKind::NotEqual { .. } => "NotEqual",
            ExprKind::LessThan { .. } => "LessThan",
            ExprKind::GreaterThan { .. } => "GreaterThan",
            ExprKind::LessThanOrEqual { .. } => "LessThanOrEqual",
            ExprKind::GreaterThanOrEqual { .. } => "GreaterThanOrEqual",
            ExprKind::Add { .. } => "Add",
            ExprKind::Mul { .. } => "Mul",
            ExprKind::Div { .. } => "Div",
            ExprKind::Mod { .. } => "Mod",
            ExprKind::PrimCast { .. } => "PrimCast",
            ExprKind::PhysicalDowncast { .. } => "PhysicalDowncast",
            ExprKind::ReadValue { .. } => "ReadValue",
            ExprKind::ReadString { .. } => "ReadString",
        }
    }
}

impl<'a> GraphComparison<'a> {
    pub fn order_dependent(self, order_dependent: bool) -> Self {
        Self {
            order_dependent,
            ..self
        }
    }

    pub fn compare_names(self, compare_names: bool) -> Self {
        Self {
            compare_names,
            ..self
        }
    }

    pub fn apply(&self) -> bool {
        if self.order_dependent {
            self.order_dependent_comparison()
        } else {
            self.order_independent_comparison()
        }
    }

    fn order_dependent_comparison(&self) -> bool {
        let lhs = self.lhs;
        let rhs = self.rhs;
        lhs.ops.len() == rhs.ops.len()
            && lhs.extern_funcs.len() == rhs.extern_funcs.len()
            && lhs.ops.iter().zip(rhs.ops.iter()).all(|(a, b)| {
                a.kind == b.kind && (!self.compare_names || a.name == b.name)
            })
            && lhs
                .extern_funcs
                .iter()
                .zip(rhs.extern_funcs.iter())
                .all(|(a, b)| a == b)
    }

    fn order_independent_comparison(&self) -> bool {
        let lhs = self.lhs;
        let rhs = self.rhs;

        if lhs.extern_funcs.len() != rhs.extern_funcs.len() {
            return false;
        }

        let mut lhs_index_to_rhs = HashMap::<OpIndex, OpIndex>::new();
        let mut rhs_index_to_lhs = HashMap::<OpIndex, OpIndex>::new();

        let mut to_visit = Vec::<(OpIndex, OpIndex)>::new();

        macro_rules! equivalent_value {
            ($lhs:expr,$rhs:expr) => {{
                let lhs: &SymbolicValue = $lhs;
                let rhs: &SymbolicValue = $rhs;

                match (*lhs, *rhs) {
                    (
                        SymbolicValue::Result(lhs_index),
                        SymbolicValue::Result(rhs_index),
                    ) => {
                        if let Some(prev_rhs) = lhs_index_to_rhs.get(&lhs_index)
                        {
                            rhs_index == *prev_rhs
                        } else if let Some(prev_lhs) =
                            rhs_index_to_lhs.get(&rhs_index)
                        {
                            lhs_index == *prev_lhs
                        } else {
                            to_visit.push((lhs_index, rhs_index));
                            lhs_index_to_rhs.insert(lhs_index, rhs_index);
                            rhs_index_to_lhs.insert(rhs_index, lhs_index);
                            true
                        }
                    }

                    (other_lhs, other_rhs) => other_lhs == other_rhs,
                }
            }};
        }

        for (lhs_extern_func, rhs_extern_func) in lhs
            .extern_funcs
            .iter()
            .cloned()
            .zip(rhs.extern_funcs.iter().cloned())
        {
            let lhs_extern_func: SymbolicValue = lhs_extern_func.into();
            let rhs_extern_func: SymbolicValue = rhs_extern_func.into();
            if !equivalent_value!(&lhs_extern_func, &rhs_extern_func) {
                return false;
            }
        }

        while let Some((lhs_index, rhs_index)) = to_visit.pop() {
            if self.compare_names && lhs[lhs_index].name != rhs[rhs_index].name
            {
                return false;
            }

            let lhs_kind = &lhs[lhs_index].kind;
            let rhs_kind = &rhs[rhs_index].kind;

            macro_rules! handle_binary_op {
                ($variant:ident, $lhs:expr, $rhs:expr) => {{
                    let lhs_lhs = $lhs;
                    let lhs_rhs = $rhs;
                    match rhs_kind {
                        ExprKind::$variant {
                            lhs: rhs_lhs,
                            rhs: rhs_rhs,
                        } => {
                            equivalent_value!(lhs_lhs, rhs_lhs)
                                && equivalent_value!(lhs_rhs, rhs_rhs)
                        }
                        _ => false,
                    }
                }};
            }

            let is_match = match lhs_kind {
                ExprKind::Function {
                    params: lhs_params,
                    output: lhs_output,
                } => match rhs_kind {
                    ExprKind::Function {
                        params: rhs_params,
                        output: rhs_output,
                    } => {
                        lhs_params.len() == rhs_params.len()
                            && lhs_params.iter().zip(rhs_params).all(
                                |(lhs_param, rhs_param)| {
                                    equivalent_value!(lhs_param, rhs_param)
                                },
                            )
                            && equivalent_value!(lhs_output, rhs_output)
                    }
                    _ => false,
                },
                ExprKind::FunctionArg(lhs_ty) => match rhs_kind {
                    ExprKind::FunctionArg(rhs_ty) => lhs_ty == rhs_ty,
                    _ => false,
                },
                ExprKind::FunctionCall {
                    func: lhs_func,
                    args: lhs_args,
                } => match rhs_kind {
                    ExprKind::FunctionCall {
                        func: rhs_func,
                        args: rhs_args,
                    } => {
                        equivalent_value!(lhs_func, rhs_func)
                            && lhs_args.len() == rhs_args.len()
                            && lhs_args.iter().zip(rhs_args).all(
                                |(lhs_arg, rhs_arg)| {
                                    equivalent_value!(lhs_arg, rhs_arg)
                                },
                            )
                    }
                    _ => false,
                },
                ExprKind::Range { extent: lhs_extent } => match rhs_kind {
                    ExprKind::Range { extent: rhs_extent } => {
                        equivalent_value!(lhs_extent, rhs_extent)
                    }
                    _ => false,
                },
                ExprKind::Map {
                    iterator: lhs_iterator,
                    map: lhs_map,
                } => match rhs_kind {
                    ExprKind::Map {
                        iterator: rhs_iterator,
                        map: rhs_map,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_map, rhs_map)
                    }
                    _ => false,
                },
                ExprKind::Reduce {
                    initial: lhs_initial,
                    iterator: lhs_iterator,
                    reduction: lhs_reduction,
                } => match rhs_kind {
                    ExprKind::Reduce {
                        initial: rhs_initial,
                        iterator: rhs_iterator,
                        reduction: rhs_reduction,
                    } => {
                        equivalent_value!(lhs_initial, rhs_initial)
                            && equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_reduction, rhs_reduction)
                    }
                    _ => false,
                },
                ExprKind::SimpleReduce {
                    initial: lhs_initial,
                    extent: lhs_extent,
                    reduction: lhs_reduction,
                } => match rhs_kind {
                    ExprKind::SimpleReduce {
                        initial: rhs_initial,
                        extent: rhs_extent,
                        reduction: rhs_reduction,
                    } => {
                        equivalent_value!(lhs_initial, rhs_initial)
                            && equivalent_value!(lhs_extent, rhs_extent)
                            && equivalent_value!(lhs_reduction, rhs_reduction)
                    }
                    _ => false,
                },
                ExprKind::NativeFunction(lhs_func) => match rhs_kind {
                    ExprKind::NativeFunction(rhs_func) => lhs_func == rhs_func,
                    _ => false,
                },
                ExprKind::Tuple(lhs_tuple) => match rhs_kind {
                    ExprKind::Tuple(rhs_tuple) => {
                        lhs_tuple.len() == rhs_tuple.len()
                            && lhs_tuple.iter().zip(rhs_tuple).all(
                                |(lhs_element, rhs_element)| {
                                    equivalent_value!(lhs_element, rhs_element)
                                },
                            )
                    }
                    _ => false,
                },
                ExprKind::StaticField(StaticField {
                    class: lhs_class,
                    field_name: lhs_field,
                }) => match rhs_kind {
                    ExprKind::StaticField(StaticField {
                        class: rhs_class,
                        field_name: rhs_field,
                    }) => lhs_class == rhs_class && lhs_field == rhs_field,
                    _ => false,
                },
                ExprKind::FieldAccess {
                    obj: lhs_obj,
                    field: lhs_field,
                } => match rhs_kind {
                    ExprKind::FieldAccess {
                        obj: rhs_obj,
                        field: rhs_field,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj)
                            && lhs_field == rhs_field
                    }
                    _ => false,
                },
                ExprKind::SymbolicDowncast {
                    obj: lhs_obj,
                    ty: lhs_ty,
                } => match rhs_kind {
                    ExprKind::SymbolicDowncast {
                        obj: rhs_obj,
                        ty: rhs_ty,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj) && lhs_ty == rhs_ty
                    }
                    _ => false,
                },
                ExprKind::IndexAccess {
                    obj: lhs_obj,
                    indices: lhs_indices,
                } => match rhs_kind {
                    ExprKind::IndexAccess {
                        obj: rhs_obj,
                        indices: rhs_indices,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj)
                            && lhs_indices.len() == rhs_indices.len()
                            && lhs_indices.iter().zip(rhs_indices.iter()).all(
                                |(lhs_index, rhs_index)| {
                                    equivalent_value!(lhs_index, rhs_index)
                                },
                            )
                    }
                    _ => false,
                },
                ExprKind::NumArrayElements { array: lhs_array } => {
                    match rhs_kind {
                        ExprKind::NumArrayElements { array: rhs_array } => {
                            equivalent_value!(lhs_array, rhs_array)
                        }
                        _ => false,
                    }
                }
                ExprKind::ArrayExtent {
                    array: lhs_array,
                    dim: lhs_dim,
                } => match rhs_kind {
                    ExprKind::ArrayExtent {
                        array: rhs_array,
                        dim: rhs_dim,
                    } => {
                        equivalent_value!(lhs_array, rhs_array)
                            && equivalent_value!(lhs_dim, rhs_dim)
                    }
                    _ => false,
                },
                ExprKind::PointerCast {
                    ptr: lhs_ptr,
                    ty: lhs_ty,
                } => match rhs_kind {
                    ExprKind::PointerCast {
                        ptr: rhs_ptr,
                        ty: rhs_ty,
                    } => {
                        equivalent_value!(lhs_ptr, rhs_ptr) && lhs_ty == rhs_ty
                    }
                    _ => false,
                },
                ExprKind::IsSome(lhs_value) => match rhs_kind {
                    ExprKind::IsSome(rhs_value) => {
                        equivalent_value!(lhs_value, rhs_value)
                    }
                    _ => false,
                },
                ExprKind::IfElse {
                    condition: lhs_condition,
                    if_branch: lhs_if_branch,
                    else_branch: lhs_else_branch,
                } => match rhs_kind {
                    ExprKind::IfElse {
                        condition: rhs_condition,
                        if_branch: rhs_if_branch,
                        else_branch: rhs_else_branch,
                    } => {
                        equivalent_value!(lhs_condition, rhs_condition)
                            && equivalent_value!(lhs_if_branch, rhs_if_branch)
                            && equivalent_value!(
                                lhs_else_branch,
                                rhs_else_branch
                            )
                    }
                    _ => false,
                },

                ExprKind::Equal { lhs, rhs } => {
                    handle_binary_op!(Equal, lhs, rhs)
                }
                ExprKind::NotEqual { lhs, rhs } => {
                    handle_binary_op!(NotEqual, lhs, rhs)
                }
                ExprKind::GreaterThan { lhs, rhs } => {
                    handle_binary_op!(GreaterThan, lhs, rhs)
                }
                ExprKind::LessThan { lhs, rhs } => {
                    handle_binary_op!(LessThan, lhs, rhs)
                }
                ExprKind::GreaterThanOrEqual { lhs, rhs } => {
                    handle_binary_op!(GreaterThanOrEqual, lhs, rhs)
                }
                ExprKind::LessThanOrEqual { lhs, rhs } => {
                    handle_binary_op!(LessThanOrEqual, lhs, rhs)
                }

                ExprKind::Add { lhs, rhs } => handle_binary_op!(Add, lhs, rhs),
                ExprKind::Mul { lhs, rhs } => handle_binary_op!(Mul, lhs, rhs),
                ExprKind::Div { lhs, rhs } => handle_binary_op!(Div, lhs, rhs),
                ExprKind::Mod { lhs, rhs } => handle_binary_op!(Mod, lhs, rhs),

                ExprKind::PrimCast {
                    value: lhs_value,
                    prim_type: lhs_prim_type,
                } => match rhs_kind {
                    ExprKind::PrimCast {
                        value: rhs_value,
                        prim_type: rhs_prim_type,
                    } => {
                        equivalent_value!(lhs_value, rhs_value)
                            && lhs_prim_type == rhs_prim_type
                    }
                    _ => false,
                },
                ExprKind::PhysicalDowncast {
                    obj: lhs_obj,
                    ty: lhs_ty,
                } => match rhs_kind {
                    ExprKind::PhysicalDowncast {
                        obj: rhs_obj,
                        ty: rhs_ty,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj) && lhs_ty == rhs_ty
                    }
                    _ => false,
                },
                ExprKind::ReadValue {
                    ptr: lhs_ptr,
                    prim_type: lhs_prim_type,
                } => match rhs_kind {
                    ExprKind::ReadValue {
                        ptr: rhs_ptr,
                        prim_type: rhs_prim_type,
                    } => {
                        equivalent_value!(lhs_ptr, rhs_ptr)
                            && lhs_prim_type == rhs_prim_type
                    }
                    _ => false,
                },
                ExprKind::ReadString { ptr: lhs_ptr } => match rhs_kind {
                    ExprKind::ReadString { ptr: rhs_ptr } => {
                        equivalent_value!(lhs_ptr, rhs_ptr)
                    }
                    _ => false,
                },
            };

            if !is_match {
                return false;
            }
        }

        true
    }
}

impl Display for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let write_tuple = |f: &mut std::fmt::Formatter<'_>,
                           tuple: &[SymbolicValue]|
         -> std::fmt::Result {
            write!(f, "(")?;
            for (i, element) in tuple.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{element}")?;
            }

            write!(f, ")")?;

            Ok(())
        };

        match self {
            ExprKind::FunctionArg(ty) => {
                write!(f, "_: {ty}")
            }
            ExprKind::Function { params, output } => {
                write!(f, "fn")?;
                write_tuple(f, params)?;
                write!(f, " {{ {output} }}")?;

                Ok(())
            }
            ExprKind::FunctionCall { func, args } => {
                write!(f, "{func}")?;
                write_tuple(f, args)
            }
            ExprKind::Range { extent } => {
                write!(f, "(0..{extent})")
            }
            ExprKind::Map { iterator, map } => {
                write!(f, "{iterator}.map({map})")
            }
            ExprKind::Reduce {
                initial,
                iterator,
                reduction,
            } => {
                write!(f, "{iterator}.reduce({initial}, {reduction})")
            }
            ExprKind::SimpleReduce {
                initial,
                extent,
                reduction,
            } => {
                write!(f, "(0..{extent}).reduce({initial}, {reduction})")
            }
            ExprKind::NativeFunction(func) => {
                write!(f, "{func}")
            }
            ExprKind::Tuple(elements) => write_tuple(f, elements),
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

            ExprKind::IsSome(value) => {
                write!(f, "{value}.is_some()")
            }

            ExprKind::IfElse {
                condition,
                if_branch,
                else_branch,
            } => {
                write!(
                    f,
                    "if {condition} \
                     {{ {if_branch} }} \
                     else \
                     {{ {else_branch} }}"
                )
            }

            // TODO: Support Add/Mul/PhysicalDowncast/ReadValue in the
            // parser.
            ExprKind::Equal { lhs, rhs } => write!(f, "{lhs} == {rhs}"),
            ExprKind::NotEqual { lhs, rhs } => write!(f, "{lhs} != {rhs}"),
            ExprKind::LessThan { lhs, rhs } => write!(f, "{lhs} < {rhs}"),
            ExprKind::GreaterThan { lhs, rhs } => write!(f, "{lhs} > {rhs}"),
            ExprKind::LessThanOrEqual { lhs, rhs } => {
                write!(f, "{lhs} <= {rhs}")
            }
            ExprKind::GreaterThanOrEqual { lhs, rhs } => {
                write!(f, "{lhs} >= {rhs}")
            }

            ExprKind::Add { lhs, rhs } => write!(f, "{lhs} + {rhs}"),
            ExprKind::Mul { lhs, rhs } => write!(f, "{lhs}*{rhs}"),
            ExprKind::Div { lhs, rhs } => write!(f, "{lhs}/{rhs}"),
            ExprKind::Mod { lhs, rhs } => write!(f, "{lhs}%{rhs}"),
            ExprKind::PhysicalDowncast { obj, ty } => {
                write!(f, "{obj}.downcast({ty})")
            }
            ExprKind::PrimCast { value, prim_type } => {
                write!(f, "{value}.prim_cast::<{prim_type}>()")
            }
            ExprKind::ReadValue { ptr, prim_type } => {
                write!(f, "{ptr}.read::<{prim_type}>()")
            }
            ExprKind::ReadString { ptr } => {
                write!(f, "{ptr}.read_string()")
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
            SymbolicValue::Bool(value) => write!(f, "{value}"),
            SymbolicValue::Int(value) => write!(f, "{value}"),
            SymbolicValue::Ptr(ptr) => write!(f, "{ptr}"),
            SymbolicValue::Result(op_index) => write!(f, "{op_index}"),
        }
    }
}

impl From<StaticField> for ExprKind {
    fn from(static_field: StaticField) -> Self {
        ExprKind::StaticField(static_field)
    }
}

impl std::ops::Index<OpIndex> for SymbolicGraph {
    type Output = Expr;

    fn index(&self, index: OpIndex) -> &Self::Output {
        &self.ops[index.0]
    }
}
