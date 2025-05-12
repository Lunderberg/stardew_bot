use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use derive_more::derive::From;
use itertools::Itertools as _;

use iterator_extensions::ResultIteratorExt as _;
use memory_reader::Pointer;

use crate::{
    bytecode::printer::IndexPrinter,
    runtime_type::{FunctionType, RuntimePrimType},
    CachedReader, Error, FieldDescription, MethodTable, OpIndex,
    RuntimePrimValue, RuntimeType, TypedPointer, VirtualMachine,
};

use super::{
    graph_rewrite::Analysis, native_function::WrappedNativeFunction,
    ExposedNativeFunction, GraphRewrite, NativeFunction, TypeInference,
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
    /// An empty expression
    None,

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

    /// Filter the elements of an iterator, retaining elements of the
    /// iterator for which the filter function returns true.
    Filter {
        /// The iterator whose elements should be mapped.
        iterator: SymbolicValue,

        /// The filter function to apply.  Should have signature
        /// `Fn(ItemA) -> bool`
        filter: SymbolicValue,
    },

    /// Collect an iterator into a vector.
    Collect {
        /// The iterator to be collected
        iterator: SymbolicValue,
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

    /// Perform a conditional statement
    IfElse {
        /// The condition.
        condition: SymbolicValue,

        /// The value if the condition is true.
        if_branch: SymbolicValue,

        /// The value if the condition is false.
        else_branch: SymbolicValue,
    },

    /// Take the boolean AND of two values
    And {
        lhs: SymbolicValue,
        rhs: SymbolicValue,
    },

    /// Take the boolean OR of two values
    Or {
        lhs: SymbolicValue,
        rhs: SymbolicValue,
    },

    /// Flip a boolean value
    Not { arg: SymbolicValue },

    /// Check if a value is well-defined.
    IsSome(SymbolicValue),

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
    ReadPrim {
        ptr: SymbolicValue,
        prim_type: RuntimePrimType,
    },

    /// Read a value from memory
    ///
    /// Given a pointer to a location in the remote process, read a
    /// value at that location.
    ReadBytes {
        ptr: SymbolicValue,
        num_bytes: SymbolicValue,
    },

    /// Cast from byte array to a primitive value
    CastBytes {
        bytes: SymbolicValue,
        offset: SymbolicValue,
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

struct RewriteResults {
    /// The resulting graph
    graph: SymbolicGraph,

    /// The number of terms rewritten by the GraphRewrite rule.
    num_rewritten_terms: usize,

    /// The number of terms which have been updated to reference a
    /// rewritten term, directly or indirectly.
    num_terms_with_new_inputs: usize,
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
    interactive_substeps: bool,
    reader: Option<CachedReader<'b>>,
    optimize_symbolic_graph: bool,
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
            "Pointer" | "ptr" | "Ptr" => Some(RuntimePrimType::Ptr),
            _ => None,
        }
    }
}

impl StaticField {
    pub(crate) fn method_table_and_field<'a>(
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

    pub fn num_extern_funcs(&self) -> usize {
        self.extern_funcs.len()
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

    pub fn none(&mut self) -> SymbolicValue {
        self.push(ExprKind::None)
    }

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

    pub fn filter(
        &mut self,
        iterator: SymbolicValue,
        filter: SymbolicValue,
    ) -> SymbolicValue {
        self.push(ExprKind::Filter { iterator, filter })
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

    pub fn collect(&mut self, iterator: SymbolicValue) -> SymbolicValue {
        self.push(ExprKind::Collect { iterator })
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
        let indices = indices.into_iter().map(Into::into).collect();
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

    pub fn named_native_function<Func, ArgList>(
        &mut self,
        name: impl Into<String>,
        func: Func,
    ) -> Result<SymbolicValue, Error>
    where
        WrappedNativeFunction<Func, ArgList>: NativeFunction,
        WrappedNativeFunction<Func, ArgList>: 'static,
    {
        let func = self.native_function(func);
        self.name(func, name.into())?;
        Ok(func)
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

    binary_op! {boolean_and, And}
    binary_op! {boolean_or, Or}
    pub fn boolean_not(
        &mut self,
        arg: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let arg = arg.into();
        self.push(ExprKind::Not { arg })
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
        self.push(ExprKind::ReadPrim { ptr, prim_type })
    }

    pub fn read_bytes(
        &mut self,
        ptr: impl Into<SymbolicValue>,
        num_bytes: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let ptr = ptr.into();
        let num_bytes = num_bytes.into();
        self.push(ExprKind::ReadBytes { ptr, num_bytes })
    }

    pub fn cast_bytes(
        &mut self,
        bytes: SymbolicValue,
        offset: impl Into<SymbolicValue>,
        prim_type: RuntimePrimType,
    ) -> SymbolicValue {
        let offset = offset.into();
        self.push(ExprKind::CastBytes {
            bytes,
            offset,
            prim_type,
        })
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

    pub(crate) fn copy_first_param(
        &mut self,
        func: SymbolicValue,
    ) -> SymbolicValue {
        let SymbolicValue::Result(func) = func else {
            panic!(
                "Internal error, \
                 SymbolicValue should point to function \
                 for copy_first_param"
            )
        };
        let params = match &self[func].kind {
            ExprKind::Function { params, .. } => params,
            ExprKind::NativeFunction(func) => {
                let sig = func.signature().unwrap();
                match sig {
                    RuntimeType::Function(FunctionType { params, .. }) => {
                        let param_ty = params
                            .map(|params| params[0].clone())
                            .unwrap_or(RuntimeType::Unknown);
                        return self.function_arg(param_ty);
                    }
                    _ => panic!(
                        "Internal error, \
                         NativeFunction should return TunctionType"
                    ),
                }
            }
            _ => panic!(
                "Internal error, \
                 SymbolicValue should point to function \
                 for copy_first_param"
            ),
        };

        let SymbolicValue::Result(first_param_index) = params[0] else {
            panic!(
                "Internal error, \
                 All function parameters \
                 should point to FunctionArg"
            )
        };

        let first_param = &self[first_param_index];

        let ExprKind::FunctionArg(param_ty) = &first_param.kind else {
            panic!(
                "Internal error, \
                 All function parameters \
                 should point to FunctionArg"
            )
        };

        let opt_name = first_param.name.clone();
        let new_param = self.function_arg(param_ty.clone());
        if let Some(name) = opt_name {
            self.name(new_param, name).expect(
                "Internal error, \
                 Existing name must already be valid",
            );
        }

        new_param
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
            .reachable(self.iter_extern_funcs())
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
        let reachable = self.reachable(self.iter_extern_funcs());

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

    /// For each expression, determine which function owns it.
    ///
    /// `Scope::Function(func_index)`: The expression uses at least
    /// one parameter from the function declared at `func_index`, and
    /// the expression is used by the function's output.
    ///
    /// `Scope::IfBranch(if_else_index)`: The expression is used by
    /// the if branch of the `ExprKind::IfElse` declared at
    /// `if_else_index`, and is not used by any other

    pub(crate) fn operation_scope(&self, reachable: &[bool]) -> Vec<Scope> {
        assert_eq!(reachable.len(), self.ops.len());

        let mut outermost_legal_scope = vec![Scope::Global; self.ops.len()];

        // Step 1: Visit each function in reverse order of
        // declaration.  For each function, mark all expressions that
        // are part of the subgraph defined by the function parameters
        // and output.  This handles nested scopes, since inner-scoped
        // functions will be visited after their containing scope.
        //
        // This step produces a valid scope assignment, preferentially
        // placing expressions in the outermost scope that may legally
        // be applied.
        self.iter_ops()
            .rev()
            .filter(|(OpIndex(i), _)| reachable[*i])
            .for_each(|(func_index, op)| match &op.kind {
                ExprKind::Function { params, output } => {
                    self.collect_subgraph(
                        params.iter().cloned(),
                        Some(*output),
                    )
                    .into_iter()
                    .for_each(|index| {
                        outermost_legal_scope[index.0] =
                            Scope::Function(func_index);
                    });
                }
                _ => {}
            });

        // Step 2: Visit each expression in reverse order of
        // declaration.  For each expression, claim the inputs as
        // being in the same scope as the expression.  If the inputs
        // are already claimed by another scope, resolve the two
        // claims with the innermost scope that contains both claims.
        //
        // This step produces a valid scope assignment, preferentially
        // placing expressions in the innermost scope that may legally
        // be applied.
        let mut innermost_legal_scope: Vec<Option<Scope>> =
            vec![None; self.ops.len()];
        self.iter_extern_funcs().for_each(|OpIndex(i)| {
            innermost_legal_scope[i] = Some(Scope::Global);
        });

        for current_index in (0..self.ops.len()).rev().filter(|i| reachable[*i])
        {
            let current_op = OpIndex(current_index);
            let opt_current_scope = innermost_legal_scope[current_index];

            let mut mark_scope =
                |value: SymbolicValue, new_scope: Option<Scope>| {
                    let Some(new_scope) = new_scope else {
                        return;
                    };
                    let Some(upstream_op) = value.as_op_index() else {
                        return;
                    };
                    let OpIndex(upstream_index) = upstream_op;

                    if matches!(
                        self[upstream_op].kind,
                        ExprKind::FunctionArg(_)
                    ) && !matches!(
                        self[current_op].kind,
                        ExprKind::Function { .. }
                    ) {
                        return;
                    }

                    innermost_legal_scope[upstream_index] =
                        match innermost_legal_scope[upstream_index] {
                            None => Some(new_scope),
                            Some(prev_scope) if prev_scope == new_scope => {
                                Some(prev_scope)
                            }
                            Some(prev_scope) => {
                                let iter_parent_scopes = |scope: Scope| {
                                    std::iter::successors(
                                        Some(scope),
                                        |scope| {
                                            scope.op_index().and_then(
                                                |OpIndex(j)| {
                                                    innermost_legal_scope[j]
                                                },
                                            )
                                        },
                                    )
                                    .collect::<Vec<_>>()
                                    .into_iter()
                                    .rev()
                                };

                                let joint_scope = iter_parent_scopes(prev_scope)
                                    .zip(iter_parent_scopes(new_scope))
                                    .take_while(|(a, b)| a == b)
                                    .map(|(a, _)| a)
                                    .last()
                                    .expect(
                                        "All expressions are within the Global scope",
                                    );

                                Some(joint_scope)
                            }
                        };
                };

            match &self[current_op].kind {
                ExprKind::Function { params, output } => {
                    let scope = Some(Scope::Function(current_op));
                    mark_scope(*output, scope);
                    params
                        .iter()
                        .cloned()
                        .for_each(|param| mark_scope(param, scope));
                }
                &ExprKind::IfElse {
                    condition,
                    if_branch,
                    else_branch,
                } => {
                    mark_scope(condition, opt_current_scope);
                    mark_scope(if_branch, Some(Scope::IfBranch(current_op)));
                    mark_scope(
                        else_branch,
                        Some(Scope::ElseBranch(current_op)),
                    );
                }
                other => other.visit_reachable_nodes(|value| {
                    mark_scope(value, opt_current_scope)
                }),
            }
        }

        // Step 3: Combine the results of steps (1) and (2)
        //
        // To minimize the amount of duplicate evaluations performed
        // in function evaluation, especially reductions, prefer to
        // have as few expressions as possible within the body of a
        // funciton.  To minimize the amount of unused evaluations in
        // branching expressions, prefer to have as many expressions
        // as possible within the body of conditional branches.

        let scope: Vec<Scope> = innermost_legal_scope
            .iter()
            .cloned()
            .zip(outermost_legal_scope.iter().cloned())
            .enumerate()
            .map(|(i,(opt_scope, func_scope))| {
                opt_scope
                    .map(|mut scope| {
                        loop {
                            if scope == func_scope {
                                break;
                            }
                            if let Scope::Function(func_index) = scope {
                                let parent_scope = innermost_legal_scope[func_index.0].unwrap_or(outermost_legal_scope[func_index.0]);
                                assert_ne!(scope,parent_scope,
                                           "Loop in scopes, expr {} in scope {scope:?}",
                                           IndexPrinter::new(OpIndex(i),self)
                                );
                                scope = parent_scope;
                            } else {
                                break;
                            }
                        }
                        scope
                    })
                    .unwrap_or(func_scope)
            })
            .collect();

        scope
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
    #[allow(dead_code)]
    pub(crate) fn dominators(&self) -> Vec<Option<OpIndex>> {
        let mut parent = vec![0; self.ops.len() + 1];
        let mut dfs_order_to_op_order = vec![0; self.ops.len() + 1];
        let mut op_order_to_dfs_order = vec![0; self.ops.len()];

        {
            let mut visited: HashSet<usize> = HashSet::new();
            let mut to_visit: Vec<(usize, usize)> = self
                .iter_extern_funcs()
                .map(|OpIndex(index)| (index + 1, 0))
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
        self.iter_extern_funcs()
            .map(|old_index| {
                let new_index = lookup
                    .get(&old_index)
                    .map(|new_value| {
                        new_value.as_op_index().ok_or(
                            Error::AttemptedToMarkNonFunctionAsExternFunc,
                        )
                    })
                    .transpose()?
                    .unwrap_or(old_index);
                Ok(new_index)
            })
            .collect()
    }

    fn rewrite_verbose(
        &self,
        rewriter: impl GraphRewrite,
    ) -> Result<RewriteResults, Error> {
        rewriter.init();

        let mut num_rewritten_terms = 0;
        let mut num_terms_with_new_inputs = 0;

        let mut prev_index_lookup: HashMap<OpIndex, SymbolicValue> =
            HashMap::new();
        let mut builder = Self::new();

        for (old_index, op) in self.iter_ops() {
            let opt_remapped = op.kind.try_remap(&prev_index_lookup);
            let kind = if let Some(remapped) = &opt_remapped {
                num_terms_with_new_inputs += 1;
                remapped
            } else {
                &op.kind
            };

            let opt_value = rewriter.rewrite_expr(&mut builder, kind)?;
            let value = if let Some(value) = opt_value {
                num_rewritten_terms += 1;
                value
            } else if let Some(remapped) = opt_remapped {
                builder.push(remapped)
            } else {
                builder.push(op.clone())
            };

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
            if value != SymbolicValue::Result(old_index) {
                prev_index_lookup.insert(old_index, value);
            }
        }

        builder.extern_funcs = self.remap_extern_funcs(&prev_index_lookup)?;

        Ok(RewriteResults {
            graph: builder,
            num_rewritten_terms,
            num_terms_with_new_inputs,
        })
    }

    pub fn rewrite(&self, rewriter: impl GraphRewrite) -> Result<Self, Error> {
        Ok(self.rewrite_verbose(rewriter)?.graph)
    }

    fn rewrite_subtree(
        &mut self,
        rewriter: impl GraphRewrite,
        indices: impl Iterator<Item = OpIndex>,
        rewrites_applied: &mut HashMap<OpIndex, SymbolicValue>,
    ) -> Result<(), Error> {
        rewriter.init();

        for old_index in indices {
            let op = self[old_index].clone();
            let opt_remapped = op.kind.try_remap(rewrites_applied);
            let kind = opt_remapped.as_ref().unwrap_or(&op.kind);

            let opt_new_value =
                if let Some(rewritten) = rewriter.rewrite_expr(self, kind)? {
                    if let Some(name) = &op.name {
                        if let SymbolicValue::Result(new_index) = rewritten {
                            if self[new_index].name.is_none() {
                                self.name(rewritten, name)?;
                            }
                        }
                    }
                    Some(rewritten)
                } else if let Some(remapped) = opt_remapped {
                    let expr = Expr {
                        kind: remapped,
                        name: op.name,
                    };

                    Some(self.push(expr))
                } else {
                    None
                };

            if let Some(new_value) = opt_new_value {
                rewrites_applied.insert(old_index, new_value);
            }
        }

        Ok(())
    }

    pub fn substitute(
        &mut self,
        mut replacements: HashMap<OpIndex, SymbolicValue>,
        value: SymbolicValue,
    ) -> Result<Option<SymbolicValue>, Error> {
        let Some(index) = value.as_op_index() else {
            return Ok(None);
        };
        if replacements.is_empty() {
            return Ok(None);
        }

        let subgraph = loop {
            let subgraph = self.collect_subgraph(
                replacements.iter().map(|(key, _)| (*key).into()),
                Some(value),
            );

            let mut must_also_replace_function = false;

            subgraph
                .iter()
                .cloned()
                .filter(|index| !replacements.contains_key(index))
                .filter_map(|index| match &self[index].kind {
                    ExprKind::Function { params, .. } => Some(params),
                    _ => None,
                })
                .flatten()
                .filter_map(|value| value.as_op_index())
                .filter(|index| !replacements.contains_key(index))
                .collect::<Vec<_>>()
                .into_iter()
                .for_each(|index| {
                    let ExprKind::FunctionArg(arg_ty) = &self[index].kind
                    else {
                        unreachable!(
                            "Ill-formed SymbolicGraph, \
                             function params must point to FunctionArg."
                        )
                    };
                    let new_arg = self.function_arg(arg_ty.clone());
                    if let Some(name) = self[index].name.clone() {
                        self.name(new_arg, name)
                            .expect("Existing name must be valid");
                    }

                    must_also_replace_function = true;
                    replacements.insert(index, new_arg);
                });

            if !must_also_replace_function {
                break subgraph;
            }
        };

        let to_rewrite = subgraph
            .into_iter()
            .filter(|index| !replacements.contains_key(index));

        let mut rewrites = HashMap::new();

        self.rewrite_subtree(
            Substitute(&replacements),
            to_rewrite,
            &mut rewrites,
        )?;

        return Ok(rewrites.get(&index).cloned());

        struct Substitute<'a>(&'a HashMap<OpIndex, SymbolicValue>);

        impl GraphRewrite for Substitute<'_> {
            fn rewrite_expr(
                &self,
                graph: &mut SymbolicGraph,
                expr: &ExprKind,
            ) -> Result<Option<SymbolicValue>, Error> {
                Ok(expr.try_remap(self.0).map(|remapped| graph.push(remapped)))
            }
        }
    }

    /// Determine which arguments
    pub(crate) fn undefined_args(
        &self,
        initial: impl IntoIterator<Item = SymbolicValue>,
    ) -> Vec<OpIndex> {
        enum VisitItem {
            PreVisit(OpIndex),
            RemoveDefinition(OpIndex),
        }

        let mut to_visit: Vec<_> = initial
            .into_iter()
            .filter_map(|value| value.as_op_index())
            .map(VisitItem::PreVisit)
            .collect();

        let mut used_without_definition = HashSet::<OpIndex>::new();
        let mut currently_defined = HashSet::<OpIndex>::new();

        while let Some(visiting) = to_visit.pop() {
            match visiting {
                VisitItem::PreVisit(op_index) => match &self[op_index].kind {
                    ExprKind::FunctionArg(_) => {
                        if !currently_defined.contains(&op_index) {
                            used_without_definition.insert(op_index);
                        }
                    }
                    ExprKind::Function { params, output } => {
                        params.iter().filter_map(|p| p.as_op_index()).for_each(
                            |param_index| {
                                assert!(
                                    !currently_defined.contains(&param_index)
                                );
                                currently_defined.insert(param_index);
                                to_visit.push(VisitItem::RemoveDefinition(
                                    param_index,
                                ));
                            },
                        );
                        if let Some(out_index) = output.as_op_index() {
                            to_visit.push(VisitItem::PreVisit(out_index));
                        }
                    }
                    other => {
                        other.visit_reachable_nodes(|value| {
                            if let Some(prev_index) = value.as_op_index() {
                                to_visit.push(VisitItem::PreVisit(prev_index));
                            }
                        });
                    }
                },
                VisitItem::RemoveDefinition(op_index) => {
                    currently_defined.remove(&op_index);
                }
            }
        }

        used_without_definition.into_iter().sorted().collect()
    }

    /// Determine which expressions are used by some expression.
    ///
    /// Given a set of initial expressions, returns a boolean vector
    /// of size `self.num_operations()`.  If one or more of the
    /// initial expressions depends on an operation, the boolean
    /// vector will contain `true` for that element.  Otherwise, the
    /// boolean vector will contain `false`.
    pub(crate) fn reachable(
        &self,
        initial: impl IntoIterator<Item = OpIndex>,
    ) -> Vec<bool> {
        let mut to_visit: Vec<_> = initial.into_iter().collect();

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

        let reachable = self.reachable(self.iter_extern_funcs());

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

impl Scope {
    pub(crate) fn op_index(&self) -> Option<OpIndex> {
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
        let get_env_var = |name: &'static str| -> bool {
            std::env::var(name)
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
                .unwrap_or(false)
        };
        let show_steps = get_env_var("SHOW_STEPS");
        let interactive_substeps = get_env_var("INTERACTIVE_SUBSTEPS");

        Self {
            graph,
            show_steps,
            interactive_substeps,
            reader: None,
            optimize_symbolic_graph: true,
        }
    }

    pub fn show_each_step(self, show_steps: bool) -> Self {
        Self { show_steps, ..self }
    }

    pub fn interactive_substeps(self, interactive_substeps: bool) -> Self {
        Self {
            interactive_substeps,
            ..self
        }
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

    fn apply_rewrites(
        &self,
        mut expr: SymbolicGraph,
        rewriter: impl GraphRewrite,
    ) -> Result<SymbolicGraph, Error> {
        if self.interactive_substeps {
            let rewriter = rewriter.apply_once();

            println!("--------- Initial ------------\n{expr}");

            for i_update in 1.. {
                std::io::stdin()
                    .read_line(&mut String::new())
                    .expect("Error reading stdin");

                let rewrite_details = expr.rewrite_verbose(&rewriter)?;

                if rewrite_details.num_rewritten_terms == 0 {
                    assert!(rewrite_details.num_terms_with_new_inputs == 0);
                    break;
                } else {
                    expr = rewrite_details.graph;
                }

                println!(
                    "--------- After {i_update} Updates ------------\n{}",
                    expr.printer().expand_all_expressions(),
                );

                expr.validate(None)?;
            }

            Ok(expr)
        } else {
            expr.rewrite(rewriter.apply_recursively())
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

        let analysis = Analysis::new(reader);

        let optional_optimizations = super::ConstantFold
            .then(super::RemoveUnusedDowncast(&analysis))
            .then(super::RemoveUnusedPrimcast(&analysis))
            .then(super::RemoveUnusedPointerCast);

        let mandatory_lowering = super::InferFunctionParameterTypes(&analysis)
            .then(super::LegalizeOperandTypes(&analysis))
            .then(super::InlineFunctionCalls)
            .then(super::MergeRangeReduceToSimpleReduce)
            .then(super::InlineIteratorMap)
            .then(super::InlineIteratorFilter)
            .then(super::ConvertCollectToReduce(&analysis))
            .then(super::ConvertBooleanOperatorToConditional)
            .then(super::LowerSymbolicExpr(&analysis));

        let expr = if self.optimize_symbolic_graph {
            self.apply_rewrites(
                expr,
                optional_optimizations.then(mandatory_lowering),
            )?
        } else {
            self.apply_rewrites(expr, mandatory_lowering)?
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

        Ok(vm)
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
            ExprKind::None
            | ExprKind::NativeFunction(_)
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
            ExprKind::Filter { iterator, filter } => {
                let opt_iterator = remap(iterator);
                let opt_filter = remap(filter);
                (opt_iterator.is_some() || opt_filter.is_some()).then(|| {
                    let iterator = opt_iterator.unwrap_or_else(|| *iterator);
                    let filter = opt_filter.unwrap_or_else(|| *filter);
                    ExprKind::Filter { iterator, filter }
                })
            }
            ExprKind::Collect { iterator } => {
                remap(iterator).map(|iterator| ExprKind::Collect { iterator })
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

            ExprKind::Not { arg } => {
                remap(arg).map(|arg| ExprKind::Not { arg })
            }

            ExprKind::And { lhs, rhs } => handle_binary_op!(And, lhs, rhs),
            ExprKind::Or { lhs, rhs } => handle_binary_op!(Or, lhs, rhs),

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
            ExprKind::ReadPrim { ptr, prim_type } => {
                remap(ptr).map(|ptr| ExprKind::ReadPrim {
                    ptr,
                    prim_type: prim_type.clone(),
                })
            }
            ExprKind::ReadBytes { ptr, num_bytes } => {
                let opt_ptr = remap(ptr);
                let opt_num_bytes = remap(num_bytes);
                let requires_remap =
                    opt_ptr.is_some() || opt_num_bytes.is_some();
                requires_remap.then(|| {
                    let ptr = opt_ptr.unwrap_or_else(|| *ptr);
                    let num_bytes = opt_num_bytes.unwrap_or_else(|| *num_bytes);
                    ExprKind::ReadBytes { ptr, num_bytes }
                })
            }
            ExprKind::CastBytes {
                bytes,
                offset,
                prim_type,
            } => {
                let opt_bytes = remap(bytes);
                let opt_offset = remap(offset);
                let requires_remap =
                    opt_bytes.is_some() || opt_offset.is_some();
                requires_remap.then(|| {
                    let bytes = opt_bytes.unwrap_or_else(|| *bytes);
                    let offset = opt_offset.unwrap_or_else(|| *offset);
                    ExprKind::CastBytes {
                        bytes,
                        offset,
                        prim_type: *prim_type,
                    }
                })
            }
            ExprKind::ReadString { ptr } => {
                remap(ptr).map(|ptr| ExprKind::ReadString { ptr })
            }
        }
    }

    pub(crate) fn iter_input_values(
        &self,
    ) -> impl Iterator<Item = SymbolicValue> + '_ {
        let (static_inputs, dynamic_inputs): (_, Option<&[SymbolicValue]>) =
            match self {
                // No upstream inputs
                ExprKind::None
                | ExprKind::NativeFunction(_)
                | ExprKind::FunctionArg(_)
                | ExprKind::StaticField(_) => ([None, None, None], None),

                // Dynamic number of upstream inputs
                ExprKind::Function { params, output } => {
                    ([Some(*output), None, None], Some(&params))
                }
                ExprKind::FunctionCall { func, args } => {
                    ([Some(*func), None, None], Some(&args))
                }
                ExprKind::Tuple(items) => ([None, None, None], Some(&items)),
                ExprKind::IndexAccess { obj, indices } => {
                    ([Some(*obj), None, None], Some(&indices))
                }

                // One upstream input
                ExprKind::Range { extent: value }
                | ExprKind::Collect { iterator: value }
                | ExprKind::FieldAccess { obj: value, .. }
                | ExprKind::SymbolicDowncast { obj: value, .. }
                | ExprKind::NumArrayElements { array: value }
                | ExprKind::PointerCast { ptr: value, .. }
                | ExprKind::IsSome(value)
                | ExprKind::Not { arg: value }
                | ExprKind::PrimCast { value, .. }
                | ExprKind::PhysicalDowncast { obj: value, .. }
                | ExprKind::ReadPrim { ptr: value, .. }
                | ExprKind::ReadString { ptr: value } => {
                    ([Some(*value), None, None], None)
                }

                // Two upstreams inputs
                &ExprKind::Map { iterator, map } => {
                    ([Some(iterator), Some(map), None], None)
                }
                &ExprKind::Filter { iterator, filter } => {
                    ([Some(iterator), Some(filter), None], None)
                }
                &ExprKind::ArrayExtent { array, dim } => {
                    ([Some(array), Some(dim), None], None)
                }
                &ExprKind::ReadBytes { ptr, num_bytes } => {
                    ([Some(ptr), Some(num_bytes), None], None)
                }
                &ExprKind::CastBytes { bytes, offset, .. } => {
                    ([Some(bytes), Some(offset), None], None)
                }

                // Binary operators
                &ExprKind::And { lhs, rhs }
                | &ExprKind::Or { lhs, rhs }
                | &ExprKind::Equal { lhs, rhs }
                | &ExprKind::NotEqual { lhs, rhs }
                | &ExprKind::LessThan { lhs, rhs }
                | &ExprKind::GreaterThan { lhs, rhs }
                | &ExprKind::LessThanOrEqual { lhs, rhs }
                | &ExprKind::GreaterThanOrEqual { lhs, rhs }
                | &ExprKind::Add { lhs, rhs }
                | &ExprKind::Mul { lhs, rhs }
                | &ExprKind::Div { lhs, rhs }
                | &ExprKind::Mod { lhs, rhs } => {
                    ([Some(lhs), Some(rhs), None], None)
                }

                // Three upstreams inputs
                &ExprKind::Reduce {
                    initial,
                    iterator,
                    reduction,
                } => ([Some(initial), Some(iterator), Some(reduction)], None),
                &ExprKind::SimpleReduce {
                    initial,
                    extent,
                    reduction,
                } => ([Some(initial), Some(extent), Some(reduction)], None),
                &ExprKind::IfElse {
                    condition,
                    if_branch,
                    else_branch,
                } => (
                    [Some(condition), Some(if_branch), Some(else_branch)],
                    None,
                ),
            };

        let iter_static =
            static_inputs.into_iter().filter_map(|opt_value| opt_value);
        let iter_dynamic = dynamic_inputs.into_iter().flatten().cloned();
        iter_static.chain(iter_dynamic)
    }

    pub(crate) fn iter_input_nodes(
        &self,
    ) -> impl Iterator<Item = OpIndex> + '_ {
        self.iter_input_values()
            .filter_map(|value| value.as_op_index())
    }

    pub(crate) fn visit_reachable_nodes(
        &self,
        callback: impl FnMut(SymbolicValue),
    ) {
        self.iter_input_values().for_each(callback)
    }

    pub(crate) fn op_name(&self) -> &'static str {
        match self {
            ExprKind::None => "None",
            ExprKind::FunctionArg { .. } => "FunctionArg",
            ExprKind::Function { .. } => "Function",
            ExprKind::FunctionCall { .. } => "FunctionCall",
            ExprKind::Tuple { .. } => "Tuple",
            ExprKind::NativeFunction { .. } => "NativeFunction",
            ExprKind::Range { .. } => "Range",
            ExprKind::Map { .. } => "Map",
            ExprKind::Filter { .. } => "Filter",
            ExprKind::Collect { .. } => "Collect",
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
            ExprKind::And { .. } => "And",
            ExprKind::Or { .. } => "Or",
            ExprKind::Not { .. } => "Not",
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
            ExprKind::ReadPrim { .. } => "ReadValue",
            ExprKind::ReadBytes { .. } => "ReadBytes",
            ExprKind::CastBytes { .. } => "CastBytes",
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

        for (lhs_extern_func, rhs_extern_func) in
            lhs.iter_extern_funcs().zip(rhs.iter_extern_funcs())
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
                ExprKind::None => matches!(rhs_kind, ExprKind::None),

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
                ExprKind::Filter {
                    iterator: lhs_iterator,
                    filter: lhs_filter,
                } => match rhs_kind {
                    ExprKind::Filter {
                        iterator: rhs_iterator,
                        filter: rhs_filter,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_filter, rhs_filter)
                    }
                    _ => false,
                },
                ExprKind::Collect {
                    iterator: lhs_iterator,
                } => match rhs_kind {
                    ExprKind::Collect {
                        iterator: rhs_iterator,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
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

                ExprKind::And { lhs, rhs } => {
                    handle_binary_op!(And, lhs, rhs)
                }
                ExprKind::Or { lhs, rhs } => {
                    handle_binary_op!(Or, lhs, rhs)
                }
                ExprKind::Not { arg: lhs_arg } => match rhs_kind {
                    ExprKind::Not { arg: rhs_arg } => {
                        equivalent_value!(lhs_arg, rhs_arg)
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
                ExprKind::ReadPrim {
                    ptr: lhs_ptr,
                    prim_type: lhs_prim_type,
                } => match rhs_kind {
                    ExprKind::ReadPrim {
                        ptr: rhs_ptr,
                        prim_type: rhs_prim_type,
                    } => {
                        equivalent_value!(lhs_ptr, rhs_ptr)
                            && lhs_prim_type == rhs_prim_type
                    }
                    _ => false,
                },
                ExprKind::ReadBytes {
                    ptr: lhs_ptr,
                    num_bytes: lhs_num_bytes,
                } => match rhs_kind {
                    ExprKind::ReadBytes {
                        ptr: rhs_ptr,
                        num_bytes: rhs_num_bytes,
                    } => {
                        equivalent_value!(lhs_ptr, rhs_ptr)
                            && equivalent_value!(lhs_num_bytes, rhs_num_bytes)
                    }
                    _ => false,
                },
                ExprKind::CastBytes {
                    bytes: lhs_bytes,
                    offset: lhs_offset,
                    prim_type: lhs_prim_type,
                } => match rhs_kind {
                    ExprKind::CastBytes {
                        bytes: rhs_bytes,
                        offset: rhs_offset,
                        prim_type: rhs_prim_type,
                    } => {
                        equivalent_value!(lhs_bytes, rhs_bytes)
                            && equivalent_value!(lhs_offset, rhs_offset)
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
            ExprKind::None => write!(f, "None"),

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
            ExprKind::Filter { iterator, filter } => {
                write!(f, "{iterator}.filter({filter})")
            }
            ExprKind::Collect { iterator } => {
                write!(f, "{iterator}.collect()")
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

            ExprKind::And { lhs, rhs } => write!(f, "{lhs} && {rhs}"),
            ExprKind::Or { lhs, rhs } => write!(f, "{lhs} || {rhs}"),
            ExprKind::Not { arg } => write!(f, "!{arg}"),

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
            ExprKind::ReadPrim { ptr, prim_type } => {
                write!(f, "{ptr}.read_value::<{prim_type}>()")
            }
            ExprKind::ReadBytes { ptr, num_bytes } => {
                write!(f, "{ptr}.read_bytes({ptr}, {num_bytes})")
            }
            ExprKind::CastBytes {
                bytes,
                offset,
                prim_type,
            } => {
                write!(f, "{bytes}.cast_bytes::<{prim_type}>({offset})")
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
