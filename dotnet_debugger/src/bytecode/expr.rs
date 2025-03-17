use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
    rc::Rc,
};

use derive_more::derive::From;
use itertools::Itertools as _;

use format_utils::Indent;
use iterator_extensions::ResultIteratorExt as _;
use memory_reader::Pointer;

use crate::{
    bytecode::virtual_machine::{
        FunctionIndex, Instruction, StackIndex, VMArg,
    },
    runtime_type::RuntimePrimType,
    CachedReader, Error, FieldDescription, MethodTable, OpIndex,
    RuntimePrimValue, RuntimeType, TypedPointer, VirtualMachine,
};

use super::{
    graph_rewrite::Analysis, native_function::WrappedNativeFunction,
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

#[derive(Clone)]
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
    NativeFunction(Rc<dyn NativeFunction>),

    /// An iterator that starts at zero, has `extent` elements, each
    /// increasing by one.
    Range { extent: SymbolicValue },

    /// Perform a reduction along an iterator.
    Reduce {
        /// The initial value of the reduction.
        initial: SymbolicValue,

        /// The iterator over which the reduction should be performed.
        iterator: SymbolicValue,

        /// The reduction function.  Should have signature
        /// `Fn(TResult, `Item) -> TResult`
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

    /// The number of spaces to indent for the body of functions.
    indent_width: usize,
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

pub struct GraphComparison<'a> {
    lhs: &'a SymbolicGraph,
    rhs: &'a SymbolicGraph,
    order_dependent: bool,
    compare_names: bool,
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
            SymbolicValue::Int(_) | SymbolicValue::Ptr(_) => {
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

    const DEFAULT_INDENT_WIDTH: usize = 4;

    pub fn printer<'a>(&'a self) -> GraphPrinter<'a> {
        GraphPrinter {
            graph: self,
            expand_all_expressions: false,
            root_subgraph_node: None,
            insert_zero_width_space_at_breakpoint: false,
            indent_width: Self::DEFAULT_INDENT_WIDTH,
        }
    }

    pub fn print<'a>(&'a self, value: SymbolicValue) -> GraphPrinter<'a> {
        GraphPrinter {
            graph: self,
            root_subgraph_node: Some(value),
            expand_all_expressions: false,
            insert_zero_width_space_at_breakpoint: false,
            indent_width: Self::DEFAULT_INDENT_WIDTH,
        }
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

    pub fn native_function<Func, ArgList>(
        &mut self,
        func: Func,
    ) -> SymbolicValue
    where
        WrappedNativeFunction<Func, ArgList>: NativeFunction,
        WrappedNativeFunction<Func, ArgList>: 'static,
    {
        let wrapped = WrappedNativeFunction::new(func);
        self.push(ExprKind::NativeFunction(Rc::new(wrapped)))
    }

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

    pub fn validate(
        &self,
        reader: Option<CachedReader<'_>>,
    ) -> Result<(), Error> {
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
                self[visiting].visit_input_values(|value| mark!(value));
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
            self[index].visit_input_values(|value| {
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
                        remapped.into()
                    } else {
                        op
                    };
                    self.push(expr)
                });
            rewrites_applied.insert(old_index, value);
        }

        Ok(())
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

        let reachable =
            self.reachable(self.extern_funcs.iter().cloned().map(Into::into));

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

        builder.extern_funcs = self.remap_extern_funcs(&prev_index_lookup)?;

        Ok(builder)
    }

    pub fn eliminate_common_subexpressions(self) -> Result<Self, Error> {
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

                // Temporary workaround.  The long-term fix is to make
                // update the hashing so that FunctionArg have
                // structural equality when encountering points of
                // definition, but reference equality when
                // encountering points of use (that haven't already
                // been defined, that is).
                if !matches!(new_kind, ExprKind::FunctionArg(_)) {
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

    pub fn to_virtual_machine(&self) -> Result<VirtualMachine, Error> {
        let mut instructions = Vec::new();
        let mut currently_stored: HashMap<OpIndex, VMArg> = HashMap::new();
        let mut previously_consumed: HashSet<OpIndex> = HashSet::new();

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

        let mut next_free_index = outputs.len();
        let mut native_functions: Vec<Rc<dyn NativeFunction>> = Vec::new();
        let mut native_function_lookup =
            HashMap::<OpIndex, FunctionIndex>::new();

        let output_lookup: HashMap<OpIndex, StackIndex> = {
            let mut output_lookup = HashMap::new();
            for (i, output) in outputs.iter().cloned().enumerate() {
                let stack_index = StackIndex(i);
                match output {
                    SymbolicValue::Result(op_index) => {
                        output_lookup.insert(op_index, stack_index);
                    }
                    SymbolicValue::Int(value) => {
                        let value = RuntimePrimValue::NativeUInt(value).into();
                        instructions.push(Instruction::Copy {
                            value,
                            output: stack_index,
                        });
                    }
                    SymbolicValue::Ptr(ptr) => {
                        let value = RuntimePrimValue::Ptr(ptr).into();
                        instructions.push(Instruction::Copy {
                            value,
                            output: stack_index,
                        });
                    }
                }
            }
            output_lookup
        };

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
                        if previously_consumed.contains(&op_index) {
                            return Err(Error::AttemptedUseOfConsumedValue);
                        }
                        currently_stored
                            .get(&op_index)
                            .expect(
                                "Internal error, \
                             {op_index} not located anywhere",
                            )
                            .clone()
                    }
                }
            };
        }

        for (op_index, op) in self.iter_ops() {
            let op_output: StackIndex =
                output_lookup.get(&op_index).cloned().unwrap_or_else(|| {
                    let index = next_free_index;
                    next_free_index += 1;
                    StackIndex(index)
                });

            match op.as_ref() {
                ExprKind::Function { .. } => {
                    assert!(op_index == main_func_index);
                }
                ExprKind::FunctionArg(_) => todo!(),
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
                    if let Some(native_func_index) =
                        native_function_lookup.get(&func)
                    {
                        let mut vm_args = Vec::new();
                        for arg in args {
                            vm_args.push(value_to_arg!(arg));
                        }

                        let mutates_first_argument = native_functions
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
                                SymbolicValue::Int(_) |
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

                            instructions.push(
                                Instruction::NativeFunctionCall {
                                    index: *native_func_index,
                                    args: vm_args,
                                    output: None,
                                },
                            );

                            currently_stored.remove(&first_arg_op);
                            previously_consumed.insert(first_arg_op);

                            if op_output.0 < outputs.len() {
                                instructions.push(Instruction::Swap(
                                    first_arg_loc,
                                    op_output,
                                ));
                                currently_stored
                                    .insert(op_index, op_output.into());
                            } else {
                                currently_stored
                                    .insert(op_index, first_arg_loc.into());
                            }
                        } else {
                            // The function produces an output value, to
                            // be stored in the output index.
                            instructions.push(
                                Instruction::NativeFunctionCall {
                                    index: *native_func_index,
                                    args: vm_args,
                                    output: Some(op_output),
                                },
                            );
                            currently_stored.insert(op_index, op_output.into());
                        }
                    } else {
                        todo!(
                            "Handle IR-defined function calls in the VM.  \
                             Until then, all functions should be inlined."
                        )
                    }
                }

                ExprKind::Range { .. } => todo!(),
                ExprKind::Reduce { .. } => todo!(),
                ExprKind::NativeFunction(func) => {
                    let func_index = FunctionIndex(native_functions.len());
                    native_functions.push(func.clone());
                    native_function_lookup.insert(op_index, func_index);
                }

                ExprKind::Add { lhs, rhs } => {
                    let lhs = value_to_arg!(lhs);
                    let rhs = value_to_arg!(rhs);
                    instructions.push(Instruction::Add {
                        lhs,
                        rhs,
                        output: op_output,
                    });
                    currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::Mul { lhs, rhs } => {
                    let lhs = value_to_arg!(lhs);
                    let rhs = value_to_arg!(rhs);
                    instructions.push(Instruction::Mul {
                        lhs,
                        rhs,
                        output: op_output,
                    });
                    currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::PrimCast { value, prim_type } => {
                    let value = value_to_arg!(value);
                    instructions.push(Instruction::PrimCast {
                        value,
                        prim_type: *prim_type,
                        output: op_output,
                    });
                    currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::PhysicalDowncast { obj, ty } => {
                    let obj = value_to_arg!(obj);
                    instructions.push(Instruction::Downcast {
                        obj,
                        subtype: *ty,
                        output: op_output,
                    });
                    currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::ReadValue { ptr, prim_type } => {
                    let ptr = value_to_arg!(ptr);
                    instructions.push(Instruction::Read {
                        ptr,
                        prim_type: *prim_type,
                        output: op_output,
                    });
                    currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::PointerCast { ptr, .. } => {
                    let ptr = value_to_arg!(ptr);
                    instructions.push(Instruction::Copy {
                        value: ptr,
                        output: op_output,
                    });
                    currently_stored.insert(op_index, ptr);
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

        let mut builder =
            VirtualMachine::builder(instructions).num_outputs(outputs.len());
        for native_func in native_functions.into_iter() {
            builder = builder.with_rc_native_function(native_func);
        }

        Ok(builder.build())
    }

    pub fn compile<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<VirtualMachine, Error> {
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

        let reader = reader.into();
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

        if show_steps {
            println!("----------- Initial DCE --------------\n{expr}");
        }

        let expr = expr.dead_code_elimination()?;
        expr.validate(reader)?;

        if show_steps {
            println!("----------- After DCE --------------\n{expr}");
        }
        let expr = expr.eliminate_common_subexpressions()?;
        expr.validate(reader)?;
        if show_steps {
            println!("----------- After CSE --------------\n{expr}");
        }

        let analysis = Analysis::new(reader);
        let rewriter = super::ConstantFold
            .then(super::RemoveUnusedDowncast(&analysis))
            .then(super::RemoveUnusedPrimcast(&analysis))
            .then(super::LowerSymbolicExpr(&analysis))
            .then(super::RemoveUnusedPointerCast)
            .then(super::InlineFunctionCalls)
            .apply_recursively();

        let expr = expr.rewrite(rewriter)?;
        expr.validate(reader)?;

        if show_steps {
            println!("----------- After Simplifcations --------------\n{expr}");
        }

        let expr = expr.dead_code_elimination()?;
        expr.validate(reader)?;

        if show_steps {
            println!("----------- After DCE --------------\n{expr}");
        }

        let expr = expr.eliminate_common_subexpressions()?;
        expr.validate(reader)?;

        if show_steps {
            println!("----------- After CSE --------------\n{expr}");
        }

        // Virtual machine, in terms of sequential operations.
        let vm = expr.to_virtual_machine()?;

        if show_steps {
            println!("----------- As VM --------------\n{vm}");
        }

        let vm = vm.simplify();
        if show_steps {
            println!("----------- VM (simplified) --------------\n{vm}");
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

        let remap_or_no_op = |value: &SymbolicValue| -> SymbolicValue {
            remap(value).unwrap_or_else(|| *value)
        };

        let vec_requires_remap = |slice: &[SymbolicValue]| -> bool {
            slice.iter().any(|item| match item {
                SymbolicValue::Result(op_index) => map.contains_key(op_index),
                SymbolicValue::Int(_) => false,
                SymbolicValue::Ptr(_) => false,
            })
        };

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
            ExprKind::NativeFunction(_)
            | ExprKind::FunctionArg(_)
            | ExprKind::StaticField(_) => {}
            ExprKind::Function { output, .. } => {
                callback(*output);
            }
            ExprKind::FunctionCall { func, args } => {
                callback(*func);
                args.iter().for_each(|arg| callback(*arg));
            }
            ExprKind::Range { extent } => {
                callback(*extent);
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
                ExprKind::NativeFunction(lhs_func) => match rhs_kind {
                    ExprKind::NativeFunction(rhs_func) => {
                        std::ptr::eq(Rc::as_ptr(lhs_func), Rc::as_ptr(rhs_func))
                    }
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
                ExprKind::Add {
                    lhs: lhs_lhs,
                    rhs: lhs_rhs,
                } => match rhs_kind {
                    ExprKind::Add {
                        lhs: rhs_lhs,
                        rhs: rhs_rhs,
                    } => {
                        equivalent_value!(lhs_lhs, rhs_lhs)
                            && equivalent_value!(lhs_rhs, rhs_rhs)
                    }
                    _ => false,
                },
                ExprKind::Mul {
                    lhs: lhs_lhs,
                    rhs: lhs_rhs,
                } => match rhs_kind {
                    ExprKind::Mul {
                        lhs: rhs_lhs,
                        rhs: rhs_rhs,
                    } => {
                        equivalent_value!(lhs_lhs, rhs_lhs)
                            && equivalent_value!(lhs_rhs, rhs_rhs)
                    }
                    _ => false,
                },
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
            ExprKind::Reduce {
                initial,
                iterator,
                reduction,
            } => {
                write!(f, "{iterator}.reduce({initial}, {reduction})")
            }
            ExprKind::NativeFunction(func) => {
                write!(f, "NativeFunction({:p})", Rc::as_ptr(func))
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
        let reachable = if let Some(root_node) = self.root_subgraph_node {
            self.graph.reachable(std::iter::once(root_node))
        } else {
            vec![true; self.graph.ops.len()]
        };

        let scope: Vec<Option<OpIndex>> = {
            let mut scope = vec![None; self.graph.ops.len()];
            self.graph
                .iter_ops()
                .rev()
                .filter(|(_, op)| matches!(op.kind, ExprKind::Function { .. }))
                .for_each(|(func_index, op)| {
                    let ExprKind::Function { params, output } = &op.kind else {
                        unreachable!("Due to earlier filter")
                    };
                    scope[func_index.0] = Some(func_index);

                    self.graph
                        .collect_subgraph(params.iter().cloned(), Some(*output))
                        .into_iter()
                        .for_each(|index| {
                            scope[index.0] = Some(func_index);
                        });
                });

            scope
        };

        let inline_expr: Vec<bool> = if self.expand_all_expressions {
            vec![false; self.graph.ops.len()]
        } else {
            let mut num_usage = vec![0; self.graph.ops.len()];
            let mut used_by_child_scope = vec![false; self.graph.ops.len()];
            self.graph
                .iter_ops()
                .filter(|(index, _)| reachable[index.0])
                .for_each(|(downstream_index, op)| {
                    op.visit_input_values(|input_value| {
                        if let SymbolicValue::Result(upstream_index) =
                            input_value
                        {
                            num_usage[upstream_index.0] += 1;
                            if scope[upstream_index.0]
                                != scope[downstream_index.0]
                            {
                                used_by_child_scope[upstream_index.0] = true;
                            }
                        }
                    })
                });

            num_usage
                .into_iter()
                .zip(self.graph.ops.iter())
                .zip(used_by_child_scope)
                .map(|((count, op), uses_parent_scope)| {
                    count == 1 && op.name.is_none() && !uses_parent_scope
                })
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

        #[derive(Debug)]
        struct PrintItem {
            kind: PrintItemKind,
            indent: Indent,
        }
        #[derive(Debug)]
        enum PrintItemKind {
            Op(OpIndex),
            FunctionOutput {
                output: SymbolicValue,
                is_extern_func: bool,
            },
        }

        // A stack of operations to print.  If an operation is not yet
        // printable, such as an operation that uses a function
        // argument prior to encountering the function, it will be
        // moved to `delayed_ops` rather than printed.  After it
        // becomes printable, it will be pushed back onto the stack.
        let mut to_print: Vec<PrintItem> = (0..self.graph.ops.len())
            .rev()
            .map(|i| PrintItem {
                kind: PrintItemKind::Op(OpIndex::new(i)),
                indent: Indent(0),
            })
            .collect();

        // Track operations that depend on a function argument.  These
        // shouldn't be printed until encountering the function that
        // owns them.
        let mut delayed_ops: Vec<PrintItem> = Vec::new();
        // Lookup from an operation to the Expr::FunctionArg that it makes use of.
        //
        // TODO: Something feels wrong for the order in which these
        // are looked up.  If there's an expression that uses
        // arguments from two separate functions, then the expression
        // is contained within both functions.  On encountering either
        // function definition, you know that the expression is part
        // of the innermost function.  But I'm only tracking a single
        // index here.
        let mut delayed_op_lookup: HashMap<OpIndex, OpIndex> = HashMap::new();

        #[derive(PartialEq, PartialOrd)]
        enum DelayedNewline {
            None,
            BeforeAssignment,
            BeforeExpression,
        }

        let mut delayed_newline = DelayedNewline::None;

        let make_expr_printer =
            |value: SymbolicValue, is_top_level: bool| ExprPrinter {
                graph: self.graph,
                value,
                is_top_level,
                inline_expr: &inline_expr,
                requires_name_prefix: &requires_name_prefix,
                insert_zero_width_space_at_breakpoint: self
                    .insert_zero_width_space_at_breakpoint,
            };

        let extern_func_lookup: HashSet<OpIndex> =
            self.graph.extern_funcs.iter().cloned().collect();

        while let Some(print_item) = to_print.pop() {
            let indent = print_item.indent;
            let index = match print_item.kind {
                PrintItemKind::Op(index) => index,
                PrintItemKind::FunctionOutput {
                    output,
                    is_extern_func,
                } => {
                    if delayed_newline >= DelayedNewline::BeforeExpression {
                        write!(fmt, "\n{indent}")?;
                    } else {
                        write!(fmt, " ")?;
                    }
                    write!(fmt, "{}", make_expr_printer(output, false))?;

                    if delayed_newline >= DelayedNewline::BeforeExpression {
                        let closing_indent = indent - self.indent_width;
                        write!(fmt, "\n{closing_indent}")?;
                    } else {
                        write!(fmt, " ")?;
                    }

                    write!(fmt, "}}")?;
                    if !is_extern_func {
                        write!(fmt, ";")?;
                    }
                    continue;
                }
            };

            let op = &self.graph[index];

            if !reachable[index.0] {
                // Only display operations that are reachable when
                // starting from the displayed node.
                continue;
            }

            let mut uses_param = None;
            if !matches!(op.kind, ExprKind::Function { .. }) {
                op.visit_input_values(|input| {
                    if let SymbolicValue::Result(op_index) = input {
                        if let Some(function_arg) =
                            delayed_op_lookup.get(&op_index)
                        {
                            uses_param = Some(function_arg);
                        }
                    }
                });
            }
            if let Some(function_arg) = uses_param {
                // Depends on a function argument.  Mark it for
                // printing out later.
                delayed_ops.push(PrintItem {
                    indent: indent + self.indent_width,
                    ..print_item
                });
                delayed_op_lookup.insert(index, *function_arg);
                continue;
            }

            if inline_expr[index.0] {
                // Do not provide a separate printout for expressions that
                // are being displayed inline.
                continue;
            }

            let index_printer = IndexPrinter {
                graph: self.graph,
                index,
                requires_name_prefix: requires_name_prefix[index.0],
            };
            let expr_printer = make_expr_printer(index.into(), true);

            let is_root_node =
                Some(SymbolicValue::Result(index)) == self.root_subgraph_node;

            if is_root_node {
                if delayed_newline >= DelayedNewline::BeforeExpression {
                    write!(fmt, "\n{indent}")?;
                }

                write!(fmt, "{expr_printer}")?;
                continue;
            }

            match op.as_ref() {
                ExprKind::FunctionArg(_) => {
                    delayed_op_lookup.insert(index, index);
                }
                ExprKind::Function { params, output } => {
                    if delayed_newline >= DelayedNewline::BeforeExpression {
                        write!(fmt, "\n{indent}")?;
                    }

                    let is_extern_func = extern_func_lookup.contains(&index);

                    if is_extern_func {
                        write!(fmt, "pub fn {index_printer}(")?;
                    } else {
                        write!(fmt, "let {index_printer} = ")?;
                        write!(fmt, "fn(")?;
                    }
                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            write!(fmt, ", ")?;
                        }
                        let param =
                            expr_printer.with_value(*param).as_top_level();
                        write!(fmt, "{param}")?;
                    }
                    write!(fmt, ") {{")?;

                    to_print.push(PrintItem {
                        kind: PrintItemKind::FunctionOutput {
                            output: *output,
                            is_extern_func,
                        },
                        indent: indent + self.indent_width,
                    });

                    params
                        .iter()
                        .filter_map(|param| match param {
                            SymbolicValue::Result(param_index) => {
                                Some(param_index)
                            }
                            _ => None,
                        })
                        .for_each(|param_index| {
                            delayed_op_lookup.remove(param_index);
                        });

                    let mut stolen_ops = Vec::new();
                    std::mem::swap(&mut delayed_ops, &mut stolen_ops);
                    for delayed in stolen_ops.into_iter().rev() {
                        if let PrintItemKind::Op(delayed_index) = &delayed.kind
                        {
                            delayed_op_lookup.remove(delayed_index);
                        }
                        to_print.push(delayed);
                    }
                    delayed_newline = DelayedNewline::BeforeAssignment;
                }
                _ => {
                    if delayed_newline >= DelayedNewline::BeforeAssignment {
                        write!(fmt, "\n{indent}")?;
                    }
                    write!(fmt, "let {index_printer} = {expr_printer};")?;
                    delayed_newline = DelayedNewline::BeforeExpression;
                }
            }
        }

        assert_eq!(delayed_ops.len(), 0);

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

    fn as_top_level(self) -> Self {
        Self {
            is_top_level: true,
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

        let index_printer = IndexPrinter {
            graph: self.graph,
            index: op_index,
            requires_name_prefix: self.requires_name_prefix[op_index.0],
        };

        let display_full_expr =
            self.is_top_level || self.inline_expr[op_index.0];
        if !display_full_expr {
            return write!(f, "{index_printer}");
        }

        let sep =
            MaybeZeroWidthSpace(self.insert_zero_width_space_at_breakpoint);

        let write_tuple = |f: &mut std::fmt::Formatter<'_>,
                           tuple: &[SymbolicValue]|
         -> std::fmt::Result {
            write!(f, "(")?;
            for (i, element) in tuple.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                let element = self.with_value(*element).as_top_level();
                write!(f, "{element}")?;
            }

            write!(f, ")")?;
            Ok(())
        };

        match self.graph[op_index].as_ref() {
            ExprKind::Function { params, output } => {
                write!(f, "fn")?;
                write_tuple(f, params)?;
                write!(f, " {{ {} }}", self.with_value(*output))?;

                Ok(())
            }
            ExprKind::FunctionArg(ty) => {
                write!(f, "{index_printer}: {ty}")
            }
            ExprKind::FunctionCall { func, args } => {
                let func = self.with_value(*func);
                write!(f, "{func}")?;
                write_tuple(f, args)
            }
            ExprKind::Range { extent } => {
                let extent = self.with_value(*extent);
                write!(f, "(0..{extent})")
            }
            ExprKind::Reduce {
                initial,
                iterator,
                reduction,
            } => {
                let initial = self.with_value(*initial);
                let iterator = self.with_value(*iterator);
                let reduction = self.with_value(*reduction);
                write!(f, "{iterator}.reduce({initial}, {reduction})")
            }
            ExprKind::NativeFunction(func) => {
                write!(f, "NativeFunction({:p})", Rc::as_ptr(func))
            }
            ExprKind::Tuple(elements) => write_tuple(f, elements),
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

impl std::fmt::Debug for ExprKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct FormatPtr<T>(*const T);
        impl<T> std::fmt::Debug for FormatPtr<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:p}", self.0)
            }
        }

        match self {
            Self::FunctionArg(arg) => {
                f.debug_tuple("FunctionArg").field(arg).finish()
            }
            Self::Function { params, output } => f
                .debug_struct("Function")
                .field("params", params)
                .field("output", output)
                .finish(),
            Self::FunctionCall { func, args } => f
                .debug_struct("FunctionCall")
                .field("func", func)
                .field("args", args)
                .finish(),
            Self::Range { extent } => {
                f.debug_struct("Range").field("extent", extent).finish()
            }
            ExprKind::Reduce {
                initial,
                iterator,
                reduction,
            } => f
                .debug_struct("Reduce")
                .field("initial", initial)
                .field("iterator", iterator)
                .field("reduction", reduction)
                .finish(),
            Self::Tuple(tuple) => f.debug_tuple("Tuple").field(tuple).finish(),
            Self::NativeFunction(func) => f
                .debug_tuple("NativeFunction")
                .field(&FormatPtr(Rc::as_ptr(func) as *const ()))
                .finish(),
            Self::StaticField(field) => {
                f.debug_tuple("StaticField").field(field).finish()
            }
            Self::FieldAccess { obj, field } => f
                .debug_struct("FieldAccess")
                .field("obj", obj)
                .field("field", field)
                .finish(),
            Self::SymbolicDowncast { obj, ty } => f
                .debug_struct("SymbolicDowncast")
                .field("obj", obj)
                .field("ty", ty)
                .finish(),
            Self::IndexAccess { obj, indices } => f
                .debug_struct("IndexAccess")
                .field("obj", obj)
                .field("indices", indices)
                .finish(),
            Self::NumArrayElements { array } => f
                .debug_struct("NumArrayElements")
                .field("array", array)
                .finish(),
            Self::ArrayExtent { array, dim } => f
                .debug_struct("ArrayExtent")
                .field("array", array)
                .field("dim", dim)
                .finish(),
            Self::PointerCast { ptr, ty } => f
                .debug_struct("PointerCast")
                .field("ptr", ptr)
                .field("ty", ty)
                .finish(),
            Self::Add { lhs, rhs } => f
                .debug_struct("Add")
                .field("lhs", lhs)
                .field("rhs", rhs)
                .finish(),
            Self::Mul { lhs, rhs } => f
                .debug_struct("Mul")
                .field("lhs", lhs)
                .field("rhs", rhs)
                .finish(),
            Self::PrimCast { value, prim_type } => f
                .debug_struct("PrimCast")
                .field("value", value)
                .field("prim_type", prim_type)
                .finish(),
            Self::PhysicalDowncast { obj, ty } => f
                .debug_struct("PhysicalDowncast")
                .field("obj", obj)
                .field("ty", ty)
                .finish(),
            Self::ReadValue { ptr, prim_type } => f
                .debug_struct("ReadValue")
                .field("ptr", ptr)
                .field("prim_type", prim_type)
                .finish(),
        }
    }
}
impl PartialEq for ExprKind {
    fn eq(&self, other: &Self) -> bool {
        match self {
            ExprKind::FunctionArg(lhs) => match other {
                ExprKind::FunctionArg(rhs) => lhs == rhs,
                _ => false,
            },
            ExprKind::Function {
                params: lhs_params,
                output: lhs_output,
            } => match other {
                ExprKind::Function {
                    params: rhs_params,
                    output: rhs_output,
                } => lhs_params == rhs_params && lhs_output == rhs_output,
                _ => false,
            },
            ExprKind::FunctionCall {
                func: lhs_func,
                args: lhs_args,
            } => match other {
                ExprKind::FunctionCall {
                    func: rhs_func,
                    args: rhs_args,
                } => lhs_func == rhs_func && lhs_args == rhs_args,
                _ => false,
            },
            ExprKind::Range { extent: lhs_extent } => match other {
                ExprKind::Range { extent: rhs_extent } => {
                    lhs_extent == rhs_extent
                }
                _ => false,
            },
            ExprKind::Reduce {
                initial: lhs_initial,
                iterator: lhs_iterator,
                reduction: lhs_reduction,
            } => match other {
                ExprKind::Reduce {
                    initial: rhs_initial,
                    iterator: rhs_iterator,
                    reduction: rhs_reduction,
                } => {
                    lhs_initial == rhs_initial
                        && lhs_iterator == rhs_iterator
                        && lhs_reduction == rhs_reduction
                }
                _ => false,
            },
            ExprKind::Tuple(lhs_values) => match other {
                ExprKind::Tuple(rhs_values) => lhs_values == rhs_values,
                _ => false,
            },
            ExprKind::NativeFunction(lhs_func) => match other {
                ExprKind::NativeFunction(rhs_func) => {
                    std::ptr::eq(Rc::as_ptr(lhs_func), Rc::as_ptr(rhs_func))
                }
                _ => false,
            },
            ExprKind::StaticField(lhs_field) => match other {
                ExprKind::StaticField(rhs_field) => lhs_field == rhs_field,
                _ => false,
            },
            ExprKind::FieldAccess {
                obj: lhs_obj,
                field: lhs_field,
            } => match other {
                ExprKind::FieldAccess {
                    obj: rhs_obj,
                    field: rhs_field,
                } => lhs_obj == rhs_obj && lhs_field == rhs_field,
                _ => false,
            },
            ExprKind::SymbolicDowncast {
                obj: lhs_obj,
                ty: lhs_ty,
            } => match other {
                ExprKind::SymbolicDowncast {
                    obj: rhs_obj,
                    ty: rhs_ty,
                } => lhs_obj == rhs_obj && lhs_ty == rhs_ty,
                _ => false,
            },
            ExprKind::IndexAccess {
                obj: lhs_obj,
                indices: lhs_indices,
            } => match other {
                ExprKind::IndexAccess {
                    obj: rhs_obj,
                    indices: rhs_indices,
                } => lhs_obj == rhs_obj && lhs_indices == rhs_indices,
                _ => false,
            },
            ExprKind::NumArrayElements { array: lhs_array } => match other {
                ExprKind::NumArrayElements { array: rhs_array } => {
                    lhs_array == rhs_array
                }
                _ => false,
            },
            ExprKind::ArrayExtent {
                array: lhs_array,
                dim: lhs_dim,
            } => match other {
                ExprKind::ArrayExtent {
                    array: rhs_array,
                    dim: rhs_dim,
                } => lhs_array == rhs_array && lhs_dim == rhs_dim,
                _ => false,
            },
            ExprKind::PointerCast {
                ptr: lhs_ptr,
                ty: lhs_ty,
            } => match other {
                ExprKind::PointerCast {
                    ptr: rhs_ptr,
                    ty: rhs_ty,
                } => lhs_ptr == rhs_ptr && lhs_ty == rhs_ty,
                _ => false,
            },
            ExprKind::Add {
                lhs: lhs_lhs,
                rhs: lhs_rhs,
            } => match other {
                ExprKind::Add {
                    lhs: rhs_lhs,
                    rhs: rhs_rhs,
                } => lhs_lhs == rhs_lhs && lhs_rhs == rhs_rhs,
                _ => false,
            },
            ExprKind::Mul {
                lhs: lhs_lhs,
                rhs: lhs_rhs,
            } => match other {
                ExprKind::Mul {
                    lhs: rhs_lhs,
                    rhs: rhs_rhs,
                } => lhs_lhs == rhs_lhs && lhs_rhs == rhs_rhs,
                _ => false,
            },
            ExprKind::PrimCast {
                value: lhs_value,
                prim_type: lhs_ty,
            } => match other {
                ExprKind::PrimCast {
                    value: rhs_value,
                    prim_type: rhs_ty,
                } => lhs_value == rhs_value && lhs_ty == rhs_ty,
                _ => false,
            },
            ExprKind::PhysicalDowncast {
                obj: lhs_obj,
                ty: lhs_ty,
            } => match other {
                ExprKind::PhysicalDowncast {
                    obj: rhs_obj,
                    ty: rhs_ty,
                } => lhs_obj == rhs_obj && lhs_ty == rhs_ty,
                _ => false,
            },
            ExprKind::ReadValue {
                ptr: lhs_ptr,
                prim_type: lhs_ty,
            } => match other {
                ExprKind::ReadValue {
                    ptr: rhs_ptr,
                    prim_type: rhs_ty,
                } => lhs_ptr == rhs_ptr && lhs_ty == rhs_ty,
                _ => false,
            },
        }
    }
}
impl Eq for ExprKind {}
impl std::hash::Hash for ExprKind {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        core::mem::discriminant(self).hash(state);
        match self {
            ExprKind::FunctionArg(ty) => ty.hash(state),
            ExprKind::Function { params, output } => {
                params.hash(state);
                output.hash(state);
            }
            ExprKind::FunctionCall { func, args } => {
                func.hash(state);
                args.hash(state);
            }
            ExprKind::Range { extent } => extent.hash(state),
            ExprKind::Reduce {
                initial,
                iterator,
                reduction,
            } => {
                initial.hash(state);
                iterator.hash(state);
                reduction.hash(state);
            }
            ExprKind::Tuple(values) => values.hash(state),
            ExprKind::NativeFunction(func) => Rc::as_ptr(func).hash(state),
            ExprKind::StaticField(static_field) => static_field.hash(state),
            ExprKind::FieldAccess { obj, field } => {
                obj.hash(state);
                field.hash(state);
            }
            ExprKind::SymbolicDowncast { obj, ty } => {
                obj.hash(state);
                ty.hash(state);
            }
            ExprKind::IndexAccess { obj, indices } => {
                obj.hash(state);
                indices.hash(state);
            }
            ExprKind::NumArrayElements { array } => array.hash(state),
            ExprKind::ArrayExtent { array, dim } => {
                array.hash(state);
                dim.hash(state);
            }
            ExprKind::PointerCast { ptr, ty } => {
                ptr.hash(state);
                ty.hash(state);
            }
            ExprKind::Add { lhs, rhs } => {
                lhs.hash(state);
                rhs.hash(state);
            }
            ExprKind::Mul { lhs, rhs } => {
                lhs.hash(state);
                rhs.hash(state);
            }
            ExprKind::PrimCast { value, prim_type } => {
                value.hash(state);
                prim_type.hash(state);
            }
            ExprKind::PhysicalDowncast { obj, ty } => {
                obj.hash(state);
                ty.hash(state);
            }
            ExprKind::ReadValue { ptr, prim_type } => {
                ptr.hash(state);
                prim_type.hash(state);
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
