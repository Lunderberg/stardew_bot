use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use derive_more::derive::From;
use itertools::Itertools as _;

use iterator_extensions::ResultIteratorExt as _;
use memory_reader::Pointer;

use crate::{
    bytecode::virtual_machine::{
        FunctionIndex, Instruction, InstructionIndex, StackIndex, VMArg,
    },
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

    /// Read a .NET string
    ///
    /// Given a location of a .NET string, produces a Rust-native
    /// String.
    ReadString { ptr: SymbolicValue },
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

pub struct GraphComparison<'a> {
    lhs: &'a SymbolicGraph,
    rhs: &'a SymbolicGraph,
    order_dependent: bool,
    compare_names: bool,
}

/// Helper struct for collecting VM instructions
struct ExpressionTranslator<'a> {
    /// The graph being translated
    graph: &'a SymbolicGraph,

    /// The instructions being collected.
    instructions: &'a mut Vec<Instruction>,

    /// Indicates which instructions belong to which scope.  Has the
    /// same length as `graph.ops`.  If an element is `None`, the
    /// element belongs in the global scope.  If the element is
    /// `Some(index)`, then `index` points to the
    /// `ExprKind::FunctionDef` that owns the expression.
    scope: &'a [Option<OpIndex>],

    num_outputs: usize,
    main_func_index: OpIndex,
    next_free_index: &'a mut usize,
    native_functions: &'a mut Vec<ExposedNativeFunction>,
    native_function_lookup: &'a mut HashMap<OpIndex, FunctionIndex>,

    output_lookup: &'a HashMap<OpIndex, StackIndex>,

    currently_stored: HashMap<OpIndex, VMArg>,

    previously_consumed: HashSet<OpIndex>,
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

    pub fn is_some(
        &mut self,
        value: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let value = value.into();
        self.push(ExprKind::IsSome(value))
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

    /// For each expression, determine which function owns it.  An
    /// expression is considered owned by a function if it uses at
    /// least one of the function parameters, and contributes to the
    /// output of the function.
    pub(crate) fn operation_scope(&self) -> Vec<Option<OpIndex>> {
        let mut scope = vec![None; self.ops.len()];
        self.iter_ops()
            .rev()
            .filter(|(_, op)| matches!(op.kind, ExprKind::Function { .. }))
            .for_each(|(func_index, op)| {
                let ExprKind::Function { params, output } = &op.kind else {
                    unreachable!("Due to earlier filter")
                };

                self.collect_subgraph(params.iter().cloned(), Some(*output))
                    .into_iter()
                    .for_each(|index| {
                        scope[index.0] = Some(func_index);
                    });
            });

        scope
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

    pub fn to_virtual_machine(&self) -> Result<VirtualMachine, Error> {
        let mut instructions = Vec::new();

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

        let scope = self.operation_scope();

        let iter_op_indices = scope
            .iter()
            .enumerate()
            .filter(|(_, scope)| scope.is_none())
            .map(|(i, _)| OpIndex::new(i))
            .filter(|index| {
                !matches!(self[*index].kind, ExprKind::Function { .. })
            });

        let mut translater = ExpressionTranslator {
            graph: self,
            instructions: &mut instructions,
            scope: &scope,
            num_outputs,
            main_func_index,
            next_free_index: &mut next_free_index,
            native_functions: &mut native_functions,
            native_function_lookup: &mut native_function_lookup,
            output_lookup: &output_lookup,
            currently_stored: HashMap::new(),
            previously_consumed: HashSet::new(),
        };
        translater.translate(iter_op_indices)?;

        let mut builder =
            VirtualMachine::builder(instructions).num_outputs(outputs.len());
        for native_func in native_functions.into_iter() {
            builder = builder.with_raw_native_function(native_func);
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

        if show_steps {
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

        if show_steps {
            println!(
                "----------- After IdentifyStaticField --------------\n{expr}"
            );
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
            .then(super::MergeRangeReduceToSimpleReduce)
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

impl ExpressionTranslator<'_> {
    fn value_to_arg(&self, value: &SymbolicValue) -> Result<VMArg, Error> {
        match value {
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

    fn translate(
        &mut self,
        instructions: impl Iterator<Item = OpIndex>,
    ) -> Result<(), Error> {
        macro_rules! next_free_index {
            () => {{
                let index = *self.next_free_index;
                *self.next_free_index += 1;
                StackIndex(index)
            }};
        }

        for op_index in instructions {
            let op = &self.graph[op_index];

            let op_output: StackIndex = self
                .output_lookup
                .get(&op_index)
                .cloned()
                .unwrap_or_else(|| next_free_index!());

            match op.as_ref() {
                ExprKind::Function { .. } => {
                    unreachable!(
                        "Function calls should be inlined, \
                         and should only be encountered in SimpleReduce."
                    )
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

                            self.instructions.push(
                                Instruction::NativeFunctionCall {
                                    index: *native_func_index,
                                    args: vm_args,
                                    output: None,
                                },
                            );

                            self.currently_stored.remove(&first_arg_op);
                            self.previously_consumed.insert(first_arg_op);

                            if op_output.0 < self.num_outputs {
                                self.instructions.push(Instruction::Swap(
                                    first_arg_loc,
                                    op_output,
                                ));
                                self.currently_stored
                                    .insert(op_index, op_output.into());
                            } else {
                                self.currently_stored
                                    .insert(op_index, first_arg_loc.into());
                            }
                        } else {
                            // The function produces an output value, to
                            // be stored in the output index.
                            self.instructions.push(
                                Instruction::NativeFunctionCall {
                                    index: *native_func_index,
                                    args: vm_args,
                                    output: Some(op_output),
                                },
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

                    match initial {
                        VMArg::Const(_) => {
                            self.instructions.push(Instruction::Copy {
                                value: initial,
                                output: op_output,
                            })
                        }
                        VMArg::SavedValue(stack_index) => self
                            .instructions
                            .push(Instruction::Swap(stack_index, op_output)),
                    }

                    let loop_iter = next_free_index!();
                    self.instructions.push(Instruction::Copy {
                        value: 0usize.into(),
                        output: loop_iter,
                    });

                    let loop_start = InstructionIndex(self.instructions.len());

                    let stop_loop_condition = next_free_index!();
                    self.instructions.push(Instruction::GreaterThanOrEqual {
                        lhs: loop_iter.into(),
                        rhs: extent,
                        output: stop_loop_condition,
                    });

                    // Placeholder for the conditional jump to break
                    // out of the loop.  To be updated after the loop
                    // body is generated, when we know the destination
                    // index of the jump.
                    let jump_to_end_instruction_index = self.instructions.len();
                    self.instructions.push(Instruction::NoOp);

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

                            let currently_stored = self
                                .currently_stored
                                .iter()
                                .map(|(&stack_index, &vmarg)| {
                                    (stack_index, vmarg)
                                })
                                .chain([
                                    (accumulator, op_output.into()),
                                    (index, loop_iter.into()),
                                ])
                                .collect();

                            let iter_body = self
                                .scope
                                .iter()
                                .enumerate()
                                .filter(|(_, scope)| {
                                    **scope == Some(reduction_index)
                                })
                                .map(|(i, _)| OpIndex::new(i))
                                .filter(|&op| {
                                    op != accumulator
                                        && op != index
                                        && !matches!(
                                            self.graph[op].kind,
                                            ExprKind::Function { .. }
                                        )
                                });
                            let iter_body: Box<dyn Iterator<Item = OpIndex>> =
                                Box::new(iter_body);

                            let mut body_translater = ExpressionTranslator {
                                currently_stored,
                                previously_consumed: self
                                    .previously_consumed
                                    .clone(),
                                graph: self.graph,
                                instructions: self.instructions,
                                scope: self.scope,
                                num_outputs: self.num_outputs,
                                main_func_index: self.main_func_index,
                                next_free_index: self.next_free_index,
                                native_functions: self.native_functions,
                                native_function_lookup: self
                                    .native_function_lookup,
                                output_lookup: self.output_lookup,
                            };

                            body_translater.translate(iter_body)?;

                            match body_translater.value_to_arg(output)? {
                                VMArg::Const(value) => {
                                    self.instructions.push(Instruction::Copy {
                                        value: value.into(),
                                        output: op_output,
                                    });
                                }
                                VMArg::SavedValue(body_output) => {
                                    self.instructions.push(Instruction::Swap(
                                        body_output,
                                        op_output,
                                    ));
                                }
                            }
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

                            self.instructions.push(
                                Instruction::NativeFunctionCall {
                                    index: *native_func_index,
                                    args: vec![
                                        op_output.into(),
                                        loop_iter.into(),
                                    ],
                                    output: func_output,
                                },
                            );
                        }
                        _ => todo!(
                            "Better error message \
                             when SimpleReduce points to non-function"
                        ),
                    }

                    self.instructions.push(Instruction::Add {
                        lhs: loop_iter.into(),
                        rhs: 1usize.into(),
                        output: loop_iter,
                    });

                    self.instructions.push(Instruction::ConditionalJump {
                        cond: RuntimePrimValue::Bool(true).into(),
                        dest: loop_start,
                    });

                    let after_loop = InstructionIndex(self.instructions.len());
                    self.instructions[jump_to_end_instruction_index] =
                        Instruction::ConditionalJump {
                            cond: stop_loop_condition.into(),
                            dest: after_loop,
                        };

                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::NativeFunction(func) => {
                    let func_index = FunctionIndex(self.native_functions.len());
                    self.native_functions.push(func.clone());
                    self.native_function_lookup.insert(op_index, func_index);
                }

                ExprKind::IsSome(value) => {
                    let value = self.value_to_arg(value)?;
                    self.instructions.push(Instruction::IsSome {
                        value,
                        output: op_output,
                    });
                    self.currently_stored.insert(op_index, op_output.into());
                }

                ExprKind::Add { lhs, rhs } => {
                    let lhs = self.value_to_arg(lhs)?;
                    let rhs = self.value_to_arg(rhs)?;
                    self.instructions.push(Instruction::Add {
                        lhs,
                        rhs,
                        output: op_output,
                    });
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::Mul { lhs, rhs } => {
                    let lhs = self.value_to_arg(lhs)?;
                    let rhs = self.value_to_arg(rhs)?;
                    self.instructions.push(Instruction::Mul {
                        lhs,
                        rhs,
                        output: op_output,
                    });
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::PrimCast { value, prim_type } => {
                    let value = self.value_to_arg(value)?;
                    self.instructions.push(Instruction::PrimCast {
                        value,
                        prim_type: *prim_type,
                        output: op_output,
                    });
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::PhysicalDowncast { obj, ty } => {
                    let obj = self.value_to_arg(obj)?;
                    self.instructions.push(Instruction::Downcast {
                        obj,
                        subtype: *ty,
                        output: op_output,
                    });
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::ReadValue { ptr, prim_type } => {
                    let ptr = self.value_to_arg(ptr)?;
                    self.instructions.push(Instruction::Read {
                        ptr,
                        prim_type: *prim_type,
                        output: op_output,
                    });
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::ReadString { ptr } => {
                    let ptr = self.value_to_arg(ptr)?;
                    self.instructions.push(Instruction::ReadString {
                        ptr,
                        output: op_output,
                    });
                    self.currently_stored.insert(op_index, op_output.into());
                }
                ExprKind::PointerCast { ptr, .. } => {
                    let ptr = self.value_to_arg(ptr)?;
                    self.instructions.push(Instruction::Copy {
                        value: ptr,
                        output: op_output,
                    });
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
            ExprKind::Add { lhs, rhs } | ExprKind::Mul { lhs, rhs } => {
                callback(*lhs);
                callback(*rhs)
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
