use std::collections::HashMap;

use dotnet_debugger::{MethodTable, RuntimePrimType, TypedPointer};

use crate::{
    DSLType, ExposedNativeFunction, OpIndex, SymbolicType, SymbolicValue,
};

#[derive(Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub name: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ExprKind {
    /// An empty expression
    None,

    /// A variable argument to a function.
    FunctionArg(DSLType),

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

    /// Chain two iterators together.
    Chain(SymbolicValue, SymbolicValue),

    /// Filter the elements of an iterator, retaining elements of the
    /// iterator for which the filter function returns true.
    Filter {
        /// The iterator whose elements should be mapped.
        iterator: SymbolicValue,

        /// The filter function to apply.  Should have signature
        /// `Fn(Item) -> bool`
        filter: SymbolicValue,
    },

    First {
        /// The iterator whose value should be retrieved.
        iterator: SymbolicValue,
    },

    Find {
        /// The iterator that should be searched.
        iterator: SymbolicValue,

        /// The condition with which to return a value.  Should have
        /// signature `Fn(Item) -> bool`.
        condition: SymbolicValue,
    },

    FindMap {
        /// The iterator that should be searched.
        iterator: SymbolicValue,

        /// The condition with which to return a value.  Should have
        /// signature `Fn(Item) -> Option<OutputType>`.
        condition: SymbolicValue,
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
    /// of the subclass may be accessed.  If the field is not an
    /// instance of the subclass, `SymbolicDowncast` will produce
    /// `None`, which will be propagated to all field accesses.
    ///
    /// This is lowered into `IsSubclassOf`, followed by a conditional
    /// statement.
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
    PointerCast { ptr: SymbolicValue, ty: DSLType },

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

    /// Perform subtraction of the LHS and RHS.
    ///
    /// This operation is used for both pointer arithmetic and numeric
    /// arithmetic.
    Sub {
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

    /// Check if a .NET type is a subclass of another .NET type
    IsSubclassOf {
        /// The pointer to a method table, as read out from the
        /// Object's header.
        method_table_ptr: SymbolicValue,

        /// A pointer to the type being checked against.
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
    ///
    /// This accepts a vector of regions to read, to enable
    /// optimizations that reduce the number of reads.  In the VM,
    /// this operator is implemented in terms of `process_vm_readv`,
    /// which reads from multiple distinct locations.
    ReadBytes(Vec<ByteRegion>),

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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StaticField {
    pub class: SymbolicType,
    pub field_name: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ByteRegion {
    pub ptr: SymbolicValue,
    pub num_bytes: SymbolicValue,
}

impl ExprKind {
    pub fn op_name(&self) -> &'static str {
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
            ExprKind::First { .. } => "First",
            ExprKind::Find { .. } => "Find",
            ExprKind::FindMap { .. } => "FindMap",
            ExprKind::Chain { .. } => "Chain",
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
            ExprKind::Sub { .. } => "Sub",
            ExprKind::Mul { .. } => "Mul",
            ExprKind::Div { .. } => "Div",
            ExprKind::Mod { .. } => "Mod",
            ExprKind::PrimCast { .. } => "PrimCast",
            ExprKind::IsSubclassOf { .. } => "IsSubclassOf",
            ExprKind::ReadPrim { .. } => "ReadValue",
            ExprKind::ReadBytes { .. } => "ReadBytes",
            ExprKind::CastBytes { .. } => "CastBytes",
            ExprKind::ReadString { .. } => "ReadString",
        }
    }

    pub fn try_remap(
        &self,
        map: &HashMap<OpIndex, SymbolicValue>,
    ) -> Option<Self> {
        let remap = |value: &SymbolicValue| -> Option<SymbolicValue> {
            match value {
                SymbolicValue::Result(index) => map.get(index).cloned(),
                _ => None,
            }
        };
        let remap_vec =
            |values: &[SymbolicValue]| -> Option<Vec<SymbolicValue>> {
                let mut output: Option<Vec<SymbolicValue>> = None;
                for (i, old_value) in values.iter().enumerate() {
                    if let Some(new_value) = remap(old_value) {
                        output
                            .get_or_insert_with(|| {
                                values[..i].iter().cloned().collect()
                            })
                            .push(new_value);
                    } else if let Some(output) = output.as_mut() {
                        output.push(*old_value);
                    }
                }
                output
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
                let opt_params = remap_vec(params);
                let opt_output = remap(output);
                let requires_remap =
                    opt_params.is_some() || opt_output.is_some();
                requires_remap.then(|| {
                    let params = opt_params.unwrap_or_else(|| params.clone());
                    let output = opt_output.unwrap_or(*output);
                    ExprKind::Function { params, output }
                })
            }
            ExprKind::FunctionCall { func, args } => {
                let opt_func = remap(func);
                let opt_args = remap_vec(args);
                let requires_remap = opt_func.is_some() || opt_args.is_some();
                requires_remap.then(|| {
                    let func = opt_func.unwrap_or(*func);
                    let args = opt_args.unwrap_or_else(|| args.clone());
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
                    let iterator = opt_iterator.unwrap_or(*iterator);
                    let map = opt_map.unwrap_or(*map);
                    ExprKind::Map { iterator, map }
                })
            }
            ExprKind::Filter { iterator, filter } => {
                let opt_iterator = remap(iterator);
                let opt_filter = remap(filter);
                (opt_iterator.is_some() || opt_filter.is_some()).then(|| {
                    let iterator = opt_iterator.unwrap_or(*iterator);
                    let filter = opt_filter.unwrap_or(*filter);
                    ExprKind::Filter { iterator, filter }
                })
            }
            ExprKind::Find {
                iterator,
                condition,
            } => {
                let opt_iterator = remap(iterator);
                let opt_condition = remap(condition);
                (opt_iterator.is_some() || opt_condition.is_some()).then(|| {
                    let iterator = opt_iterator.unwrap_or(*iterator);
                    let condition = opt_condition.unwrap_or(*condition);
                    ExprKind::Find {
                        iterator,
                        condition,
                    }
                })
            }
            ExprKind::FindMap {
                iterator,
                condition,
            } => {
                let opt_iterator = remap(iterator);
                let opt_condition = remap(condition);
                (opt_iterator.is_some() || opt_condition.is_some()).then(|| {
                    let iterator = opt_iterator.unwrap_or(*iterator);
                    let condition = opt_condition.unwrap_or(*condition);
                    ExprKind::FindMap {
                        iterator,
                        condition,
                    }
                })
            }
            ExprKind::Chain(iter_a, iter_b) => {
                let opt_iter_a = remap(iter_a);
                let opt_iter_b = remap(iter_b);
                (opt_iter_a.is_some() || opt_iter_b.is_some()).then(|| {
                    let iter_a = opt_iter_a.unwrap_or(*iter_a);
                    let iter_b = opt_iter_b.unwrap_or(*iter_b);
                    ExprKind::Chain(iter_a, iter_b)
                })
            }

            ExprKind::First { iterator } => {
                remap(iterator).map(|iterator| ExprKind::First { iterator })
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
                    let initial = opt_initial.unwrap_or(*initial);
                    let iterator = opt_iterator.unwrap_or(*iterator);
                    let reduction = opt_reduction.unwrap_or(*reduction);
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
                    let initial = opt_initial.unwrap_or(*initial);
                    let extent = opt_extent.unwrap_or(*extent);
                    let reduction = opt_reduction.unwrap_or(*reduction);
                    ExprKind::SimpleReduce {
                        initial,
                        extent,
                        reduction,
                    }
                })
            }
            ExprKind::Tuple(elements) => {
                remap_vec(elements).map(|elements| ExprKind::Tuple(elements))
            }
            ExprKind::FieldAccess { obj, field } => {
                remap(obj).map(|obj| ExprKind::FieldAccess {
                    obj,
                    field: field.clone(),
                })
            }
            ExprKind::IndexAccess { obj, indices } => {
                let opt_obj = remap(obj);
                let opt_indices = remap_vec(indices);
                let requires_remap = opt_obj.is_some() || opt_indices.is_some();
                requires_remap.then(|| {
                    let obj = opt_obj.unwrap_or(*obj);
                    let indices =
                        opt_indices.unwrap_or_else(|| indices.clone());
                    ExprKind::IndexAccess { obj, indices }
                })
            }
            ExprKind::PrimCast { value, prim_type } => {
                remap(value).map(|value| ExprKind::PrimCast {
                    value,
                    prim_type: *prim_type,
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
                    let array = opt_array.unwrap_or(*array);
                    let dim = opt_array.unwrap_or(*dim);
                    ExprKind::ArrayExtent { array, dim }
                })
            }
            ExprKind::PointerCast { ptr, ty } => {
                remap(ptr).map(|ptr| ExprKind::PointerCast {
                    ptr,
                    ty: ty.clone(),
                })
            }
            ExprKind::IsSome(value) => remap(value).map(ExprKind::IsSome),

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
                    let condition = opt_condition.unwrap_or(*condition);
                    let if_branch = opt_if_branch.unwrap_or(*if_branch);
                    let else_branch = opt_else_branch.unwrap_or(*else_branch);
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
            ExprKind::Sub { lhs, rhs } => handle_binary_op!(Sub, lhs, rhs),
            ExprKind::Mul { lhs, rhs } => handle_binary_op!(Mul, lhs, rhs),
            ExprKind::Div { lhs, rhs } => handle_binary_op!(Div, lhs, rhs),
            ExprKind::Mod { lhs, rhs } => handle_binary_op!(Mod, lhs, rhs),

            ExprKind::IsSubclassOf {
                method_table_ptr,
                ty,
            } => remap(method_table_ptr).map(|method_table_ptr| {
                ExprKind::IsSubclassOf {
                    method_table_ptr,
                    ty: *ty,
                }
            }),

            ExprKind::ReadPrim { ptr, prim_type } => {
                remap(ptr).map(|ptr| ExprKind::ReadPrim {
                    ptr,
                    prim_type: *prim_type,
                })
            }
            ExprKind::ReadBytes(regions) => {
                let requires_remap = regions.iter().any(|region| {
                    remap(&region.ptr).is_some()
                        || remap(&region.num_bytes).is_some()
                });

                requires_remap.then(|| {
                    let regions = regions
                        .iter()
                        .map(|region| {
                            let ptr = remap(&region.ptr).unwrap_or(region.ptr);
                            let num_bytes = remap(&region.num_bytes)
                                .unwrap_or(region.num_bytes);
                            ByteRegion { ptr, num_bytes }
                        })
                        .collect();
                    ExprKind::ReadBytes(regions)
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
                    let bytes = opt_bytes.unwrap_or(*bytes);
                    let offset = opt_offset.unwrap_or(*offset);
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
}

impl<T> From<T> for Expr
where
    T: Into<ExprKind>,
{
    fn from(value: T) -> Self {
        let kind: ExprKind = value.into();
        Expr { kind, name: None }
    }
}

impl From<StaticField> for ExprKind {
    fn from(static_field: StaticField) -> Self {
        ExprKind::StaticField(static_field)
    }
}

impl std::fmt::Display for ExprKind {
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
            ExprKind::Find {
                iterator,
                condition,
            } => {
                write!(f, "{iterator}.find({condition})")
            }
            ExprKind::FindMap {
                iterator,
                condition,
            } => {
                write!(f, "{iterator}.find_map({condition})")
            }
            ExprKind::First { iterator } => {
                write!(f, "{iterator}.first()")
            }
            ExprKind::Chain(iter_a, iter_b) => {
                write!(f, "{iter_a}.chain({iter_b})")
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
            ExprKind::Sub { lhs, rhs } => write!(f, "{lhs} - {rhs}"),
            ExprKind::Mul { lhs, rhs } => write!(f, "{lhs}*{rhs}"),
            ExprKind::Div { lhs, rhs } => write!(f, "{lhs}/{rhs}"),
            ExprKind::Mod { lhs, rhs } => write!(f, "{lhs}%{rhs}"),
            ExprKind::IsSubclassOf {
                method_table_ptr,
                ty,
            } => {
                write!(f, "{method_table_ptr}.is_subclass_of({ty})")
            }
            ExprKind::PrimCast { value, prim_type } => {
                write!(f, "{value}.prim_cast::<{prim_type}>()")
            }
            ExprKind::ReadPrim { ptr, prim_type } => {
                write!(f, "{ptr}.read_value::<{prim_type}>()")
            }
            ExprKind::ReadBytes(regions) if regions.len() == 1 => {
                let ptr = &regions[0].ptr;
                let num_bytes = &regions[0].num_bytes;
                write!(f, "{ptr}.read_bytes({num_bytes})")
            }
            ExprKind::ReadBytes(regions) => {
                write!(f, "read_bytes(")?;
                for region in regions.iter() {
                    let ptr = &region.ptr;
                    let num_bytes = &region.num_bytes;
                    write!(f, "{ptr}, {num_bytes}, ")?;
                }
                write!(f, ")")?;
                Ok(())
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

impl std::ops::Deref for Expr {
    type Target = ExprKind;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

impl<T> From<(SymbolicValue, T)> for ByteRegion
where
    T: Into<SymbolicValue>,
{
    fn from(region: (SymbolicValue, T)) -> Self {
        let (ptr, num_bytes) = region;
        let num_bytes = num_bytes.into();
        Self { ptr, num_bytes }
    }
}
