use dotnet_debugger::MethodTable;

use crate::{
    ByteRegion, DSLType, Error, ExposedNativeFunction, Expr, ExprKind,
    NativeFunction, OpIndex, RuntimePrimType, StaticField, SymbolicType,
    SymbolicValue, TypedPointer, WrappedNativeFunction,
};

#[derive(Default, Clone)]
pub struct SymbolicGraph {
    ops: Vec<Expr>,
    extern_funcs: Vec<OpIndex>,
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

    pub fn iter_ops(
        &self,
    ) -> impl DoubleEndedIterator<Item = (OpIndex, &Expr)> + '_ {
        self.ops
            .iter()
            .enumerate()
            .map(|(i, op)| (OpIndex::new(i), op))
    }

    pub fn num_extern_funcs(&self) -> usize {
        self.extern_funcs.len()
    }

    pub fn iter_extern_funcs(
        &self,
    ) -> impl DoubleEndedIterator<Item = OpIndex> + '_ {
        self.extern_funcs.iter().cloned()
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
                .nth(1)
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
            SymbolicValue::Const(_) => {
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

    //////////////////////////////////////////////////
    ////          Symbolic Operations              ///
    //////////////////////////////////////////////////

    pub fn none(&mut self) -> SymbolicValue {
        self.push(ExprKind::None)
    }

    pub fn function_arg(&mut self, ty: impl Into<DSLType>) -> SymbolicValue {
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

    pub fn chain(
        &mut self,
        iter_a: SymbolicValue,
        iter_b: SymbolicValue,
    ) -> SymbolicValue {
        self.push(ExprKind::Chain(iter_a, iter_b))
    }

    pub fn first(&mut self, iterator: SymbolicValue) -> SymbolicValue {
        self.push(ExprKind::First { iterator })
    }

    pub fn find(
        &mut self,
        iterator: SymbolicValue,
        condition: SymbolicValue,
    ) -> SymbolicValue {
        self.push(ExprKind::Find {
            iterator,
            condition,
        })
    }

    pub fn find_map(
        &mut self,
        iterator: SymbolicValue,
        condition: SymbolicValue,
    ) -> SymbolicValue {
        self.push(ExprKind::FindMap {
            iterator,
            condition,
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
        ty: DSLType,
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
    binary_op! {sub, Sub}
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

    pub fn object_method_table(
        &mut self,
        obj: impl Into<SymbolicValue>,
    ) -> SymbolicValue {
        let obj = obj.into();
        self.push(ExprKind::ObjectMethodTable { obj })
    }

    pub fn is_subclass_of(
        &mut self,
        method_table_ptr: impl Into<SymbolicValue>,
        base_type: TypedPointer<MethodTable>,
    ) -> SymbolicValue {
        let method_table_ptr = method_table_ptr.into();
        self.push(ExprKind::IsSubclassOf {
            method_table_ptr,
            ty: base_type,
        })
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
        let region = ByteRegion { ptr, num_bytes };
        self.push(ExprKind::ReadBytes(vec![region]))
    }

    pub fn read_byte_regions<Regions>(
        &mut self,
        regions: Regions,
    ) -> SymbolicValue
    where
        Regions: IntoIterator,
        <Regions as IntoIterator>::Item: Into<ByteRegion>,
    {
        self.push(ExprKind::ReadBytes(
            regions.into_iter().map(Into::into).collect(),
        ))
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
}

impl std::ops::Index<OpIndex> for SymbolicGraph {
    type Output = Expr;

    fn index(&self, index: OpIndex) -> &Self::Output {
        &self.ops[index.0]
    }
}
