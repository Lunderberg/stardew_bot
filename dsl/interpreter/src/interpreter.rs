use std::collections::HashMap;

use dsl_ir::{
    DSLType, ExposedNativeFunction, ExprKind, FunctionType, OpIndex,
    RuntimePrimType, RuntimePrimValue, StackValue, SymbolicGraph,
    SymbolicValue,
};
use dsl_runtime::{Reader, Runtime, RuntimeFunc, RuntimeOutput, ValueIndex};
use smallvec::SmallVec;

use crate::Error;

pub struct Interpreter {
    graph: SymbolicGraph,
}

#[allow(dead_code)]
pub struct InterpretedFunc<'a> {
    graph: &'a SymbolicGraph,
    reader: Box<dyn Reader + 'a>,
    current_op: OpIndex,
    values: RuntimeOutput,
    bindings: HashMap<OpIndex, Option<StackValue>>,
}

impl Interpreter {
    pub fn new(graph: SymbolicGraph) -> Self {
        Self { graph }
    }
}

impl Runtime for Interpreter {
    type Error = Error;

    type Func<'a>
        = InterpretedFunc<'a>
    where
        Self: 'a;

    fn get_function<'a>(
        &'a self,
        name: &str,
    ) -> Result<Self::Func<'a>, Self::Error> {
        let func = self
            .graph
            .iter_extern_funcs()
            .find(|op| {
                self.graph[*op]
                    .name
                    .as_ref()
                    .map(|op_name| op_name == name)
                    .unwrap_or(false)
            })
            .ok_or_else(|| Error::NoSuchFunction(name.into()))?;

        Ok(InterpretedFunc {
            graph: &self.graph,
            reader: Box::new(dsl_runtime::DummyReader),
            current_op: func,
            values: RuntimeOutput::new(0),
            bindings: HashMap::new(),
        })
    }
}

macro_rules! define_binary_op {
    ($func_name:ident, $method_name:ident) => {
        fn $func_name(
            &mut self,
            lhs: SymbolicValue,
            rhs: SymbolicValue,
        ) -> Result<(), Error> {
            self.eval(lhs)?;
            self.eval(rhs)?;

            let opt_rhs = self.pop_prim()?;
            let opt_lhs = self.pop_prim()?;

            let output = match (opt_lhs, opt_rhs) {
                (Some(lhs), Some(rhs)) => Some(lhs.$method_name(rhs)?),
                _ => None,
            };
            self.push(output);

            Ok(())
        }
    };
}

impl<'a> InterpretedFunc<'a> {
    pub fn with_reader(self, reader: impl Reader + 'a) -> Self {
        Self {
            reader: Box::new(reader.with_cache()),
            ..self
        }
    }

    pub fn evaluate(mut self) -> Result<RuntimeOutput, Error> {
        let op = &self.graph[self.current_op];
        let ExprKind::Function { params, output } = &op.kind else {
            return Err(Error::CalledOpIsNotFunction);
        };
        if !params.is_empty() {
            return Err(Error::CalledFunctionHasInputParameters);
        }

        self.eval(*output)?;
        Ok(self.values)
    }

    fn push(&mut self, value: Option<impl Into<StackValue>>) {
        self.values.push(value.map(Into::into))
    }

    fn pop_prim(&mut self) -> Result<Option<RuntimePrimValue>, Error> {
        self.values
            .pop()?
            .map(|value| {
                value.as_prim().ok_or_else(|| {
                    Error::OperatorExpectsPrimitiveArgument {
                        operator: self.graph[self.current_op].op_name(),
                        index: self.current_op,
                        arg_type: value.runtime_type(),
                    }
                })
            })
            .transpose()
    }

    fn bind(
        &mut self,
        var: OpIndex,
        value: Option<impl Into<StackValue>>,
    ) -> Result<impl Fn(&mut Self) -> Result<(), Error> + 'static, Error> {
        if self.bindings.contains_key(&var) {
            return Err(Error::CannotRebindVariable { var });
        }
        let value = value.map(Into::into);

        self.bindings.insert(var, value);

        let cleanup =
            move |func: &mut InterpretedFunc<'a>| -> Result<(), Error> {
                func.bindings.remove(&var);
                Ok(())
            };
        Ok(cleanup)
    }

    fn as_function_arg(&self, value: SymbolicValue) -> Result<OpIndex, Error> {
        let index = match value {
            SymbolicValue::Result(index) => Ok(index),
            SymbolicValue::Const(_) => Err(Error::InvalidFunctionDefinition {
                used_as_param: "prim",
            }),
        }?;

        match &self.graph[index].kind {
            ExprKind::FunctionArg(_) => Ok(()),
            other => Err(Error::InvalidFunctionDefinition {
                used_as_param: other.op_name(),
            }),
        }?;

        Ok(index)
    }

    fn eval(&mut self, value: SymbolicValue) -> Result<(), Error> {
        let index = match value {
            SymbolicValue::Result(index) => index,
            SymbolicValue::Const(prim) => {
                self.values.push(Some(prim.into()));
                return Ok(());
            }
        };

        let parent_op = self.current_op;
        self.current_op = index;

        match &self.graph[index].kind {
            ExprKind::None => self.eval_none()?,
            ExprKind::FunctionArg(ty) => self.eval_function_arg(ty)?,
            ExprKind::Function { .. } => todo!(),
            ExprKind::FunctionCall { func, args } => {
                self.eval_function_call(*func, args)?
            }
            ExprKind::Tuple(elements) => self.eval_tuple(elements)?,
            ExprKind::NativeFunction(_) => todo!(),
            ExprKind::Range { .. } => todo!(),
            ExprKind::Map { .. } => todo!(),
            ExprKind::Chain(_, _) => todo!(),
            ExprKind::Filter { .. } => todo!(),
            ExprKind::First { .. } => todo!(),
            ExprKind::Find { .. } => todo!(),
            ExprKind::FindMap { .. } => todo!(),
            ExprKind::Collect { .. } => todo!(),
            ExprKind::Reduce { .. } => todo!(),
            &ExprKind::SimpleReduce {
                initial,
                extent,
                reduction,
            } => self.eval_simple_reduce(initial, extent, reduction)?,
            ExprKind::StaticField(_) => todo!(),
            ExprKind::FieldAccess { .. } => todo!(),
            ExprKind::SymbolicDowncast { .. } => todo!(),
            ExprKind::IndexAccess { .. } => todo!(),
            ExprKind::NumArrayElements { .. } => todo!(),
            ExprKind::ArrayExtent { .. } => todo!(),
            ExprKind::PointerCast { .. } => todo!(),
            &ExprKind::IfElse {
                condition,
                if_branch,
                else_branch,
            } => self.eval_if_else(condition, if_branch, else_branch)?,
            &ExprKind::And { lhs, rhs } => self.eval_and(lhs, rhs)?,
            &ExprKind::Or { lhs, rhs } => self.eval_or(lhs, rhs)?,
            &ExprKind::Not { arg } => self.eval_not(arg)?,
            &ExprKind::IsSome(value) => self.eval_is_some(value)?,
            &ExprKind::Equal { lhs, rhs } => self.eval_eq(lhs, rhs)?,
            &ExprKind::NotEqual { lhs, rhs } => self.eval_ne(lhs, rhs)?,
            &ExprKind::LessThan { lhs, rhs } => self.eval_lt(lhs, rhs)?,
            &ExprKind::GreaterThan { lhs, rhs } => self.eval_gt(lhs, rhs)?,
            &ExprKind::LessThanOrEqual { lhs, rhs } => {
                self.eval_le(lhs, rhs)?
            }
            &ExprKind::GreaterThanOrEqual { lhs, rhs } => {
                self.eval_ge(lhs, rhs)?
            }
            &ExprKind::Add { lhs, rhs } => self.eval_add(lhs, rhs)?,
            &ExprKind::Sub { lhs, rhs } => self.eval_sub(lhs, rhs)?,
            &ExprKind::Mul { lhs, rhs } => self.eval_mul(lhs, rhs)?,
            &ExprKind::Div { lhs, rhs } => self.eval_div(lhs, rhs)?,
            &ExprKind::Mod { lhs, rhs } => self.eval_mod(lhs, rhs)?,
            &ExprKind::PrimCast { value, prim_type } => {
                self.eval_prim_cast(value, prim_type)?
            }
            ExprKind::IsSubclassOf { .. } => todo!(),
            ExprKind::PhysicalDowncast { .. } => todo!(),
            ExprKind::ReadPrim { .. } => todo!(),
            ExprKind::ReadBytes(_) => todo!(),
            ExprKind::CastBytes { .. } => todo!(),
            ExprKind::ReadString { .. } => todo!(),
        }

        self.current_op = parent_op;

        Ok(())
    }

    fn eval_none(&mut self) -> Result<(), Error> {
        self.push(None::<StackValue>);
        Ok(())
    }

    fn eval_is_some(&mut self, value: SymbolicValue) -> Result<(), Error> {
        self.eval(value)?;
        let value = self.values.pop()?.is_some();
        self.push(Some(value));
        Ok(())
    }

    fn eval_tuple(&mut self, elements: &[SymbolicValue]) -> Result<(), Error> {
        for element in elements {
            self.eval(*element)?;
        }

        Ok(())
    }

    fn eval_prim_cast(
        &mut self,
        value: SymbolicValue,
        ty: RuntimePrimType,
    ) -> Result<(), Error> {
        self.eval(value)?;

        let opt_value = self
            .pop_prim()?
            .map(|value| value.prim_cast(ty))
            .transpose()?;

        self.push(opt_value);

        Ok(())
    }

    define_binary_op! { eval_eq, try_eq }
    define_binary_op! { eval_ne, try_ne }
    define_binary_op! { eval_gt, try_gt }
    define_binary_op! { eval_lt, try_lt }
    define_binary_op! { eval_ge, try_ge }
    define_binary_op! { eval_le, try_le }

    define_binary_op! { eval_and, try_and }
    define_binary_op! { eval_or, try_or }
    fn eval_not(&mut self, arg: SymbolicValue) -> Result<(), Error> {
        self.eval(arg)?;
        let opt_value =
            self.pop_prim()?.map(|arg| arg.try_not()).transpose()?;
        self.push(opt_value);
        Ok(())
    }

    define_binary_op! { eval_add, try_add }
    define_binary_op! { eval_sub, try_sub }
    define_binary_op! { eval_mul, try_mul }
    define_binary_op! { eval_div, try_div }
    define_binary_op! { eval_mod, try_mod }

    fn eval_simple_reduce(
        &mut self,
        initial: SymbolicValue,
        extent: SymbolicValue,
        reduction: SymbolicValue,
    ) -> Result<(), Error> {
        self.eval(initial)?;
        self.eval(extent)?;

        let extent: usize = self
            .pop_prim()?
            .map(|prim| prim.try_into())
            .transpose()?
            .unwrap_or(0usize);

        let reduction_index = match reduction {
            SymbolicValue::Result(index) => index,
            SymbolicValue::Const(prim) => {
                return Err(Error::CannotCallPrimitiveAsFunction {
                    parent_op: self.current_op,
                    used_as_function: prim,
                });
            }
        };
        match &self.graph[reduction_index].kind {
            ExprKind::Function { params, output } => {
                if params.len() != 2 {
                    return Err(Error::InvalidReductionFunction {
                        reduction: reduction_index,
                        num_params: params.len(),
                    });
                }
                let accumulator = self.as_function_arg(params[0])?;
                let index = self.as_function_arg(params[1])?;

                for i in 0..extent {
                    let reduced = self.values.pop()?;

                    let cleanup_acc = self.bind(accumulator, reduced)?;
                    let cleanup_index = self.bind(index, Some(i))?;
                    self.eval(*output)?;

                    cleanup_acc(self)?;
                    cleanup_index(self)?;
                }
            }

            ExprKind::NativeFunction(func) => {
                let sig = func.signature()?;
                match sig {
                    DSLType::Function(FunctionType {
                        params: Some(params),
                        ..
                    }) if params.len() == 2 => Ok(()),
                    other => Err(Error::InvalidNativeReductionFunction {
                        reduction: reduction_index,
                        signature: other,
                    }),
                }?;

                let mut accumulator = self.values.pop()?;
                for i in 0..extent {
                    let mut index: Option<StackValue> = Some(i.into());
                    let mut args = [&mut accumulator, &mut index];
                    if func.mutates_first_argument() {
                        func.apply(&mut args)?;
                    } else {
                        accumulator = func.apply(&mut args)?;
                    }
                }
                self.push(accumulator);
            }

            other => {
                return Err(Error::UnsupportedFunctionExpression {
                    parent_op: self.current_op,
                    used_as_function: other.op_name(),
                });
            }
        }

        Ok(())
    }

    fn eval_function_arg(&mut self, ty: &DSLType) -> Result<(), Error> {
        let var = self.current_op;
        let opt_value_ref: &Option<StackValue> = self
            .bindings
            .get(&var)
            .ok_or_else(|| Error::UndefinedVariable { var })?;

        let opt_value: Option<StackValue> = match opt_value_ref {
            None => None,
            Some(StackValue::Prim(prim)) => Some(StackValue::Prim(*prim)),
            Some(StackValue::ByteArray(bytes)) => {
                Some(StackValue::ByteArray(bytes.clone()))
            }
            Some(StackValue::SmallByteArray(bytes)) => {
                Some(StackValue::SmallByteArray(*bytes))
            }
            Some(StackValue::Native(_)) => self
                .bindings
                .remove(&var)
                .expect("Already checked that `var` is a bound variable."),
        };

        if let Some(value) = opt_value.as_ref() {
            if ty != &DSLType::Unknown && &value.runtime_type() != ty {
                return Err(Error::IncorrectVariableType {
                    var,
                    expected: ty.clone(),
                    actual: opt_value.unwrap(),
                });
            }
        }

        self.push(opt_value);

        Ok(())
    }

    fn eval_function_call(
        &mut self,
        func: SymbolicValue,
        args: &[SymbolicValue],
    ) -> Result<(), Error> {
        let func_index = match func {
            SymbolicValue::Result(index) => index,
            SymbolicValue::Const(prim) => {
                return Err(Error::CannotCallPrimitiveAsFunction {
                    parent_op: self.current_op,
                    used_as_function: prim,
                });
            }
        };

        match &self.graph[func_index].kind {
            ExprKind::Function { params, output } => {
                self.eval_ir_function_call(params, args, *output)
            }
            ExprKind::NativeFunction(func) => {
                self.eval_native_function_call(func, args)
            }

            other => Err(Error::UnsupportedFunctionExpression {
                parent_op: self.current_op,
                used_as_function: other.op_name(),
            }),
        }
    }

    fn eval_ir_function_call(
        &mut self,
        params: &[SymbolicValue],
        args: &[SymbolicValue],
        output: SymbolicValue,
    ) -> Result<(), Error> {
        if params.len() != args.len() {
            return Err(Error::IncorrectNumberOfArguments {
                func: self.current_op,
                num_params: params.len(),
                num_args: args.len(),
            });
        }

        let mut bindings = SmallVec::<[_; 32]>::with_capacity(params.len());
        for (param, arg) in params.iter().cloned().zip(args.iter().cloned()) {
            self.eval(arg)?;
            let arg_val = self.values.pop()?;
            let cleanup = self.bind(self.as_function_arg(param)?, arg_val)?;
            bindings.push(cleanup);
        }

        self.eval(output)?;

        for cleanup in bindings {
            cleanup(self)?;
        }

        Ok(())
    }

    fn eval_native_function_call(
        &mut self,
        func: &ExposedNativeFunction,
        args: &[SymbolicValue],
    ) -> Result<(), Error> {
        let arg_start = self.values.len();
        for arg in args {
            self.eval(*arg)?;
        }
        let opt_output = {
            let mut remaining = &mut self.values[ValueIndex(arg_start)..];
            let mut arg_refs =
                SmallVec::<[&mut Option<StackValue>; 32]>::with_capacity(
                    args.len(),
                );

            while !remaining.is_empty() {
                let (first, rest) = remaining.split_at_mut(1);
                arg_refs.push(&mut first[0]);
                remaining = rest;
            }
            func.apply(&mut arg_refs)?
        };

        if func.mutates_first_argument() {
            self.values.truncate(arg_start + 1);
        } else {
            self.values.truncate(arg_start);
            self.push(opt_output);
        }
        Ok(())
    }

    fn eval_if_else(
        &mut self,
        condition: SymbolicValue,
        if_branch: SymbolicValue,
        else_branch: SymbolicValue,
    ) -> Result<(), Error> {
        self.eval(condition)?;
        let condition: bool = self
            .pop_prim()?
            .map(TryInto::try_into)
            .transpose()?
            .unwrap_or(false);

        if condition {
            self.eval(if_branch)
        } else {
            self.eval(else_branch)
        }
    }
}

impl<'a> RuntimeFunc<'a> for InterpretedFunc<'a> {
    type Error = Error;

    fn with_reader(self, reader: impl Reader + 'a) -> Self {
        self.with_reader(reader)
    }

    fn evaluate(self) -> Result<RuntimeOutput, Self::Error> {
        self.evaluate()
    }
}

impl Into<SymbolicGraph> for Interpreter {
    fn into(self) -> SymbolicGraph {
        self.graph
    }
}
