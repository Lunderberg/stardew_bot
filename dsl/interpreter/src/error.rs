use thiserror::Error;

use dsl_ir::{DSLType, OpIndex, RuntimePrimValue, StackValue};

#[derive(Error)]
pub enum Error {
    #[error("memory_reader::Error( {0} )")]
    MemoryReader(#[from] memory_reader::Error),

    #[error("dotnet_debugger::Error( {0} )")]
    DotnetDebugger(#[from] dotnet_debugger::Error),

    #[error("dsl::ir::Error( {0} )")]
    DslIr(#[from] dsl_ir::Error),

    #[error("dsl::runtime::Error( {0} )")]
    DslRuntime(#[from] dsl_runtime::Error),

    #[error("Interpreter did not contain a function named '{0}'")]
    NoSuchFunction(String),

    #[error("Externally-called operation is not a function definition")]
    CalledOpIsNotFunction,

    #[error("Input arguments to interpreted function not yet implemented")]
    CalledFunctionHasInputParameters,

    #[error(
        "Operator {operator} at location {index} \
         requires primitive operands, \
         and does not support operand of type {arg_type:?}."
    )]
    OperatorExpectsPrimitiveArgument {
        operator: &'static str,
        index: OpIndex,
        arg_type: DSLType,
    },

    #[error(
        "Cannot apply operator '{op}' with operand types '{lhs}' and '{rhs}'"
    )]
    InvalidOperandsForBinaryOp {
        op: &'static str,
        lhs: DSLType,
        rhs: DSLType,
    },

    #[error(
        "Expected function to be ExprKind::Function, \
         or a rust-native function object, \
         but {parent_op} instead found \
         {used_as_function} of type {}.",
        used_as_function.runtime_type(),
    )]
    CannotCallPrimitiveAsFunction {
        parent_op: OpIndex,
        used_as_function: RuntimePrimValue,
    },

    #[error(
        "Expected function to be ExprKind::Function, \
         or a rust-native function object, \
         but {parent_op} instead found {used_as_function}."
    )]
    UnsupportedFunctionExpression {
        parent_op: OpIndex,
        used_as_function: &'static str,
    },

    #[error(
        "Expected callee in closure to be ExprKind::Function, \
         but {parent_op} instead found {used_as_function}."
    )]
    InvalidClosureExpression {
        parent_op: OpIndex,
        used_as_function: &'static str,
    },

    #[error(
        "Each function parameter must be a reference \
         to an instance of ExprKind::FunctionArg.  \
         However, expression of type {used_as_param} \
         was used as a function parameter."
    )]
    InvalidFunctionDefinition { used_as_param: &'static str },

    #[error(
        "Cannot bind variable {var}, \
         because it is already bound to a value."
    )]
    CannotRebindVariable { var: OpIndex },

    #[error("Variable {var} is undefined at point of use")]
    UndefinedVariable { var: OpIndex },

    #[error(
        "Expected variable {var} to have type {expected}.  \
         However, it instead contained {actual}, \
         which is of type {}",
        actual.runtime_type(),
    )]
    IncorrectVariableType {
        var: OpIndex,
        expected: DSLType,
        actual: StackValue,
    },

    #[error(
        "Function {func} expects {num_params} parameters, \
         but was called with {num_args} arguments."
    )]
    IncorrectNumberOfArguments {
        func: OpIndex,
        num_params: usize,
        num_args: usize,
    },
}

impl std::fmt::Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
