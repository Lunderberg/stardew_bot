#![allow(dead_code, unused_imports)]

use std::{any::Any, rc::Rc};

use memory_reader::Pointer;

use crate::{
    runtime_type::{FunctionType, RustType},
    Error, RuntimePrimType, RuntimePrimValue, RuntimeType, StackValue,
    TypeInferenceError, VMExecutionError,
};

pub trait NativeFunction {
    fn apply(
        &self,
        args: &mut [&mut Option<StackValue>],
    ) -> Result<Option<StackValue>, Error>;

    fn signature(&self) -> Result<RuntimeType, Error>;

    fn mutates_first_argument(&self) -> bool;
}

/// A marker trait for Rust-native types that should be exposed
/// through the VM.
///
/// For any type implementing `RustNativeObject`, a blanket
/// implementation provides an implementation of `UnwrapArg`.
/// Unfortunately, this does mean that these types `impl
/// RustNativeObject for Foo {}` whenever they are used.  This
/// requirement is necessary, to keep the blanket implementation of
/// `UnwrapArg` from overlapping with the implementation of
/// `UnwrapArg` for primitive types.
pub trait RustNativeObject: Any {}

impl<Func> NativeFunction for Func
where
    Func: Fn(&[&mut Option<StackValue>]) -> Result<Option<StackValue>, Error>,
{
    fn apply(
        &self,
        args: &mut [&mut Option<StackValue>],
    ) -> Result<Option<StackValue>, Error> {
        self(args)
    }

    fn signature(&self) -> Result<RuntimeType, Error> {
        let sig = FunctionType {
            params: None,
            output: Box::new(RuntimeType::Unknown),
        };
        Ok(sig.into())
    }

    fn mutates_first_argument(&self) -> bool {
        false
    }
}

trait WrapReturn {
    fn wrap_return(self) -> Result<Option<StackValue>, Error>;

    fn return_signature_type() -> RuntimeType;

    const IS_VOID: bool = false;
}

trait UnwrapArg: Sized {
    type Unwrapped<'t>;
    fn unwrap_arg<'t>(
        arg: &'t mut StackValue,
    ) -> Result<Self::Unwrapped<'t>, Error>;

    fn arg_signature_type() -> RuntimeType;

    const IS_MUTABLE: bool = false;
}

macro_rules! impl_prim_return {
    ($variant:ident, $prim:ty) => {
        impl WrapReturn for $prim {
            fn wrap_return(self) -> Result<Option<StackValue>, Error> {
                let prim: RuntimePrimValue = self.into();
                Ok(Some(prim.into()))
            }

            fn return_signature_type() -> RuntimeType {
                RuntimeType::Prim(RuntimePrimType::$variant)
            }
        }

        impl UnwrapArg for $prim {
            type Unwrapped<'t> = $prim;
            fn unwrap_arg<'t>(
                arg: &'t mut StackValue,
            ) -> Result<Self::Unwrapped<'t>, Error> {
                match arg {
                    // Some primitive types may have automatic type
                    // conversions applied.  Therefore, when passing a
                    // primitive by value, use TryInto to apply these
                    // conversions rather than explicitly unwrapping
                    // the RuntimePrimValue.
                    StackValue::Prim(prim) => (*prim).try_into(),
                    other => Err(
                        VMExecutionError::InvalidArgumentForNativeFunction {
                            expected: RuntimeType::Prim(
                                RuntimePrimType::$variant,
                            ),
                            actual: other.runtime_type(),
                        }
                        .into(),
                    ),
                }
            }

            fn arg_signature_type() -> RuntimeType {
                RuntimeType::Prim(RuntimePrimType::$variant)
            }
        }

        impl<'s> UnwrapArg for &'s $prim {
            type Unwrapped<'t> = &'t $prim;

            fn unwrap_arg<'t>(
                arg: &'t mut StackValue,
            ) -> Result<Self::Unwrapped<'t>, Error> {
                match arg {
                    StackValue::Prim(RuntimePrimValue::$variant(prim)) => {
                        Ok(prim)
                    }
                    other => Err(
                        VMExecutionError::InvalidArgumentForNativeFunction {
                            expected: RuntimeType::Prim(
                                RuntimePrimType::$variant,
                            ),
                            actual: other.runtime_type(),
                        }
                        .into(),
                    ),
                }
            }

            fn arg_signature_type() -> RuntimeType {
                RuntimeType::Prim(RuntimePrimType::$variant)
            }
        }

        impl<'s> UnwrapArg for &'s mut $prim {
            type Unwrapped<'t> = &'t mut $prim;

            fn unwrap_arg<'t>(
                arg: &'t mut StackValue,
            ) -> Result<Self::Unwrapped<'t>, Error> {
                match arg {
                    StackValue::Prim(RuntimePrimValue::$variant(prim)) => {
                        Ok(prim)
                    }
                    other => Err(
                        VMExecutionError::InvalidArgumentForNativeFunction {
                            expected: RuntimeType::Prim(
                                RuntimePrimType::$variant,
                            ),
                            actual: other.runtime_type(),
                        }
                        .into(),
                    ),
                }
            }

            fn arg_signature_type() -> RuntimeType {
                RuntimeType::Prim(RuntimePrimType::$variant)
            }

            const IS_MUTABLE: bool = true;
        }
    };
}

impl_prim_return!(Bool, bool);
impl_prim_return!(U8, u8);
impl_prim_return!(U16, u16);
impl_prim_return!(U32, u32);
impl_prim_return!(U64, u64);
impl_prim_return!(NativeUInt, usize);
impl_prim_return!(I8, i8);
impl_prim_return!(I16, i16);
impl_prim_return!(I32, i32);
impl_prim_return!(I64, i64);
impl_prim_return!(NativeInt, isize);
impl_prim_return!(F32, f32);
impl_prim_return!(F64, f64);
impl_prim_return!(Ptr, Pointer);

impl RustNativeObject for String {}
impl<T: 'static> RustNativeObject for Vec<T> {}

impl<T> WrapReturn for T
where
    T: RustNativeObject,
{
    fn wrap_return(self) -> Result<Option<StackValue>, Error> {
        Ok(Some(StackValue::Any(Box::new(self))))
    }

    fn return_signature_type() -> RuntimeType {
        std::any::TypeId::of::<T>().into()
    }
}

impl WrapReturn for StackValue {
    fn wrap_return(self) -> Result<Option<StackValue>, Error> {
        Ok(Some(self))
    }

    fn return_signature_type() -> RuntimeType {
        RuntimeType::Unknown
    }
}

impl<T> WrapReturn for Option<T>
where
    T: WrapReturn,
{
    fn wrap_return(self) -> Result<Option<StackValue>, Error> {
        match self {
            Some(value) => value.wrap_return(),
            None => Ok(None),
        }
    }

    fn return_signature_type() -> RuntimeType {
        <T as WrapReturn>::return_signature_type()
    }
}

impl<T, E> WrapReturn for Result<T, E>
where
    T: WrapReturn,
    E: Into<Error>,
{
    fn wrap_return(self) -> Result<Option<StackValue>, Error> {
        match self {
            Ok(value) => value.wrap_return(),
            Err(err) => Err(err.into()),
        }
    }

    fn return_signature_type() -> RuntimeType {
        <T as WrapReturn>::return_signature_type()
    }
}

impl WrapReturn for () {
    fn wrap_return(self) -> Result<Option<StackValue>, Error> {
        Ok(None)
    }

    fn return_signature_type() -> RuntimeType {
        RuntimeType::Unknown
    }

    const IS_VOID: bool = true;
}

impl<'b, T> UnwrapArg for &'b T
where
    T: RustNativeObject,
{
    type Unwrapped<'a> = &'a T;

    fn unwrap_arg<'a>(
        arg: &'a mut StackValue,
    ) -> Result<Self::Unwrapped<'a>, Error> {
        let err: Error = VMExecutionError::InvalidArgumentForNativeFunction {
            expected: RuntimeType::Rust(RustType::Opaque(
                std::any::TypeId::of::<T>(),
            )),
            actual: arg.runtime_type(),
        }
        .into();
        match arg {
            StackValue::Any(any) => any.downcast_ref().ok_or(err),
            _ => Err(err),
        }
    }

    fn arg_signature_type() -> RuntimeType {
        std::any::TypeId::of::<T>().into()
    }
}

impl<'b, T> UnwrapArg for &'b mut T
where
    T: RustNativeObject,
{
    type Unwrapped<'a> = &'a mut T;

    fn unwrap_arg<'a>(
        arg: &'a mut StackValue,
    ) -> Result<Self::Unwrapped<'a>, Error> {
        let err: Error = VMExecutionError::InvalidArgumentForNativeFunction {
            expected: RuntimeType::Rust(RustType::Opaque(
                std::any::TypeId::of::<T>(),
            )),
            actual: arg.runtime_type(),
        }
        .into();
        match arg {
            StackValue::Any(any) => any.downcast_mut().ok_or(err),
            _ => Err(err),
        }
    }

    fn arg_signature_type() -> RuntimeType {
        std::any::TypeId::of::<T>().into()
    }

    const IS_MUTABLE: bool = true;
}

pub struct WrappedNativeFunction<Func, ArgList> {
    func: Func,
    _phantom: std::marker::PhantomData<ArgList>,
}
impl<Func, ArgList> WrappedNativeFunction<Func, ArgList> {
    pub(crate) fn new(func: Func) -> Self {
        Self {
            func,
            _phantom: std::marker::PhantomData,
        }
    }
}

macro_rules! count_args {
    () => { 0 };
    ($first:ident $(, $rest:ident )* ) => {
        1 + count_args!( $($rest),* )
    }
}

#[allow(unused_macros)]
macro_rules! impl_wrapped_native_function {
    ( $($arg_type:ident),* ) => {


        impl<Func, Return $(, $arg_type )*> NativeFunction
            for WrappedNativeFunction<Func, ($($arg_type,)*)>
        where
            Func: Fn( $( $arg_type ),* ) -> Return,
            Return: WrapReturn,
        $(
            $arg_type: UnwrapArg,
        )*
            Func: Fn(
                $( <$arg_type as UnwrapArg>::Unwrapped<'_> ),*
            ) -> Return,

        {
            fn apply(&self, args: &mut [&mut Option<StackValue>])
                     -> Result<Option<StackValue>,Error>
            {

                const NUM_ARGS: usize = count_args!( $($arg_type),* );
                if args.len() != NUM_ARGS {
                    return Err(
                        VMExecutionError::InvalidNumberOfOperandsForNativeFunction {
                            expected: NUM_ARGS,
                            provided: args.len(),
                        }
                        .into(),
                    );
                }

                // When defining for nullary function, `arg_iter` is
                // unused.  But both the variable and the mutability
                // are required for all other cases, so the warning
                // gets silenced.
                #[allow(unused_variables, unused_mut)]
                let mut arg_iter = args.into_iter();
                let result = (self.func)(
                    $({
                        let opt_arg = arg_iter
                            .next()
                            .expect("Handled by earlier size check.");
                        let Some(arg) = opt_arg else {
                            return Ok(None);
                        };
                        let unwrapped = $arg_type::unwrap_arg(arg)?;
                        unwrapped
                    }),*
                );

                result.wrap_return()
            }

            fn signature(&self) -> Result<RuntimeType,Error> {
                let params = vec![
                    $(
                        <$arg_type as UnwrapArg>::arg_signature_type(),
                    )*
                ];
                let output = <Return as WrapReturn>::return_signature_type();

                const NUM_ARGS: usize = count_args!( $($arg_type),* );
                let mutability: [bool; NUM_ARGS] = [
                    $( <$arg_type as UnwrapArg>::IS_MUTABLE, )*
                ];
                let sig = if mutability.iter().all(|b| !b) {
                    // All parameters are immutable, represent the
                    // signature as written.
                    Ok(FunctionType{
                        params: Some(params),
                        output: Box::new(output),
                    })
                } else if let Some(i_param) = mutability
                    .iter()
                    .enumerate()
                    .skip(1)
                    .find(|(_,mutable)| **mutable)
                    .map(|(i_param,_)| i_param)
                {
                    // A parameter other than the first parameter is
                    // mutable, which is not supported.
                    let sig = format!(
                        "{}",
                        FunctionType{
                            params: Some(params),
                            output: Box::new(output),
                        },
                    );
                    let reason = format!(
                        "mutability is only supported for the first parameter \
                         but parameter {i_param} is accepted as a mutable reference");
                    Err(TypeInferenceError::UnsupportedNativeFunction{
                        sig,
                        reason,
                    })

                } else if !<Return as WrapReturn>::IS_VOID {
                    // A mutable parameter is only supported for void
                    // outputs.  In the SymbolicGraph, the function's
                    // output is used to represent the mutable
                    // argument after.
                    //
                    // TODO: Add analysis routine to ensure a single
                    // use of the pre-mutation parameter.
                    let sig = format!(
                        "{}",
                        FunctionType{
                            params: Some(params),
                            output: Box::new(output),
                        },
                    );
                    let sig = format!("{sig}");
                    let reason = "for a mutable first parameter \
                                  the output type must be void,\
                                  so that the SymbolicGraph can represent \
                                  the mutated argument as the output".into();
                    Err(TypeInferenceError::UnsupportedNativeFunction{
                        sig,
                        reason,
                    })
                } else {

                    Ok(FunctionType{
                        output: Box::new(params[0].clone()),
                        params: Some(params),
                    })
                }?;

                Ok(sig.into())
            }

            fn mutates_first_argument(&self) -> bool {
                const NUM_ARGS: usize = count_args!( $($arg_type),* );
                let mutability: [bool; NUM_ARGS] = [
                    $( <$arg_type as UnwrapArg>::IS_MUTABLE, )*
                ];
                NUM_ARGS > 0 && mutability[0]
            }
        }
    };
}

// Twelve function arguments should be good enough, right?  Any more,
// and the function is clearly doing something weird, and can always
// fall back to the `with_raw_native_function` instead of
// `with_native_function`.
impl_wrapped_native_function! {}
impl_wrapped_native_function! {T1}
impl_wrapped_native_function! {T1,T2}
impl_wrapped_native_function! {T1,T2,T3}
impl_wrapped_native_function! {T1,T2,T3,T4}
impl_wrapped_native_function! {T1,T2,T3,T4,T5}
impl_wrapped_native_function! {T1,T2,T3,T4,T5,T6}
impl_wrapped_native_function! {T1,T2,T3,T4,T5,T6,T7}
impl_wrapped_native_function! {T1,T2,T3,T4,T5,T6,T7,T8}
impl_wrapped_native_function! {T1,T2,T3,T4,T5,T6,T7,T8,T9}
impl_wrapped_native_function! {T1,T2,T3,T4,T5,T6,T7,T8,T9,T10}
impl_wrapped_native_function! {T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11}
impl_wrapped_native_function! {T1,T2,T3,T4,T5,T6,T7,T8,T9,T10,T11,T12}
