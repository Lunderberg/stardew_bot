#![allow(dead_code, unused_imports)]

use std::any::Any;

use memory_reader::Pointer;

use crate::{
    runtime_type::RustType, Error, RuntimePrimType, RuntimePrimValue,
    RuntimeType, StackValue, VMExecutionError,
};

pub trait NativeFunction {
    fn apply(
        &self,
        args: &mut [&mut Option<StackValue>],
    ) -> Result<Option<StackValue>, Error>;
}

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
}

trait WrapReturn {
    fn wrap_return(self) -> Result<Option<StackValue>, Error>;
}

trait UnwrapArg: Sized {
    type Unwrapped<'t>;
    fn unwrap_arg<'t>(
        arg: &'t mut StackValue,
    ) -> Result<Self::Unwrapped<'t>, Error>;
}

macro_rules! impl_prim_return {
    ($variant:ident, $prim:ty) => {
        impl WrapReturn for $prim {
            fn wrap_return(self) -> Result<Option<StackValue>, Error> {
                let prim: RuntimePrimValue = self.into();
                Ok(Some(prim.into()))
            }
        }

        impl UnwrapArg for $prim {
            type Unwrapped<'t> = $prim;
            fn unwrap_arg<'t>(
                arg: &'t mut StackValue,
            ) -> Result<Self::Unwrapped<'t>, Error> {
                match arg {
                    StackValue::Prim(RuntimePrimValue::$variant(prim)) => {
                        Ok(*prim)
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

impl<T: 'static> RustNativeObject for Vec<T> {}

impl<T> WrapReturn for T
where
    T: RustNativeObject,
{
    fn wrap_return(self) -> Result<Option<StackValue>, Error> {
        Ok(Some(StackValue::Any(Box::new(self))))
    }
}

impl WrapReturn for StackValue {
    fn wrap_return(self) -> Result<Option<StackValue>, Error> {
        Ok(Some(self))
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
}

impl WrapReturn for () {
    fn wrap_return(self) -> Result<Option<StackValue>, Error> {
        Ok(None)
    }
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
                const N: usize = count_args!( $($arg_type),* );
                if args.len() != N {
                    return Err(
                        VMExecutionError::InvalidNumberOfOperandsForNativeFunction {
                            expected: N,
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
