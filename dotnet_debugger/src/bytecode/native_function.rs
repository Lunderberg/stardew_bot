use crate::{Error, RuntimePrimValue, VMExecutionError};

pub trait NativeFunction {
    fn apply(
        &self,
        args: &mut [Option<RuntimePrimValue>],
    ) -> Result<Option<RuntimePrimValue>, Error>;
}

impl<Func> NativeFunction for Func
where
    Func: Fn(
        &mut [Option<RuntimePrimValue>],
    ) -> Result<Option<RuntimePrimValue>, Error>,
{
    fn apply(
        &self,
        args: &mut [Option<RuntimePrimValue>],
    ) -> Result<Option<RuntimePrimValue>, Error> {
        self(args)
    }
}

trait WrapReturn {
    fn wrap_return(self) -> Result<Option<RuntimePrimValue>, Error>;
}
trait UnwrapArg: Sized {
    fn unwrap_arg(arg: RuntimePrimValue) -> Result<Self, Error>;
}

impl<T> WrapReturn for T
where
    T: Into<RuntimePrimValue>,
{
    fn wrap_return(self) -> Result<Option<RuntimePrimValue>, Error> {
        Ok(Some(self.into()))
    }
}
impl<T> WrapReturn for Option<T>
where
    T: Into<RuntimePrimValue>,
{
    fn wrap_return(self) -> Result<Option<RuntimePrimValue>, Error> {
        Ok(self.map(Into::into))
    }
}
impl<T, E> WrapReturn for Result<Option<T>, E>
where
    T: Into<RuntimePrimValue>,
    E: Into<Error>,
{
    fn wrap_return(self) -> Result<Option<RuntimePrimValue>, Error> {
        match self {
            Ok(Some(val)) => Ok(Some(val.into())),
            Ok(None) => Ok(None),
            Err(err) => Err(err.into()),
        }
    }
}

impl<T> UnwrapArg for T
where
    RuntimePrimValue: TryInto<T, Error = Error>,
{
    fn unwrap_arg(arg: RuntimePrimValue) -> Result<Self, Error> {
        arg.try_into()
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

macro_rules! impl_wrapped_native_function {
    ( $($arg_type:ident),* ) => {
        // When defining for nullary function, the second parameter is
        // `(Return)` rather than `Return`.  But since I can't get rid
        // of the parentheses without breaking the macro for other
        // cases, silencing the warning instead.
        #[allow(unused_parens)]
        impl<Func, Return $(, $arg_type )*> NativeFunction
            for WrappedNativeFunction<Func, (Return $(,$arg_type)*)>
            where
            Func: Fn( $($arg_type,)* ) -> Return,
            Return: WrapReturn,
            $(
               $arg_type: UnwrapArg,
            )*

        {
            fn apply(&self, args: &mut [Option<RuntimePrimValue>])
                     -> Result<Option<RuntimePrimValue>,Error> {
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
                let mut arg_iter = args.iter();
                let result = (self.func)(
                    $({
                        let opt_arg = arg_iter
                            .next()
                            .expect("Handled by earlier size check.");
                        let Some(arg) = opt_arg else {
                            return Ok(None);
                        };
                        $arg_type::unwrap_arg(*arg)?
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
