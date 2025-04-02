use std::rc::Rc;

use super::{native_function::WrappedNativeFunction, NativeFunction};

/// Wrapper trait, solely so that it can be used within structs that
/// derive Debug.
#[derive(Clone)]
pub struct ExposedNativeFunction(Rc<dyn NativeFunction>);

impl ExposedNativeFunction {
    pub fn new(func: impl NativeFunction + 'static) -> Self {
        Self(Rc::new(func))
    }

    pub fn from_closure<Func, ArgList>(func: Func) -> Self
    where
        WrappedNativeFunction<Func, ArgList>: NativeFunction,
        WrappedNativeFunction<Func, ArgList>: 'static,
    {
        let wrapped = WrappedNativeFunction::new(func);
        wrapped.into()
    }
}

impl From<Rc<dyn NativeFunction>> for ExposedNativeFunction {
    fn from(value: Rc<dyn NativeFunction>) -> Self {
        Self(value)
    }
}

impl<T> From<T> for ExposedNativeFunction
where
    T: NativeFunction + 'static,
{
    fn from(value: T) -> Self {
        ExposedNativeFunction::new(value)
    }
}

impl std::fmt::Display for ExposedNativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ptr = Rc::as_ptr(&self.0) as *const ();
        let sig = self
            .signature()
            .expect("Signature should be validated on construction");
        write!(f, "NativeFunction({ptr:p}, {sig})",)
    }
}

impl std::fmt::Debug for ExposedNativeFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        struct FormatPtr<T>(*const T);
        impl<T> std::fmt::Debug for FormatPtr<T> {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{:p}", self.0)
            }
        }

        f.debug_tuple("ExposedNativeFunction")
            .field(&FormatPtr(Rc::as_ptr(&self.0) as *const ()))
            .finish()
    }
}

impl std::ops::Deref for ExposedNativeFunction {
    type Target = dyn NativeFunction;

    fn deref(&self) -> &Self::Target {
        self.0.as_ref()
    }
}

impl std::cmp::PartialEq for ExposedNativeFunction {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(Rc::as_ptr(&self.0), Rc::as_ptr(&other.0))
    }
}
impl std::cmp::Eq for ExposedNativeFunction {}

impl std::hash::Hash for ExposedNativeFunction {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        Rc::as_ptr(&self.0).hash(state);
    }
}
