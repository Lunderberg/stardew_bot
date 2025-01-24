use crate::{Error, RuntimePrimValue};

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
