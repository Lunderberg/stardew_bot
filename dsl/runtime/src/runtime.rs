use crate::{Reader, RuntimeOutput};

pub trait Runtime {
    type Error;
    type Func<'a>: RuntimeFunc<'a, Error = Self::Error>
    where
        Self: 'a;

    fn get_function<'a>(
        &'a self,
        name: &str,
    ) -> Result<Self::Func<'a>, Self::Error>;

    fn local_eval(&self) -> Result<RuntimeOutput, Self::Error> {
        self.get_function("main")?.evaluate()
    }

    fn evaluate(
        &self,
        reader: impl Reader,
    ) -> Result<RuntimeOutput, Self::Error> {
        self.get_function("main")?.with_reader(reader).evaluate()
    }
}

pub trait RuntimeFunc<'a> {
    type Error;

    fn with_reader(self, reader: impl Reader + 'a) -> Self;

    fn evaluate(self) -> Result<RuntimeOutput, Self::Error>;
}
