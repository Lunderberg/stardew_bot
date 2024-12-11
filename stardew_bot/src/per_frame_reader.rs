use dotnet_debugger::{
    CachedReader, RuntimePrimValue, SymbolicExpr, VirtualMachine,
};

use crate::Error;

#[derive(Default)]
pub struct ExpressionsToRead {
    values: Vec<SymbolicExpr>,
}

pub struct PerFrameReader {
    vm: VirtualMachine,
}

pub struct PerFrameValues {
    values: Vec<Option<RuntimePrimValue>>,
}

#[derive(Clone, Copy)]
pub struct ValueToken(usize);

impl ExpressionsToRead {
    pub fn register(&mut self, expr: SymbolicExpr) -> ValueToken {
        self.values.push(expr);
        ValueToken(self.values.len() - 1)
    }

    pub fn compile(
        &self,
        reader: CachedReader,
    ) -> Result<PerFrameReader, Error> {
        let expr: SymbolicExpr = self.values.clone().into();
        let vm = expr.compile(reader)?;
        Ok(PerFrameReader { vm })
    }
}
impl PerFrameReader {
    pub fn read(&self, reader: CachedReader) -> Result<PerFrameValues, Error> {
        let values = self.vm.evaluate(reader)?;
        Ok(PerFrameValues { values })
    }
}

impl std::ops::Index<ValueToken> for PerFrameValues {
    type Output = Option<RuntimePrimValue>;

    fn index(&self, index: ValueToken) -> &Self::Output {
        &self.values[index.0]
    }
}
