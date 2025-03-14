use dotnet_debugger::{
    bytecode::virtual_machine::VMReader, CachedReader, StackValue,
    SymbolicGraph, SymbolicValue, VMResults, VirtualMachine,
};

use crate::Error;

#[derive(Default)]
pub struct WatchPointDefinition {
    graph: SymbolicGraph,
    outputs: Vec<SymbolicValue>,
}

pub struct WatchPoint {
    vm: VirtualMachine,
}

pub struct WatchPointResults {
    vm_results: VMResults,
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct ValueToken(pub(crate) usize);

impl WatchPointDefinition {
    pub(crate) fn mark_output(&mut self, value: SymbolicValue) -> ValueToken {
        let token = ValueToken(self.outputs.len());
        self.outputs.push(value);
        token
    }

    pub(crate) fn finalize<'a>(
        mut self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<WatchPoint, Error> {
        let output = self.graph.tuple(self.outputs);
        let main_func = self.graph.function_def(vec![], output);
        self.graph.name(main_func, "main")?;
        self.graph.mark_extern_func(main_func)?;
        let vm = self.graph.compile(reader)?;
        Ok(WatchPoint { vm })
    }
}

impl WatchPoint {
    pub fn evaluate(
        &self,
        reader: impl VMReader,
    ) -> Result<WatchPointResults, Error> {
        let vm_results = self.vm.evaluate(reader)?;
        Ok(WatchPointResults { vm_results })
    }
}

impl std::ops::Deref for WatchPointDefinition {
    type Target = SymbolicGraph;

    fn deref(&self) -> &Self::Target {
        &self.graph
    }
}
impl std::ops::DerefMut for WatchPointDefinition {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.graph
    }
}

impl std::ops::Index<ValueToken> for WatchPointResults {
    type Output = Option<StackValue>;

    fn index(&self, index: ValueToken) -> &Self::Output {
        &self.vm_results[index.0]
    }
}
