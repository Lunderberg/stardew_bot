use dotnet_debugger::CachedReader;
use dsl_ir::SymbolicGraph;
use dsl_vm::VirtualMachine;

use crate::{Error, SymbolicGraphCompiler};

pub trait SymbolicGraphCompile {
    fn compiler<'a, 'b>(&'a self) -> SymbolicGraphCompiler<'a, 'b>;

    fn compile<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<VirtualMachine, Error>;
}
impl SymbolicGraphCompile for SymbolicGraph {
    fn compiler<'a, 'b>(&'a self) -> SymbolicGraphCompiler<'a, 'b> {
        SymbolicGraphCompiler::from_graph(self)
    }

    fn compile<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<VirtualMachine, Error> {
        SymbolicGraphCompiler::from_graph(self)
            .with_reader(reader)
            .compile()
    }
}
