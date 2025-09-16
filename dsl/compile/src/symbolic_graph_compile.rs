use dotnet_debugger::CachedReader;

use dsl_interpreter::Interpreter;
use dsl_ir::SymbolicGraph;
use dsl_vm::VirtualMachine;

use crate::{Error, SymbolicGraphCompiler};

pub trait SymbolicGraphCompile {
    fn compiler<'a, 'b>(&'a self) -> SymbolicGraphCompiler<'a, 'b>;

    fn compile<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<VirtualMachine, Error>;

    fn interpreter<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<Interpreter, Error>;
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

    fn interpreter<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<Interpreter, Error> {
        SymbolicGraphCompiler::from_graph(self)
            .with_reader(reader)
            .interpreter()
    }
}
