use dsl_ir::{SymbolicGraph, SymbolicValue};

pub trait CopySymbolicName {
    fn copy_name(
        &mut self,
        copy_from: impl Into<SymbolicValue>,
        copy_to: impl Into<SymbolicValue>,
    ) -> Result<(), dsl_ir::Error>;
}

impl CopySymbolicName for SymbolicGraph {
    fn copy_name(
        &mut self,
        from_value: impl Into<SymbolicValue>,
        to_value: impl Into<SymbolicValue>,
    ) -> Result<(), dsl_ir::Error> {
        let from_value = from_value.into();
        let to_value = to_value.into();

        let Some(from_index) = from_value.as_op_index() else {
            return Ok(());
        };
        let Some(to_index) = to_value.as_op_index() else {
            return Ok(());
        };

        if self[to_index].name.is_some() {
            return Ok(());
        }

        if let Some(name) = &self[from_index].name {
            let name = name.to_string();
            self.name(to_value, name)?;
        }

        Ok(())
    }
}
