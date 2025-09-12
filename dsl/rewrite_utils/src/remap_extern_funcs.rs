use std::collections::HashMap;

use dsl_ir::{OpIndex, SymbolicGraph, SymbolicValue};

use crate::Error;

pub trait RemapExternFuncs {
    fn remap_extern_funcs(
        &self,
        builder: &mut SymbolicGraph,
        lookup: &HashMap<OpIndex, SymbolicValue>,
    ) -> Result<(), Error>;
}

impl RemapExternFuncs for SymbolicGraph {
    fn remap_extern_funcs(
        &self,
        builder: &mut SymbolicGraph,
        lookup: &HashMap<OpIndex, SymbolicValue>,
    ) -> Result<(), Error> {
        for old_index in self.iter_extern_funcs() {
            let new_index = lookup
                .get(&old_index)
                .cloned()
                .unwrap_or_else(|| old_index.into());
            builder.mark_extern_func(new_index)?;
        }
        Ok(())
    }
}
