use std::collections::HashMap;

use dsl_ir::{OpIndex, SymbolicGraph, SymbolicValue};

use crate::{Error, RemapExternFuncs as _};

pub trait SymbolicGraphDCE: Sized {
    fn dead_code_elimination(self) -> Result<Self, Error>;
}
impl SymbolicGraphDCE for SymbolicGraph {
    fn dead_code_elimination(self) -> Result<Self, Error> {
        let mut prev_index_lookup: HashMap<OpIndex, SymbolicValue> =
            HashMap::new();
        let mut builder = Self::new();

        let reachable = self.reachable(self.iter_extern_funcs());

        for (prev_index, op) in self.iter_ops() {
            if reachable[prev_index.0] {
                let kind = op
                    .kind
                    .try_remap(&prev_index_lookup)
                    .unwrap_or_else(|| op.kind.clone());
                let new_index = builder.push(kind);
                if let Some(name) = &op.name {
                    builder.name(new_index, name)?;
                }

                prev_index_lookup.insert(prev_index, new_index);
            }
        }

        self.remap_extern_funcs(&mut builder, &prev_index_lookup)?;

        Ok(builder)
    }
}
