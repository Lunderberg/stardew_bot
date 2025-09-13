use std::collections::HashMap;

use dsl_ir::{OpIndex, SymbolicGraph, SymbolicValue};

use crate::{GraphRewrite, RemapExternFuncs as _};

pub trait SymbolicGraphRewrite: Sized {
    fn rewrite<Error>(
        &self,
        rewriter: impl GraphRewrite<Error = Error>,
    ) -> Result<Self, Error>
    where
        Error: From<dsl_ir::Error>,
        Error: From<crate::Error>;

    fn rewrite_verbose<Error>(
        &self,
        rewriter: impl GraphRewrite<Error = Error>,
    ) -> Result<RewriteResults, Error>
    where
        Error: From<dsl_ir::Error>,
        Error: From<crate::Error>;
}

pub struct RewriteResults {
    /// The resulting graph
    pub graph: SymbolicGraph,

    /// The number of terms rewritten by the GraphRewrite rule.
    pub num_rewritten_terms: usize,

    /// The number of terms which have been updated to reference a
    /// rewritten term, directly or indirectly.
    pub num_terms_with_new_inputs: usize,
}

impl SymbolicGraphRewrite for SymbolicGraph {
    fn rewrite<Error>(
        &self,
        rewriter: impl GraphRewrite<Error = Error>,
    ) -> Result<Self, Error>
    where
        Error: From<dsl_ir::Error>,
        Error: From<crate::Error>,
    {
        Ok(self.rewrite_verbose(rewriter)?.graph)
    }

    fn rewrite_verbose<Error>(
        &self,
        rewriter: impl GraphRewrite<Error = Error>,
    ) -> Result<RewriteResults, Error>
    where
        Error: From<dsl_ir::Error>,
        Error: From<crate::Error>,
    {
        rewriter.init();

        let mut num_rewritten_terms = 0;
        let mut num_terms_with_new_inputs = 0;

        let mut prev_index_lookup: HashMap<OpIndex, SymbolicValue> =
            HashMap::new();
        let mut builder = Self::new();

        for (old_index, op) in self.iter_ops() {
            let opt_remapped = op.kind.try_remap(&prev_index_lookup);
            let kind = if let Some(remapped) = &opt_remapped {
                num_terms_with_new_inputs += 1;
                remapped
            } else {
                &op.kind
            };

            let opt_value = rewriter.rewrite_expr(
                &mut builder,
                kind,
                op.name.as_deref(),
            )?;
            let value = if let Some(value) = opt_value {
                num_rewritten_terms += 1;
                value
            } else if let Some(remapped) = opt_remapped {
                builder.push(remapped)
            } else {
                builder.push(op.clone())
            };

            // If the pre-rewrite value had a name, then copy it to
            // the post-rewrite value.  This should only be applied if
            // the post-rewrite value does not already have a name.
            // For example, in `let y = x+0`, both `x` and `y` are
            // named values.  When `y` gets replaced with `x`, it
            // should *NOT* cause `x` to be renamed in earlier parts
            // of the function.
            if let Some(name) = &op.name {
                if let SymbolicValue::Result(new_index) = value {
                    if builder[new_index].name.is_none() {
                        builder.name(value, name)?;
                    }
                }
            }
            if value != SymbolicValue::Result(old_index) {
                prev_index_lookup.insert(old_index, value);
            }
        }

        self.remap_extern_funcs(&mut builder, &prev_index_lookup)?;

        Ok(RewriteResults {
            graph: builder,
            num_rewritten_terms,
            num_terms_with_new_inputs,
        })
    }
}
