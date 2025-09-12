use std::collections::{HashMap, HashSet};

use env_var_flag::env_var_flag;

use dotnet_debugger::CachedReader;

use dsl_analysis::{Analysis, TypeInference};
use dsl_ir::{
    DSLType, Expr, ExprKind, FunctionType, OpIndex, SymbolicGraph,
    SymbolicValue,
};
use dsl_vm::VirtualMachine;

use crate::{
    expr_to_virtual_machine::SymbolicGraphToVirtualMachine as _, Error,
    GraphRewrite,
};

struct RewriteResults {
    /// The resulting graph
    graph: SymbolicGraph,

    /// The number of terms rewritten by the GraphRewrite rule.
    num_rewritten_terms: usize,

    /// The number of terms which have been updated to reference a
    /// rewritten term, directly or indirectly.
    num_terms_with_new_inputs: usize,
}

pub struct SymbolicGraphCompiler<'a, 'b> {
    graph: &'a SymbolicGraph,
    show_steps: bool,
    interactive_substeps: bool,
    reader: Option<CachedReader<'b>>,
    optimize_symbolic_graph: bool,
}

pub(crate) trait CopyFirstParamExt {
    fn copy_first_param(&mut self, func: SymbolicValue) -> SymbolicValue;
}
impl CopyFirstParamExt for SymbolicGraph {
    fn copy_first_param(&mut self, func: SymbolicValue) -> SymbolicValue {
        let SymbolicValue::Result(func) = func else {
            panic!(
                "Internal error, \
                 SymbolicValue should point to function \
                 for copy_first_param"
            )
        };
        let params = match &self[func].kind {
            ExprKind::Function { params, .. } => params,
            ExprKind::NativeFunction(func) => {
                let sig = func.signature().unwrap();
                match sig {
                    DSLType::Function(FunctionType { params, .. }) => {
                        let param_ty = params
                            .map(|params| params[0].clone())
                            .unwrap_or(DSLType::Unknown);
                        return self.function_arg(param_ty);
                    }
                    _ => panic!(
                        "Internal error, \
                         NativeFunction should return TunctionType"
                    ),
                }
            }
            _ => panic!(
                "Internal error, \
                 SymbolicValue should point to function \
                 for copy_first_param"
            ),
        };

        let SymbolicValue::Result(first_param_index) = params[0] else {
            panic!(
                "Internal error, \
                 All function parameters \
                 should point to FunctionArg"
            )
        };

        let first_param = &self[first_param_index];

        let ExprKind::FunctionArg(param_ty) = &first_param.kind else {
            panic!(
                "Internal error, \
                 All function parameters \
                 should point to FunctionArg"
            )
        };

        let opt_name = first_param.name.clone();
        let new_param = self.function_arg(param_ty.clone());
        if let Some(name) = opt_name {
            self.name(new_param, name).expect(
                "Internal error, \
                 Existing name must already be valid",
            );
        }

        new_param
    }
}

pub trait SymbolicGraphValidation {
    fn validate_types(
        &self,
        reader: Option<CachedReader<'_>>,
    ) -> Result<(), Error>;

    fn validate_only_back_references(&self) -> Result<(), Error>;

    fn validate_unique_parameter_owner_among_reachable_functions(
        &self,
    ) -> Result<(), Error>;

    fn validate_all_parameters_defined(&self) -> Result<(), Error>;

    fn validate(&self, reader: Option<CachedReader<'_>>) -> Result<(), Error>;
}
impl SymbolicGraphValidation for SymbolicGraph {
    fn validate_types(
        &self,
        reader: Option<CachedReader<'_>>,
    ) -> Result<(), Error> {
        let type_inference = TypeInference::new(reader);

        for (index, _) in self.iter_ops() {
            type_inference.infer_type(self, index.into())?;
        }

        Ok(())
    }

    fn validate_only_back_references(&self) -> Result<(), Error> {
        for (index, op) in self.iter_ops() {
            let mut result = Ok(());
            op.iter_input_nodes().for_each(|prev_index| {
                if prev_index.0 >= index.0 {
                    result = Err(Error::InvalidReference {
                        from: index,
                        to: prev_index,
                    });
                }
            });
            result?;
        }

        Ok(())
    }

    fn validate_unique_parameter_owner_among_reachable_functions(
        &self,
    ) -> Result<(), Error> {
        let iter_functions = self
            .reachable(self.iter_extern_funcs())
            .into_iter()
            .enumerate()
            .filter(|(_, reachable)| *reachable)
            .map(|(i, _)| OpIndex::new(i))
            .filter_map(|index| match &self[index].kind {
                ExprKind::Function { params, .. } => Some((index, params)),
                _ => None,
            })
            .flat_map(|(func_index, params)| {
                params.iter().map(move |param| (func_index, *param))
            })
            .filter_map(|(func_index, param_value)| match param_value {
                SymbolicValue::Result(param_index) => {
                    Some((func_index, param_index))
                }
                _ => None,
            });

        let mut param_to_owner: HashMap<OpIndex, OpIndex> = HashMap::new();
        for (func_index, param_index) in iter_functions {
            if let Some(prev_func) = param_to_owner.get(&param_index) {
                return Err(Error::MultipleFunctionsOwnSameParam {
                    param: param_index,
                    first_owner: *prev_func,
                    second_owner: func_index,
                });
            } else {
                param_to_owner.insert(param_index, func_index);
            }
        }

        Ok(())
    }

    fn validate_all_parameters_defined(&self) -> Result<(), Error> {
        let reachable = self.reachable(self.iter_extern_funcs());

        let defined_params: HashSet<OpIndex> = reachable
            .iter()
            .cloned()
            .enumerate()
            .filter(|(_, reachable)| *reachable)
            .map(|(i, _)| OpIndex::new(i))
            .filter_map(|index| match &self[index].kind {
                ExprKind::Function { params, .. } => Some(params),
                _ => None,
            })
            .flatten()
            .filter_map(|param| param.as_op_index())
            .collect();

        reachable
            .iter()
            .cloned()
            .enumerate()
            .filter(|(_, reachable)| *reachable)
            .map(|(i, _)| OpIndex::new(i))
            .filter(|index| {
                matches!(self[*index].kind, ExprKind::FunctionArg(_))
            })
            .try_for_each(|param_index| {
                if defined_params.contains(&param_index) {
                    Ok(())
                } else {
                    Err(Error::UseOfUndefinedParam { param: param_index })
                }
            })
    }

    fn validate(&self, reader: Option<CachedReader<'_>>) -> Result<(), Error> {
        self.validate_types(reader)?;
        self.validate_only_back_references()?;
        self.validate_unique_parameter_owner_among_reachable_functions()?;
        self.validate_all_parameters_defined()?;

        Ok(())
    }
}

pub trait SymbolicGraphSimplify: Sized {
    fn simplify<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<Self, Error>;
}
impl SymbolicGraphSimplify for SymbolicGraph {
    fn simplify<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<Self, Error> {
        let analysis = Analysis::new(reader);
        let rewriter = super::RemoveUnusedDowncast(&analysis)
            .then(super::ConstantFold)
            .then(super::RemoveUnusedPrimcast(&analysis))
            .apply_recursively();
        self.rewrite(rewriter)
    }
}

pub trait SymbolicGraphRewrite: Sized {
    fn rewrite(&self, rewriter: impl GraphRewrite) -> Result<Self, Error>;
}
trait SymbolicGraphRewriteVerbose {
    fn remap_extern_funcs(
        &self,
        builder: &mut SymbolicGraph,
        lookup: &HashMap<OpIndex, SymbolicValue>,
    ) -> Result<(), Error>;

    fn rewrite_verbose(
        &self,
        rewriter: impl GraphRewrite,
    ) -> Result<RewriteResults, Error>;
}
impl SymbolicGraphRewriteVerbose for SymbolicGraph {
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

    fn rewrite_verbose(
        &self,
        rewriter: impl GraphRewrite,
    ) -> Result<RewriteResults, Error> {
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
impl SymbolicGraphRewrite for SymbolicGraph {
    fn rewrite(&self, rewriter: impl GraphRewrite) -> Result<Self, Error> {
        Ok(self.rewrite_verbose(rewriter)?.graph)
    }
}

pub(crate) trait SymbolicGraphSubstitute {
    fn rewrite_subtree(
        &mut self,
        rewriter: impl GraphRewrite,
        indices: impl Iterator<Item = OpIndex>,
        rewrites_applied: &mut HashMap<OpIndex, SymbolicValue>,
    ) -> Result<(), Error>;

    fn substitute(
        &mut self,
        replacements: HashMap<OpIndex, SymbolicValue>,
        value: SymbolicValue,
    ) -> Result<Option<SymbolicValue>, Error>;
}
impl SymbolicGraphSubstitute for SymbolicGraph {
    fn rewrite_subtree(
        &mut self,
        rewriter: impl GraphRewrite,
        indices: impl Iterator<Item = OpIndex>,
        rewrites_applied: &mut HashMap<OpIndex, SymbolicValue>,
    ) -> Result<(), Error> {
        rewriter.init();

        for old_index in indices {
            let op = self[old_index].clone();
            let opt_remapped = op.kind.try_remap(rewrites_applied);
            let kind = opt_remapped.as_ref().unwrap_or(&op.kind);

            let opt_new_value = if let Some(rewritten) =
                rewriter.rewrite_expr(self, kind, op.name.as_deref())?
            {
                if let Some(name) = &op.name {
                    if let SymbolicValue::Result(new_index) = rewritten {
                        if self[new_index].name.is_none() {
                            self.name(rewritten, name)?;
                        }
                    }
                }
                Some(rewritten)
            } else if let Some(remapped) = opt_remapped {
                let expr = Expr {
                    kind: remapped,
                    name: op.name,
                };

                Some(self.push(expr))
            } else {
                None
            };

            if let Some(new_value) = opt_new_value {
                rewrites_applied.insert(old_index, new_value);
            }
        }

        Ok(())
    }

    fn substitute(
        &mut self,
        mut replacements: HashMap<OpIndex, SymbolicValue>,
        value: SymbolicValue,
    ) -> Result<Option<SymbolicValue>, Error> {
        let Some(index) = value.as_op_index() else {
            return Ok(None);
        };
        if replacements.is_empty() {
            return Ok(None);
        }

        let subgraph = loop {
            let subgraph = self.collect_subgraph(
                replacements.keys().map(|key| (*key).into()),
                Some(value),
            );

            let mut must_also_replace_function = false;

            subgraph
                .iter()
                .cloned()
                .filter(|index| !replacements.contains_key(index))
                .filter_map(|index| match &self[index].kind {
                    ExprKind::Function { params, .. } => Some(params),
                    _ => None,
                })
                .flatten()
                .filter_map(|value| value.as_op_index())
                .filter(|index| !replacements.contains_key(index))
                .collect::<Vec<_>>()
                .into_iter()
                .for_each(|index| {
                    let ExprKind::FunctionArg(arg_ty) = &self[index].kind
                    else {
                        unreachable!(
                            "Ill-formed SymbolicGraph, \
                             function params must point to FunctionArg."
                        )
                    };
                    let new_arg = self.function_arg(arg_ty.clone());
                    if let Some(name) = self[index].name.clone() {
                        self.name(new_arg, name)
                            .expect("Existing name must be valid");
                    }

                    must_also_replace_function = true;
                    replacements.insert(index, new_arg);
                });

            if !must_also_replace_function {
                break subgraph;
            }
        };

        let to_rewrite = subgraph
            .into_iter()
            .filter(|index| !replacements.contains_key(index));

        let mut rewrites = HashMap::new();

        self.rewrite_subtree(
            Substitute(&replacements),
            to_rewrite,
            &mut rewrites,
        )?;

        return Ok(rewrites.get(&index).cloned());

        struct Substitute<'a>(&'a HashMap<OpIndex, SymbolicValue>);

        impl GraphRewrite for Substitute<'_> {
            fn rewrite_expr(
                &self,
                graph: &mut SymbolicGraph,
                expr: &ExprKind,
                _name: Option<&str>,
            ) -> Result<Option<SymbolicValue>, Error> {
                Ok(expr.try_remap(self.0).map(|remapped| graph.push(remapped)))
            }
        }
    }
}

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

pub trait SymbolicGraphCSE: Sized {
    fn eliminate_common_subexpressions(self) -> Result<Self, Error>;
}
impl SymbolicGraphCSE for SymbolicGraph {
    fn eliminate_common_subexpressions(self) -> Result<Self, Error> {
        let mut builder = Self::new();

        let mut prev_index_lookup: HashMap<OpIndex, SymbolicValue> =
            HashMap::new();
        let mut dedup_lookup: HashMap<ExprKind, SymbolicValue> = HashMap::new();
        let mut functions_returning_rust_native: HashSet<SymbolicValue> =
            HashSet::new();

        for (prev_index, op) in self.iter_ops() {
            let new_kind = op
                .kind
                .try_remap(&prev_index_lookup)
                .unwrap_or_else(|| op.kind.clone());

            let new_index = if let Some(new_index) = dedup_lookup.get(&new_kind)
            {
                *new_index
            } else {
                let new_index = builder.push(new_kind.clone());
                if let Some(name) = &op.name {
                    builder.name(new_index, name)?;
                }

                if let ExprKind::NativeFunction(func) = &new_kind {
                    // Track fucntions
                    let DSLType::Function(FunctionType { output, .. }) =
                        func.signature()?
                    else {
                        panic!(
                            "Internal error, \
                                    NativeFunction should have function type."
                        )
                    };

                    if matches!(
                        output.as_ref(),
                        DSLType::Unknown | DSLType::Rust(_)
                    ) {
                        functions_returning_rust_native.insert(new_index);
                    }
                }

                let can_dedup = match &new_kind {
                    ExprKind::FunctionArg(_) => {
                        // Temporary workaround.  The long-term fix is
                        // to make update the hashing so that
                        // FunctionArg have structural equality when
                        // encountering points of definition, but
                        // reference equality when encountering points
                        // of use (that haven't already been defined,
                        // that is).
                        false
                    }
                    ExprKind::FunctionCall { func, .. } => {
                        !functions_returning_rust_native.contains(func)
                    }

                    _ => true,
                };

                if can_dedup {
                    dedup_lookup.insert(new_kind, new_index);
                }

                new_index
            };
            prev_index_lookup.insert(prev_index, new_index);
        }

        self.remap_extern_funcs(&mut builder, &prev_index_lookup)?;

        Ok(builder)
    }
}

pub trait SymbolicGraphCompileExt {
    fn compiler<'a, 'b>(&'a self) -> SymbolicGraphCompiler<'a, 'b>;

    fn compile<'a>(
        &self,
        reader: impl Into<Option<CachedReader<'a>>>,
    ) -> Result<VirtualMachine, Error>;
}
impl SymbolicGraphCompileExt for SymbolicGraph {
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

impl<'a, 'b> SymbolicGraphCompiler<'a, 'b> {
    pub(crate) fn from_graph(graph: &'a SymbolicGraph) -> Self {
        let show_steps = env_var_flag("SHOW_STEPS");
        let interactive_substeps = env_var_flag("INTERACTIVE_SUBSTEPS");

        Self {
            graph,
            show_steps,
            interactive_substeps,
            reader: None,
            optimize_symbolic_graph: true,
        }
    }

    pub fn show_each_step(self, show_steps: bool) -> Self {
        Self { show_steps, ..self }
    }

    pub fn interactive_substeps(self, interactive_substeps: bool) -> Self {
        Self {
            interactive_substeps,
            ..self
        }
    }

    pub fn with_reader(
        self,
        reader: impl Into<Option<CachedReader<'b>>>,
    ) -> Self {
        Self {
            reader: reader.into(),
            ..self
        }
    }

    pub fn disable_optimizations(self) -> Self {
        Self {
            optimize_symbolic_graph: false,
            ..self
        }
    }

    fn apply_rewrites(
        &self,
        mut expr: SymbolicGraph,
        rewriter: impl GraphRewrite,
    ) -> Result<SymbolicGraph, Error> {
        if self.interactive_substeps {
            let rewriter = rewriter.apply_once();

            println!("--------- Initial ------------\n{expr}");

            for i_update in 1.. {
                std::io::stdin()
                    .read_line(&mut String::new())
                    .expect("Error reading stdin");

                let rewrite_details = expr.rewrite_verbose(&rewriter)?;

                if rewrite_details.num_rewritten_terms == 0 {
                    assert!(rewrite_details.num_terms_with_new_inputs == 0);
                    break;
                } else {
                    expr = rewrite_details.graph;
                }

                println!(
                    "--------- After {i_update} Updates ------------\n{}",
                    expr.printer().expand_all_expressions(),
                );

                expr.validate(None)?;
            }

            Ok(expr)
        } else {
            expr.rewrite(rewriter.apply_recursively())
        }
    }

    pub fn compile(self) -> Result<VirtualMachine, Error> {
        let reader = self.reader;
        let expr = self.graph;

        if self.show_steps {
            println!("----------- Initial Graph --------------\n{expr}");
        }
        expr.validate_only_back_references()?;
        expr.validate_unique_parameter_owner_among_reachable_functions()?;
        expr.validate_all_parameters_defined()?;

        // The display of static fields is done similar to C#, which
        // uses the same syntax for specifying static fields, and for
        // specifying namespaces.
        //
        // For example, `A.B.C.D` could be class `B` within namespace
        // `A`, which has a static field `C`, and a subfield `D`.
        // Alternatively, it could be class `C` within namespace
        // `A.B`, which has a static field `C`.
        //
        // Within the CLR, this ambiguity is handled by explicitly
        // specifying both the name and the namespace as separate
        // fields, but the printed-out format (and C# itself) is
        // ambiguous without additional information.  Here, it is
        // handled while lowering, by identifying how many elements
        // are required to produce some "namespace.name" of a valid
        // class.
        let expr = expr.rewrite(
            super::IdentifyStaticField(&Analysis::new(reader))
                .apply_recursively(),
        )?;
        expr.validate_only_back_references()?;
        expr.validate_unique_parameter_owner_among_reachable_functions()?;
        expr.validate_all_parameters_defined()?;

        if self.show_steps {
            println!(
                "----------- After IdentifyStaticField --------------\n{expr}"
            );
        }

        let expr = expr.dead_code_elimination()?;
        expr.validate(reader)?;

        if self.show_steps {
            println!("----------- After DCE --------------\n{expr}");
        }
        let expr = expr.eliminate_common_subexpressions()?;
        expr.validate(reader)?;
        if self.show_steps {
            println!("----------- After CSE --------------\n{expr}");
        }

        let analysis = Analysis::new(reader);

        let optional_optimizations = super::ConstantFold
            .then(super::RemoveUnusedDowncast(&analysis))
            .then(super::RemoveUnusedPrimcast(&analysis))
            .then(super::RemoveUnusedPointerCast)
            // Currently, the MergeParallelReads pass works, but
            // doesn't seem to provide a performance benefit.
            //
            // .then(super::MergeParallelReads)
            ;

        let mandatory_lowering = super::InferFunctionParameterTypes(&analysis)
            .then(super::LegalizeOperandTypes(&analysis))
            .then(super::InlineFunctionCalls)
            .then(super::MergeRangeReduceToSimpleReduce)
            .then(super::InlineIteratorMap)
            .then(super::InlineIteratorFilter)
            .then(super::SplitIteratorChainReduce)
            .then(super::ConvertCollectToReduce(&analysis))
            .then(super::ConvertFindToFilterFirst)
            .then(super::ConvertFirstToReduce)
            .then(super::ConvertFindMapToFilterFind)
            .then(super::ConvertBooleanOperatorToConditional)
            .then(super::LowerSymbolicExpr(&analysis))
            .then(super::SeparateReadAndParseBytes);

        let expr = if self.optimize_symbolic_graph {
            self.apply_rewrites(
                expr,
                optional_optimizations.then(mandatory_lowering),
            )?
        } else {
            self.apply_rewrites(expr, mandatory_lowering)?
        };

        expr.validate(reader)?;

        if self.show_steps {
            println!("----------- After Simplifcations --------------\n{expr}");
        }

        let expr = expr.dead_code_elimination()?;
        expr.validate(reader)?;

        if self.show_steps {
            println!("----------- After DCE --------------\n{expr}");
        }

        let expr = expr.eliminate_common_subexpressions()?;
        expr.validate(reader)?;

        if self.show_steps {
            println!("----------- After CSE --------------\n{expr}");
            println!(
                "----------- Before VM Conversion --------------\n{}",
                expr.printer()
                    .expand_all_expressions()
                    .number_all_expressions()
            );
        }

        // Virtual machine, in terms of sequential operations.
        let vm = expr.to_virtual_machine(self.show_steps)?;

        if self.show_steps {
            println!("----------- As VM --------------\n{vm}");
        }

        Ok(vm)
    }
}
