use std::collections::{HashMap, HashSet};

use itertools::Itertools as _;

use env_var_flag::env_var_flag;
use iterator_extensions::ResultIteratorExt as _;

use crate::{
    expr_to_virtual_machine::SymbolicGraphToVirtualMachine as _, Error,
    VirtualMachine,
};
use dotnet_debugger::{
    CachedReader, FieldDescription, MethodTable, TypeHandlePtrExt as _,
};
use dsl_ir::{
    DSLType, Expr, ExprKind, FunctionType, OpIndex, StaticField, SymbolicGraph,
    SymbolicType, SymbolicValue, TypedPointer,
};

use super::{Analysis, GraphRewrite, TypeInference};

struct RewriteResults {
    /// The resulting graph
    graph: SymbolicGraph,

    /// The number of terms rewritten by the GraphRewrite rule.
    num_rewritten_terms: usize,

    /// The number of terms which have been updated to reference a
    /// rewritten term, directly or indirectly.
    num_terms_with_new_inputs: usize,
}

pub struct GraphComparison<'a> {
    lhs: &'a SymbolicGraph,
    rhs: &'a SymbolicGraph,
    order_dependent: bool,
    compare_names: bool,
}

pub struct SymbolicGraphCompiler<'a, 'b> {
    graph: &'a SymbolicGraph,
    show_steps: bool,
    interactive_substeps: bool,
    reader: Option<CachedReader<'b>>,
    optimize_symbolic_graph: bool,
}

pub(crate) trait SymbolicTypeExt {
    fn method_table<'a>(
        &self,
        reader: impl Into<CachedReader<'a>>,
    ) -> Result<TypedPointer<MethodTable>, Error>;
}
impl SymbolicTypeExt for SymbolicType {
    fn method_table<'a>(
        &self,
        reader: impl Into<CachedReader<'a>>,
    ) -> Result<TypedPointer<MethodTable>, Error> {
        let reader = reader.into();

        let method_table_ptr = reader
            .method_table_by_name(&self.full_name)?
            .ok_or_else(|| {
                Error::UnexpectedNullMethodTable(format!("{self}"))
            })?;

        if self.generics.is_empty() {
            return Ok(method_table_ptr);
        }

        let method_table = reader.method_table(method_table_ptr)?;

        let expected_type_def = method_table.token();

        // TODO: Move this to a cache inside the StaticValueCache.
        let generics = self
            .generics
            .iter()
            .map(|generic| generic.method_table(reader))
            .collect::<Result<Vec<_>, _>>()?;

        let instantiated_generic = [method_table_ptr, generics[0]]
            .into_iter()
            .map(|method_table_ptr| reader.method_table(method_table_ptr))
            .map_ok(|method_table| method_table.module())
            .and_map_ok(|module_ptr| reader.runtime_module(module_ptr))
            .and_map_ok(|module| module.loaded_types(reader))
            .flatten_ok()
            .and_flat_map_ok(|instantiated_generics| {
                instantiated_generics.iter_method_tables(&reader)
            })
            .filter_map_ok(|type_handle_ptr| type_handle_ptr.as_method_table())
            .and_map_ok(|method_table_ptr| {
                reader.method_table(method_table_ptr)
            })
            .filter_ok(|method_table| method_table.token() == expected_type_def)
            .filter_ok(|method_table| method_table.has_generics())
            .and_filter_ok(|candidate_method_table| {
                let mut iter_candidate_generics = candidate_method_table
                    .generic_types_excluding_base_class(&reader)?;
                for generic in generics.iter().cloned() {
                    let Some(candidate_generic) =
                        iter_candidate_generics.next()
                    else {
                        return Ok(false);
                    };

                    let Some(candidate_generic) =
                        candidate_generic.as_method_table()
                    else {
                        return Ok(false);
                    };

                    if candidate_generic != generic {
                        return Ok(false);
                    }
                }

                if iter_candidate_generics.next().is_some() {
                    return Ok(false);
                }

                Ok(true)
            })
            .next()
            .ok_or_else(|| {
                Error::NoSuchMethodTableFound(format!("{self}"))
            })??;

        Ok(instantiated_generic.ptr())
    }
}

pub(crate) trait StaticFieldExt {
    fn method_table_and_field<'a>(
        &self,
        reader: CachedReader<'a>,
    ) -> Result<(TypedPointer<MethodTable>, FieldDescription<'a>), Error>;
    fn runtime_type(&self, reader: CachedReader<'_>) -> Result<DSLType, Error>;
}
impl StaticFieldExt for StaticField {
    fn method_table_and_field<'a>(
        &self,
        reader: CachedReader<'a>,
    ) -> Result<(TypedPointer<MethodTable>, FieldDescription<'a>), Error> {
        let base_method_table_ptr = self.class.method_table(reader)?;

        let field = reader
            .iter_static_fields(base_method_table_ptr)?
            .find(|field| {
                reader
                    .field_to_name(field)
                    .map(|name| name == self.field_name)
                    .unwrap_or(false)
            })
            .ok_or_else(|| Error::NoSuchStaticField {
                class: format!("{}", self.class),
                field: self.field_name.clone(),
            })?;

        Ok((base_method_table_ptr, field))
    }

    fn runtime_type(&self, reader: CachedReader<'_>) -> Result<DSLType, Error> {
        let (base_method_table_ptr, field) =
            self.method_table_and_field(reader)?;

        let base_type =
            reader.field_to_runtime_type(base_method_table_ptr, &field)?;

        Ok(base_type.into())
    }
}

pub trait GraphComparisonExt {
    fn graph_comparison<'a>(&'a self, rhs: &'a Self) -> GraphComparison<'a>;
}
impl GraphComparisonExt for SymbolicGraph {
    fn graph_comparison<'a>(&'a self, rhs: &'a Self) -> GraphComparison<'a> {
        GraphComparison {
            lhs: self,
            rhs,
            order_dependent: false,
            compare_names: false,
        }
    }
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

impl<'a> GraphComparison<'a> {
    pub fn order_dependent(self, order_dependent: bool) -> Self {
        Self {
            order_dependent,
            ..self
        }
    }

    pub fn compare_names(self, compare_names: bool) -> Self {
        Self {
            compare_names,
            ..self
        }
    }

    pub fn apply(&self) -> bool {
        if self.order_dependent {
            self.order_dependent_comparison()
        } else {
            self.order_independent_comparison()
        }
    }

    fn order_dependent_comparison(&self) -> bool {
        let lhs = self.lhs;
        let rhs = self.rhs;
        lhs.num_operations() == rhs.num_operations()
            && lhs.num_extern_funcs() == rhs.num_extern_funcs()
            && lhs.iter_ops().zip(rhs.iter_ops()).all(|((_, a), (_, b))| {
                a.kind == b.kind && (!self.compare_names || a.name == b.name)
            })
            && lhs
                .iter_extern_funcs()
                .zip(rhs.iter_extern_funcs())
                .all(|(a, b)| a == b)
    }

    fn order_independent_comparison(&self) -> bool {
        let lhs = self.lhs;
        let rhs = self.rhs;

        if lhs.num_extern_funcs() != rhs.num_extern_funcs() {
            return false;
        }

        let mut lhs_index_to_rhs = HashMap::<OpIndex, OpIndex>::new();
        let mut rhs_index_to_lhs = HashMap::<OpIndex, OpIndex>::new();

        let mut to_visit = Vec::<(OpIndex, OpIndex)>::new();

        macro_rules! equivalent_value {
            ($lhs:expr,$rhs:expr) => {{
                let lhs: &SymbolicValue = $lhs;
                let rhs: &SymbolicValue = $rhs;

                match (*lhs, *rhs) {
                    (
                        SymbolicValue::Result(lhs_index),
                        SymbolicValue::Result(rhs_index),
                    ) => {
                        if let Some(prev_rhs) = lhs_index_to_rhs.get(&lhs_index)
                        {
                            rhs_index == *prev_rhs
                        } else if let Some(prev_lhs) =
                            rhs_index_to_lhs.get(&rhs_index)
                        {
                            lhs_index == *prev_lhs
                        } else {
                            to_visit.push((lhs_index, rhs_index));
                            lhs_index_to_rhs.insert(lhs_index, rhs_index);
                            rhs_index_to_lhs.insert(rhs_index, lhs_index);
                            true
                        }
                    }

                    (other_lhs, other_rhs) => other_lhs == other_rhs,
                }
            }};
        }

        for (lhs_extern_func, rhs_extern_func) in
            lhs.iter_extern_funcs().zip(rhs.iter_extern_funcs())
        {
            let lhs_extern_func: SymbolicValue = lhs_extern_func.into();
            let rhs_extern_func: SymbolicValue = rhs_extern_func.into();
            if !equivalent_value!(&lhs_extern_func, &rhs_extern_func) {
                return false;
            }
        }

        while let Some((lhs_index, rhs_index)) = to_visit.pop() {
            if self.compare_names && lhs[lhs_index].name != rhs[rhs_index].name
            {
                return false;
            }

            let lhs_kind = &lhs[lhs_index].kind;
            let rhs_kind = &rhs[rhs_index].kind;

            macro_rules! handle_binary_op {
                ($variant:ident, $lhs:expr, $rhs:expr) => {{
                    let lhs_lhs = $lhs;
                    let lhs_rhs = $rhs;
                    match rhs_kind {
                        ExprKind::$variant {
                            lhs: rhs_lhs,
                            rhs: rhs_rhs,
                        } => {
                            equivalent_value!(lhs_lhs, rhs_lhs)
                                && equivalent_value!(lhs_rhs, rhs_rhs)
                        }
                        _ => false,
                    }
                }};
            }

            let is_match = match lhs_kind {
                ExprKind::None => matches!(rhs_kind, ExprKind::None),

                ExprKind::Function {
                    params: lhs_params,
                    output: lhs_output,
                } => match rhs_kind {
                    ExprKind::Function {
                        params: rhs_params,
                        output: rhs_output,
                    } => {
                        lhs_params.len() == rhs_params.len()
                            && lhs_params.iter().zip(rhs_params).all(
                                |(lhs_param, rhs_param)| {
                                    equivalent_value!(lhs_param, rhs_param)
                                },
                            )
                            && equivalent_value!(lhs_output, rhs_output)
                    }
                    _ => false,
                },
                ExprKind::FunctionArg(lhs_ty) => match rhs_kind {
                    ExprKind::FunctionArg(rhs_ty) => lhs_ty == rhs_ty,
                    _ => false,
                },
                ExprKind::FunctionCall {
                    func: lhs_func,
                    args: lhs_args,
                } => match rhs_kind {
                    ExprKind::FunctionCall {
                        func: rhs_func,
                        args: rhs_args,
                    } => {
                        equivalent_value!(lhs_func, rhs_func)
                            && lhs_args.len() == rhs_args.len()
                            && lhs_args.iter().zip(rhs_args).all(
                                |(lhs_arg, rhs_arg)| {
                                    equivalent_value!(lhs_arg, rhs_arg)
                                },
                            )
                    }
                    _ => false,
                },
                ExprKind::Range { extent: lhs_extent } => match rhs_kind {
                    ExprKind::Range { extent: rhs_extent } => {
                        equivalent_value!(lhs_extent, rhs_extent)
                    }
                    _ => false,
                },
                ExprKind::Map {
                    iterator: lhs_iterator,
                    map: lhs_map,
                } => match rhs_kind {
                    ExprKind::Map {
                        iterator: rhs_iterator,
                        map: rhs_map,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_map, rhs_map)
                    }
                    _ => false,
                },
                ExprKind::Filter {
                    iterator: lhs_iterator,
                    filter: lhs_filter,
                } => match rhs_kind {
                    ExprKind::Filter {
                        iterator: rhs_iterator,
                        filter: rhs_filter,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_filter, rhs_filter)
                    }
                    _ => false,
                },
                ExprKind::Find {
                    iterator: lhs_iterator,
                    condition: rhs_condition,
                } => match rhs_kind {
                    ExprKind::Find {
                        iterator: rhs_iterator,
                        condition: lhs_condition,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_condition, rhs_condition)
                    }
                    _ => false,
                },
                ExprKind::FindMap {
                    iterator: lhs_iterator,
                    condition: lhs_condition,
                } => match rhs_kind {
                    ExprKind::FindMap {
                        iterator: rhs_iterator,
                        condition: rhs_condition,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_condition, rhs_condition)
                    }
                    _ => false,
                },
                ExprKind::Chain(lhs_iter_a, lhs_iter_b) => match rhs_kind {
                    ExprKind::Chain(rhs_iter_a, rhs_iter_b) => {
                        equivalent_value!(lhs_iter_a, rhs_iter_a)
                            && equivalent_value!(lhs_iter_b, rhs_iter_b)
                    }
                    _ => false,
                },

                ExprKind::First {
                    iterator: lhs_iterator,
                } => match rhs_kind {
                    ExprKind::First {
                        iterator: rhs_iterator,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                    }
                    _ => false,
                },

                ExprKind::Collect {
                    iterator: lhs_iterator,
                } => match rhs_kind {
                    ExprKind::Collect {
                        iterator: rhs_iterator,
                    } => {
                        equivalent_value!(lhs_iterator, rhs_iterator)
                    }
                    _ => false,
                },

                ExprKind::Reduce {
                    initial: lhs_initial,
                    iterator: lhs_iterator,
                    reduction: lhs_reduction,
                } => match rhs_kind {
                    ExprKind::Reduce {
                        initial: rhs_initial,
                        iterator: rhs_iterator,
                        reduction: rhs_reduction,
                    } => {
                        equivalent_value!(lhs_initial, rhs_initial)
                            && equivalent_value!(lhs_iterator, rhs_iterator)
                            && equivalent_value!(lhs_reduction, rhs_reduction)
                    }
                    _ => false,
                },
                ExprKind::SimpleReduce {
                    initial: lhs_initial,
                    extent: lhs_extent,
                    reduction: lhs_reduction,
                } => match rhs_kind {
                    ExprKind::SimpleReduce {
                        initial: rhs_initial,
                        extent: rhs_extent,
                        reduction: rhs_reduction,
                    } => {
                        equivalent_value!(lhs_initial, rhs_initial)
                            && equivalent_value!(lhs_extent, rhs_extent)
                            && equivalent_value!(lhs_reduction, rhs_reduction)
                    }
                    _ => false,
                },
                ExprKind::NativeFunction(lhs_func) => match rhs_kind {
                    ExprKind::NativeFunction(rhs_func) => lhs_func == rhs_func,
                    _ => false,
                },
                ExprKind::Tuple(lhs_tuple) => match rhs_kind {
                    ExprKind::Tuple(rhs_tuple) => {
                        lhs_tuple.len() == rhs_tuple.len()
                            && lhs_tuple.iter().zip(rhs_tuple).all(
                                |(lhs_element, rhs_element)| {
                                    equivalent_value!(lhs_element, rhs_element)
                                },
                            )
                    }
                    _ => false,
                },
                ExprKind::StaticField(StaticField {
                    class: lhs_class,
                    field_name: lhs_field,
                }) => match rhs_kind {
                    ExprKind::StaticField(StaticField {
                        class: rhs_class,
                        field_name: rhs_field,
                    }) => lhs_class == rhs_class && lhs_field == rhs_field,
                    _ => false,
                },
                ExprKind::FieldAccess {
                    obj: lhs_obj,
                    field: lhs_field,
                } => match rhs_kind {
                    ExprKind::FieldAccess {
                        obj: rhs_obj,
                        field: rhs_field,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj)
                            && lhs_field == rhs_field
                    }
                    _ => false,
                },
                ExprKind::SymbolicDowncast {
                    obj: lhs_obj,
                    ty: lhs_ty,
                } => match rhs_kind {
                    ExprKind::SymbolicDowncast {
                        obj: rhs_obj,
                        ty: rhs_ty,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj) && lhs_ty == rhs_ty
                    }
                    _ => false,
                },
                ExprKind::IndexAccess {
                    obj: lhs_obj,
                    indices: lhs_indices,
                } => match rhs_kind {
                    ExprKind::IndexAccess {
                        obj: rhs_obj,
                        indices: rhs_indices,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj)
                            && lhs_indices.len() == rhs_indices.len()
                            && lhs_indices.iter().zip(rhs_indices.iter()).all(
                                |(lhs_index, rhs_index)| {
                                    equivalent_value!(lhs_index, rhs_index)
                                },
                            )
                    }
                    _ => false,
                },
                ExprKind::NumArrayElements { array: lhs_array } => {
                    match rhs_kind {
                        ExprKind::NumArrayElements { array: rhs_array } => {
                            equivalent_value!(lhs_array, rhs_array)
                        }
                        _ => false,
                    }
                }
                ExprKind::ArrayExtent {
                    array: lhs_array,
                    dim: lhs_dim,
                } => match rhs_kind {
                    ExprKind::ArrayExtent {
                        array: rhs_array,
                        dim: rhs_dim,
                    } => {
                        equivalent_value!(lhs_array, rhs_array)
                            && equivalent_value!(lhs_dim, rhs_dim)
                    }
                    _ => false,
                },
                ExprKind::PointerCast {
                    ptr: lhs_ptr,
                    ty: lhs_ty,
                } => match rhs_kind {
                    ExprKind::PointerCast {
                        ptr: rhs_ptr,
                        ty: rhs_ty,
                    } => {
                        equivalent_value!(lhs_ptr, rhs_ptr) && lhs_ty == rhs_ty
                    }
                    _ => false,
                },
                ExprKind::IsSome(lhs_value) => match rhs_kind {
                    ExprKind::IsSome(rhs_value) => {
                        equivalent_value!(lhs_value, rhs_value)
                    }
                    _ => false,
                },
                ExprKind::IfElse {
                    condition: lhs_condition,
                    if_branch: lhs_if_branch,
                    else_branch: lhs_else_branch,
                } => match rhs_kind {
                    ExprKind::IfElse {
                        condition: rhs_condition,
                        if_branch: rhs_if_branch,
                        else_branch: rhs_else_branch,
                    } => {
                        equivalent_value!(lhs_condition, rhs_condition)
                            && equivalent_value!(lhs_if_branch, rhs_if_branch)
                            && equivalent_value!(
                                lhs_else_branch,
                                rhs_else_branch
                            )
                    }
                    _ => false,
                },

                ExprKind::And { lhs, rhs } => {
                    handle_binary_op!(And, lhs, rhs)
                }
                ExprKind::Or { lhs, rhs } => {
                    handle_binary_op!(Or, lhs, rhs)
                }
                ExprKind::Not { arg: lhs_arg } => match rhs_kind {
                    ExprKind::Not { arg: rhs_arg } => {
                        equivalent_value!(lhs_arg, rhs_arg)
                    }
                    _ => false,
                },

                ExprKind::Equal { lhs, rhs } => {
                    handle_binary_op!(Equal, lhs, rhs)
                }
                ExprKind::NotEqual { lhs, rhs } => {
                    handle_binary_op!(NotEqual, lhs, rhs)
                }
                ExprKind::GreaterThan { lhs, rhs } => {
                    handle_binary_op!(GreaterThan, lhs, rhs)
                }
                ExprKind::LessThan { lhs, rhs } => {
                    handle_binary_op!(LessThan, lhs, rhs)
                }
                ExprKind::GreaterThanOrEqual { lhs, rhs } => {
                    handle_binary_op!(GreaterThanOrEqual, lhs, rhs)
                }
                ExprKind::LessThanOrEqual { lhs, rhs } => {
                    handle_binary_op!(LessThanOrEqual, lhs, rhs)
                }

                ExprKind::Add { lhs, rhs } => handle_binary_op!(Add, lhs, rhs),
                ExprKind::Sub { lhs, rhs } => handle_binary_op!(Sub, lhs, rhs),
                ExprKind::Mul { lhs, rhs } => handle_binary_op!(Mul, lhs, rhs),
                ExprKind::Div { lhs, rhs } => handle_binary_op!(Div, lhs, rhs),
                ExprKind::Mod { lhs, rhs } => handle_binary_op!(Mod, lhs, rhs),

                ExprKind::PrimCast {
                    value: lhs_value,
                    prim_type: lhs_prim_type,
                } => match rhs_kind {
                    ExprKind::PrimCast {
                        value: rhs_value,
                        prim_type: rhs_prim_type,
                    } => {
                        equivalent_value!(lhs_value, rhs_value)
                            && lhs_prim_type == rhs_prim_type
                    }
                    _ => false,
                },
                ExprKind::PhysicalDowncast {
                    obj: lhs_obj,
                    ty: lhs_ty,
                } => match rhs_kind {
                    ExprKind::PhysicalDowncast {
                        obj: rhs_obj,
                        ty: rhs_ty,
                    } => {
                        equivalent_value!(lhs_obj, rhs_obj) && lhs_ty == rhs_ty
                    }
                    _ => false,
                },
                ExprKind::IsSubclassOf {
                    method_table_ptr: lhs_method_table_ptr,
                    ty: lhs_ty,
                } => match rhs_kind {
                    ExprKind::IsSubclassOf {
                        method_table_ptr: rhs_method_table_ptr,
                        ty: rhs_ty,
                    } => {
                        equivalent_value!(
                            lhs_method_table_ptr,
                            rhs_method_table_ptr
                        ) && lhs_ty == rhs_ty
                    }
                    _ => false,
                },
                ExprKind::ReadPrim {
                    ptr: lhs_ptr,
                    prim_type: lhs_prim_type,
                } => match rhs_kind {
                    ExprKind::ReadPrim {
                        ptr: rhs_ptr,
                        prim_type: rhs_prim_type,
                    } => {
                        equivalent_value!(lhs_ptr, rhs_ptr)
                            && lhs_prim_type == rhs_prim_type
                    }
                    _ => false,
                },
                ExprKind::ReadBytes(lhs_regions) => match rhs_kind {
                    ExprKind::ReadBytes(rhs_regions) => {
                        lhs_regions.len() == rhs_regions.len()
                            && lhs_regions.iter().zip(rhs_regions.iter()).all(
                                |(lhs_region, rhs_region)| {
                                    equivalent_value!(
                                        &lhs_region.ptr,
                                        &rhs_region.ptr
                                    ) && equivalent_value!(
                                        &lhs_region.num_bytes,
                                        &rhs_region.num_bytes
                                    )
                                },
                            )
                    }
                    _ => false,
                },
                ExprKind::CastBytes {
                    bytes: lhs_bytes,
                    offset: lhs_offset,
                    prim_type: lhs_prim_type,
                } => match rhs_kind {
                    ExprKind::CastBytes {
                        bytes: rhs_bytes,
                        offset: rhs_offset,
                        prim_type: rhs_prim_type,
                    } => {
                        equivalent_value!(lhs_bytes, rhs_bytes)
                            && equivalent_value!(lhs_offset, rhs_offset)
                            && lhs_prim_type == rhs_prim_type
                    }
                    _ => false,
                },
                ExprKind::ReadString { ptr: lhs_ptr } => match rhs_kind {
                    ExprKind::ReadString { ptr: rhs_ptr } => {
                        equivalent_value!(lhs_ptr, rhs_ptr)
                    }
                    _ => false,
                },
            };

            if !is_match {
                return false;
            }
        }

        true
    }
}
