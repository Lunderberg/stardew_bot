use dsl_lowering::lowering_passes;
use dsl_optimize::optimization_passes;
use dsl_validation::SymbolicGraphValidation as _;
use env_var_flag::env_var_flag;

use dotnet_debugger::CachedReader;

use dsl_analysis::Analysis;
use dsl_ir::SymbolicGraph;
use dsl_rewrite_utils::{GraphRewrite, SymbolicGraphRewrite as _};
use dsl_vm::VirtualMachine;

use crate::{
    expr_to_virtual_machine::SymbolicGraphToVirtualMachine as _, Error,
    SymbolicGraphCSE as _, SymbolicGraphDCE as _,
};

pub struct SymbolicGraphCompiler<'a, 'b> {
    graph: &'a SymbolicGraph,
    show_steps: bool,
    interactive_substeps: bool,
    reader: Option<CachedReader<'b>>,
    optimize_symbolic_graph: bool,
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
        rewriter: impl GraphRewrite<Error = Error>,
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

        let optional_optimizations = optimization_passes(&analysis)
            .map_err(|err| -> Error { err.into() });
        let mandatory_lowering =
            lowering_passes(&analysis).map_err(|err| -> Error { err.into() });

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
