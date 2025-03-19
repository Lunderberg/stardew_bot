use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use format_utils::Indent;

use crate::{ExprKind, RuntimeType};

use super::{
    expr::StaticField, OpIndex, SymbolicGraph, SymbolicType, SymbolicValue,
};

impl SymbolicGraph {
    const DEFAULT_INDENT_WIDTH: usize = 4;

    pub fn printer<'a>(&'a self) -> GraphPrinter<'a> {
        GraphPrinter {
            graph: self,
            expand_all_expressions: false,
            root_subgraph_node: None,
            insert_zero_width_space_at_breakpoint: false,
            indent_width: Self::DEFAULT_INDENT_WIDTH,
        }
    }

    pub fn print<'a>(&'a self, value: SymbolicValue) -> GraphPrinter<'a> {
        GraphPrinter {
            graph: self,
            root_subgraph_node: Some(value),
            expand_all_expressions: false,
            insert_zero_width_space_at_breakpoint: false,
            indent_width: Self::DEFAULT_INDENT_WIDTH,
        }
    }
}

pub struct GraphPrinter<'a> {
    graph: &'a SymbolicGraph,

    /// If true, expand all expressions into non-nested expressions.
    /// If false, only expand expressions if necessary for unambiguous
    /// representation of the underlying expression.
    ///
    /// As an example of this ambiguity, consider the expression `(x +
    /// y) + (x + y)`.  Since `(x + y)` shows up multiple times, it
    /// could be evaluated either once or twice.  When using
    /// `GraphPrinter`, all subexpressions on the right-hand side of
    /// an assignment are distinct.
    ///
    /// Two evaluations, printed with `expand_all_expressions: true`
    ///     let _0 = x + y;
    ///     let _1 = x + y;
    ///     let _2 = _0 + _1;
    ///
    /// Two evaluations, printed with `expand_all_expressions: false`
    ///     let _2 = (x + y) + (x + y);
    ///
    /// One evaluation, printed with `expand_all_expressions: true`
    ///     let _0 = x + y;
    ///     let _1 = _0 + _0;
    ///
    /// One evaluation, printed with `expand_all_expressions: false`.
    /// The expression is still expanded into two separate
    /// assignments, to avoid ambiguity with the earlier case.
    ///     let _0 = x + y;
    ///     let _1 = _0 + _0;
    expand_all_expressions: bool,

    /// The subgraph to be printed.  If it contains a value, only
    /// nodes that are used by the specified value will be printed.
    /// Otherwise, all nodes will be printed.
    root_subgraph_node: Option<SymbolicValue>,

    /// If true, insert zero-width spaces at locations that would be
    /// convenient for line breaks to be inserted.  If false, do not
    /// insert the zero-width spaces.
    insert_zero_width_space_at_breakpoint: bool,

    /// The number of spaces to indent for the body of functions.
    indent_width: usize,
}

#[derive(Clone, Copy)]
pub struct ExprPrinter<'a> {
    graph: &'a SymbolicGraph,
    value: SymbolicValue,
    is_top_level: bool,
    inline_expr: &'a [bool],
    requires_name_prefix: &'a [bool],
    insert_zero_width_space_at_breakpoint: bool,
}

#[derive(Clone, Copy)]
struct TypePrinter<'a> {
    ty: &'a SymbolicType,
    insert_zero_width_space_at_breakpoint: bool,
}

#[derive(Clone, Copy)]
struct IndexPrinter<'a> {
    graph: &'a SymbolicGraph,
    index: OpIndex,
    requires_name_prefix: bool,
}

impl<'a> GraphPrinter<'a> {
    pub fn expand_all_expressions(self) -> Self {
        Self {
            expand_all_expressions: true,
            ..self
        }
    }

    pub fn insert_zero_width_space_at_breakpoint(self) -> Self {
        Self {
            insert_zero_width_space_at_breakpoint: true,
            ..self
        }
    }

    fn display(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reachable = if let Some(root_node) = self.root_subgraph_node {
            self.graph.reachable(std::iter::once(root_node))
        } else {
            vec![true; self.graph.num_operations()]
        };

        let scope = self.graph.operation_scope();

        let inline_expr: Vec<bool> = if self.expand_all_expressions {
            vec![false; self.graph.num_operations()]
        } else {
            let mut num_usage = vec![0; self.graph.num_operations()];
            let mut used_by_child_scope =
                vec![false; self.graph.num_operations()];
            self.graph
                .iter_ops()
                .filter(|(index, _)| reachable[index.0])
                .for_each(|(downstream_index, op)| {
                    op.visit_reachable_nodes(|input_value| {
                        if let SymbolicValue::Result(upstream_index) =
                            input_value
                        {
                            num_usage[upstream_index.0] += 1;
                            if scope[upstream_index.0]
                                != scope[downstream_index.0]
                            {
                                used_by_child_scope[upstream_index.0] = true;
                            }
                        }
                    })
                });

            num_usage
                .into_iter()
                .enumerate()
                .zip(used_by_child_scope)
                .map(|((i, count), uses_parent_scope)| {
                    let index = OpIndex(i);
                    let op = &self.graph[index];
                    count == 1 && op.name.is_none() && !uses_parent_scope
                })
                .collect()
        };

        let requires_name_prefix = {
            let mut requires_name_prefix =
                vec![false; self.graph.num_operations()];
            let mut name_lookup: HashMap<&str, OpIndex> = HashMap::new();
            for (index, op) in self.graph.iter_ops() {
                if let Some(name) = op.name.as_ref().map(|name| name.as_str()) {
                    if let Some(prev_index) = name_lookup.get(name) {
                        requires_name_prefix[index.0] = true;
                        requires_name_prefix[prev_index.0] = true;
                    } else {
                        name_lookup.insert(name, index);
                    }
                }
            }

            requires_name_prefix
        };

        #[derive(Debug)]
        struct PrintItem {
            kind: PrintItemKind,
            indent: Indent,
        }
        #[derive(Debug)]
        enum PrintItemKind {
            Op(OpIndex),
            FunctionOutput {
                output: SymbolicValue,
                is_extern_func: bool,
            },
        }

        // A stack of operations to print.  If an operation is not yet
        // printable, such as an operation that uses a function
        // argument prior to encountering the function, it will be
        // moved to `delayed_ops` rather than printed.  After it
        // becomes printable, it will be pushed back onto the stack.
        let mut to_print: Vec<PrintItem> = (0..self.graph.num_operations())
            .rev()
            .map(|i| PrintItem {
                kind: PrintItemKind::Op(OpIndex::new(i)),
                indent: Indent(0),
            })
            .collect();

        // Track operations that depend on a function argument.  These
        // shouldn't be printed until encountering the function that
        // owns them.
        let mut delayed_ops: Vec<PrintItem> = Vec::new();
        // Lookup from an operation to the Expr::FunctionArg that it makes use of.
        //
        // TODO: Something feels wrong for the order in which these
        // are looked up.  If there's an expression that uses
        // arguments from two separate functions, then the expression
        // is contained within both functions.  On encountering either
        // function definition, you know that the expression is part
        // of the innermost function.  But I'm only tracking a single
        // index here.
        let mut delayed_op_lookup: HashMap<OpIndex, OpIndex> = HashMap::new();

        #[derive(PartialEq, PartialOrd)]
        enum DelayedNewline {
            None,
            BeforeAssignment,
            BeforeExpression,
        }

        let mut delayed_newline = DelayedNewline::None;

        let make_expr_printer =
            |value: SymbolicValue, is_top_level: bool| ExprPrinter {
                graph: self.graph,
                value,
                is_top_level,
                inline_expr: &inline_expr,
                requires_name_prefix: &requires_name_prefix,
                insert_zero_width_space_at_breakpoint: self
                    .insert_zero_width_space_at_breakpoint,
            };

        let extern_func_lookup: HashSet<OpIndex> =
            self.graph.iter_extern_funcs().collect();

        while let Some(print_item) = to_print.pop() {
            let indent = print_item.indent;
            let index = match print_item.kind {
                PrintItemKind::Op(index) => index,
                PrintItemKind::FunctionOutput {
                    output,
                    is_extern_func,
                } => {
                    if delayed_newline >= DelayedNewline::BeforeExpression {
                        write!(fmt, "\n{indent}")?;
                    } else {
                        write!(fmt, " ")?;
                    }
                    write!(fmt, "{}", make_expr_printer(output, false))?;

                    if delayed_newline >= DelayedNewline::BeforeExpression {
                        let closing_indent = indent - self.indent_width;
                        write!(fmt, "\n{closing_indent}")?;
                    } else {
                        write!(fmt, " ")?;
                    }

                    write!(fmt, "}}")?;
                    if !is_extern_func {
                        write!(fmt, ";")?;
                        delayed_newline = DelayedNewline::BeforeExpression;
                    }
                    continue;
                }
            };

            let op = &self.graph[index];

            if !reachable[index.0] {
                // Only display operations that are reachable when
                // starting from the displayed node.
                continue;
            }

            let mut uses_param = None;
            if !matches!(op.kind, ExprKind::Function { .. }) {
                op.visit_reachable_nodes(|input| {
                    if let SymbolicValue::Result(op_index) = input {
                        if let Some(function_arg) =
                            delayed_op_lookup.get(&op_index)
                        {
                            uses_param = Some(function_arg);
                        }
                    }
                });
            }
            if let Some(function_arg) = uses_param {
                // Depends on a function argument.  Mark it for
                // printing out later.
                delayed_ops.push(PrintItem {
                    indent: indent + self.indent_width,
                    ..print_item
                });
                delayed_op_lookup.insert(index, *function_arg);
                continue;
            }

            if inline_expr[index.0] {
                // Do not provide a separate printout for expressions that
                // are being displayed inline.
                continue;
            }

            let index_printer = IndexPrinter {
                graph: self.graph,
                index,
                requires_name_prefix: requires_name_prefix[index.0],
            };
            let expr_printer = make_expr_printer(index.into(), true);

            let is_root_node =
                Some(SymbolicValue::Result(index)) == self.root_subgraph_node;

            if is_root_node {
                if delayed_newline >= DelayedNewline::BeforeExpression {
                    write!(fmt, "\n{indent}")?;
                }

                write!(fmt, "{expr_printer}")?;
                continue;
            }

            match op.as_ref() {
                ExprKind::FunctionArg(_) => {
                    delayed_op_lookup.insert(index, index);
                }
                ExprKind::Function { params, output } => {
                    if delayed_newline >= DelayedNewline::BeforeExpression {
                        write!(fmt, "\n{indent}")?;
                    }

                    let is_extern_func = extern_func_lookup.contains(&index);

                    if is_extern_func {
                        write!(fmt, "pub fn {index_printer}(")?;
                    } else {
                        write!(fmt, "let {index_printer} = ")?;
                        write!(fmt, "fn(")?;
                    }
                    for (i, param) in params.iter().enumerate() {
                        if i > 0 {
                            write!(fmt, ", ")?;
                        }
                        let param =
                            expr_printer.with_value(*param).as_top_level();
                        write!(fmt, "{param}")?;
                    }
                    write!(fmt, ") {{")?;

                    to_print.push(PrintItem {
                        kind: PrintItemKind::FunctionOutput {
                            output: *output,
                            is_extern_func,
                        },
                        indent: indent + self.indent_width,
                    });

                    params
                        .iter()
                        .filter_map(|param| match param {
                            SymbolicValue::Result(param_index) => {
                                Some(param_index)
                            }
                            _ => None,
                        })
                        .for_each(|param_index| {
                            delayed_op_lookup.remove(param_index);
                        });

                    let mut stolen_ops = Vec::new();
                    std::mem::swap(&mut delayed_ops, &mut stolen_ops);
                    for delayed in stolen_ops.into_iter().rev() {
                        if let PrintItemKind::Op(delayed_index) = &delayed.kind
                        {
                            delayed_op_lookup.remove(delayed_index);
                        }
                        to_print.push(delayed);
                    }
                    delayed_newline = DelayedNewline::BeforeAssignment;
                }
                _ => {
                    if delayed_newline >= DelayedNewline::BeforeAssignment {
                        write!(fmt, "\n{indent}")?;
                    }
                    write!(fmt, "let {index_printer} = {expr_printer};")?;
                    delayed_newline = DelayedNewline::BeforeExpression;
                }
            }
        }

        assert_eq!(delayed_ops.len(), 0);

        Ok(())
    }
}

impl<'a> Display for GraphPrinter<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(fmt)
    }
}

impl<'a> ExprPrinter<'a> {
    fn with_value(self, value: SymbolicValue) -> Self {
        Self {
            value,
            is_top_level: false,
            ..self
        }
    }

    fn as_top_level(self) -> Self {
        Self {
            is_top_level: true,
            ..self
        }
    }
}

struct MaybeZeroWidthSpace(bool);
impl Display for MaybeZeroWidthSpace {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 {
            write!(f, "\u{200B}")
        } else {
            Ok(())
        }
    }
}

impl<'a> Display for IndexPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let index = self.index.0;
        if let Some(name) = self.graph[self.index].name.as_ref() {
            if self.requires_name_prefix {
                write!(f, "_{index}_{name}")
            } else {
                write!(f, "{name}")
            }
        } else {
            write!(f, "_{index}")
        }
    }
}

impl<'a> Display for ExprPrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let op_index = match self.value {
            SymbolicValue::Int(int) => {
                return write!(f, "{int}");
            }
            SymbolicValue::Ptr(ptr) => {
                return write!(f, "{ptr}");
            }
            SymbolicValue::Result(op_index) => op_index,
        };

        let index_printer = IndexPrinter {
            graph: self.graph,
            index: op_index,
            requires_name_prefix: self.requires_name_prefix[op_index.0],
        };

        let display_full_expr =
            self.is_top_level || self.inline_expr[op_index.0];
        if !display_full_expr {
            return write!(f, "{index_printer}");
        }

        let sep =
            MaybeZeroWidthSpace(self.insert_zero_width_space_at_breakpoint);

        let write_tuple = |f: &mut std::fmt::Formatter<'_>,
                           tuple: &[SymbolicValue],
                           is_top_level: bool|
         -> std::fmt::Result {
            write!(f, "(")?;
            for (i, element) in tuple.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                let element = self.with_value(*element);
                let element = if is_top_level {
                    element.as_top_level()
                } else {
                    element
                };
                write!(f, "{element}")?;
            }

            write!(f, ")")?;
            Ok(())
        };

        match self.graph[op_index].as_ref() {
            ExprKind::Function { params, output } => {
                write!(f, "fn")?;
                write_tuple(f, params, true)?;
                write!(f, " {{ {} }}", self.with_value(*output))?;

                Ok(())
            }
            ExprKind::FunctionArg(ty) => {
                write!(f, "{index_printer}")?;
                if self.is_top_level && !matches!(ty, RuntimeType::Unknown) {
                    write!(f, ": {ty}")?;
                }
                Ok(())
            }
            ExprKind::FunctionCall { func, args } => {
                let func = self.with_value(*func);
                write!(f, "{func}")?;
                write_tuple(f, args, false)
            }
            ExprKind::Range { extent } => {
                let extent = self.with_value(*extent);
                write!(f, "(0..{extent})")
            }
            ExprKind::Reduce {
                initial,
                iterator,
                reduction,
            } => {
                let initial = self.with_value(*initial);
                let iterator = self.with_value(*iterator);
                let reduction = self.with_value(*reduction);
                write!(f, "{iterator}.reduce({initial}, {reduction})")
            }
            ExprKind::SimpleReduce {
                initial,
                extent,
                reduction,
            } => {
                let initial = self.with_value(*initial);
                let extent = self.with_value(*extent);
                let reduction = self.with_value(*reduction);
                write!(f, "(0..{extent}).reduce({initial}, {reduction})")
            }
            ExprKind::NativeFunction(func) => {
                write!(f, "{func}")
            }
            ExprKind::Tuple(elements) => write_tuple(f, elements, false),
            ExprKind::StaticField(StaticField { class, field_name }) => {
                write!(f, "{class}{sep}.{field_name}")
            }
            ExprKind::FieldAccess { obj, field } => {
                let obj = self.with_value(*obj);
                write!(f, "{obj}{sep}.{field}")
            }
            ExprKind::IndexAccess { obj, indices } => {
                let obj = self.with_value(*obj);
                write!(f, "{obj}[{sep}")?;

                for (i, index) in indices.iter().enumerate() {
                    let index = self.with_value(*index);
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{index}")?;
                }

                write!(f, "{sep}]")
            }
            ExprKind::SymbolicDowncast { obj, ty } => {
                let obj = self.with_value(*obj);
                let ty = TypePrinter {
                    ty,
                    insert_zero_width_space_at_breakpoint: self
                        .insert_zero_width_space_at_breakpoint,
                };
                write!(f, "{obj}.as::<{sep}{ty}{sep}>()")
            }
            ExprKind::NumArrayElements { array } => {
                let array = self.with_value(*array);
                write!(f, "{array}{sep}.len()")
            }
            ExprKind::ArrayExtent { array, dim } => {
                let array = self.with_value(*array);
                let dim = self.with_value(*dim);
                write!(f, "{array}{sep}.extent({dim})")
            }
            ExprKind::PointerCast { ptr, ty } => {
                let ptr = self.with_value(*ptr);
                write!(f, "{ptr}{sep}.ptr_cast::<{ty}>()")
            }
            ExprKind::IsSome(value) => {
                let value = self.with_value(*value);
                write!(f, "{value}.is_some()")
            }
            ExprKind::Add { lhs, rhs } => {
                let lhs = self.with_value(*lhs);
                let rhs = self.with_value(*rhs);
                write!(f, "{lhs} + {rhs}")
            }
            ExprKind::Mul { lhs, rhs } => {
                let lhs = self.with_value(*lhs);
                let rhs = self.with_value(*rhs);
                write!(f, "{lhs}*{rhs}")
            }
            ExprKind::PhysicalDowncast { obj, ty } => {
                let obj = self.with_value(*obj);
                write!(f, "{obj}.downcast::<{ty}>()")
            }
            ExprKind::PrimCast { value, prim_type } => {
                let value = self.with_value(*value);
                write!(f, "{value}.prim_cast::<{prim_type}>()")
            }
            ExprKind::ReadValue { ptr, prim_type } => {
                let ptr = self.with_value(*ptr);
                write!(f, "{ptr}.read::<{prim_type}>()")
            }
            ExprKind::ReadString { ptr } => {
                let ptr = self.with_value(*ptr);
                write!(f, "{ptr}.read_string()")
            }
        }
    }
}

impl<'a> Display for TypePrinter<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let SymbolicType {
            full_name,
            generics,
        } = &self.ty;
        let sep =
            MaybeZeroWidthSpace(self.insert_zero_width_space_at_breakpoint);

        write!(f, "{full_name}")?;

        if !generics.is_empty() {
            write!(f, "<{sep}")?;
            for (i, generic) in generics.iter().enumerate() {
                if i > 0 {
                    write!(f, ", {sep}")?;
                }
                write!(f, "{generic}")?;
            }
            write!(f, ">")?;
        }

        Ok(())
    }
}

impl Display for SymbolicType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let printer = TypePrinter {
            ty: self,
            insert_zero_width_space_at_breakpoint: false,
        };
        write!(f, "{printer}")
    }
}

impl Display for SymbolicGraph {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.printer())
    }
}
