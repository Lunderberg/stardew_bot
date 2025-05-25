use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

use itertools::{Either, Itertools, Position};

use format_utils::Indent;

use crate::{
    bytecode::expr::Scope, ExprKind, MethodTable, RuntimePrimType, RuntimeType,
    TypedPointer,
};

use super::{
    expr::StaticField, OpIndex, OpPrecedence, SymbolicGraph, SymbolicType,
    SymbolicValue,
};

impl SymbolicGraph {
    const DEFAULT_INDENT_WIDTH: usize = 4;

    pub fn printer<'a>(&'a self) -> GraphPrinter<'a> {
        GraphPrinter {
            graph: self,
            expand_all_expressions: false,
            number_all_expressions: false,
            root_subgraph_node: None,
            insert_zero_width_space_at_breakpoint: false,
            indent_width: Self::DEFAULT_INDENT_WIDTH,
        }
    }

    pub fn print<'a>(&'a self, value: SymbolicValue) -> GraphPrinter<'a> {
        GraphPrinter {
            root_subgraph_node: Some(value),
            ..self.printer()
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

    number_all_expressions: bool,

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
struct TypePrinter<'a> {
    ty: &'a SymbolicType,
    insert_zero_width_space_at_breakpoint: bool,
}

#[derive(Clone, Copy)]
pub(crate) struct IndexPrinter<'a> {
    graph: &'a SymbolicGraph,
    index: OpIndex,
    requires_name_prefix: bool,
}

#[derive(Debug)]
enum PrintItem<'a> {
    Stmt(OpIndex),
    Expr(SymbolicValue, OpPrecedence),
    ExpandedExpr(OpIndex, OpPrecedence),
    AssignmentEnd,
    FunctionBody {
        func_index: OpIndex,
        output: SymbolicValue,
        is_stmt: bool,
    },
    FunctionClose {
        is_stmt: bool,
    },
    Comma,
    OptionalTrailingComma,
    Str(&'a str),
    SymbolicType(&'a SymbolicType),
    RuntimeType(&'a RuntimeType),
    PrimType(&'a RuntimePrimType),
    MethodTablePointer(TypedPointer<MethodTable>),
    ParenOpen,
    ParenClose,
    SquareBracketOpen,
    SquareBracketClose,
    BraceOpen,
    BraceClose,
    MemberAccess,
}

trait IterPrintItemExt<'a>: Iterator<Item = PrintItem<'a>> + Sized {
    fn comma_separated(self) -> impl Iterator<Item = PrintItem<'a>> {
        self.with_position().flat_map(|(pos, item)| {
            std::iter::once(item).chain(match pos {
                Position::First | Position::Middle => Some(PrintItem::Comma),
                Position::Last => Some(PrintItem::OptionalTrailingComma),
                Position::Only => None,
            })
        })
    }
}
impl<'a, Iter> IterPrintItemExt<'a> for Iter where
    Iter: Iterator<Item = PrintItem<'a>>
{
}

impl<'a> GraphPrinter<'a> {
    pub fn expand_all_expressions(self) -> Self {
        Self {
            expand_all_expressions: true,
            ..self
        }
    }

    pub fn number_all_expressions(self) -> Self {
        Self {
            number_all_expressions: true,
            ..self
        }
    }

    pub fn insert_zero_width_space_at_breakpoint(self) -> Self {
        Self {
            insert_zero_width_space_at_breakpoint: true,
            ..self
        }
    }

    fn iter_root_nodes(
        &self,
    ) -> impl DoubleEndedIterator<Item = SymbolicValue> + '_ {
        match self.root_subgraph_node {
            Some(root_index) => Either::Left(std::iter::once(root_index)),
            None => {
                Either::Right(self.graph.iter_extern_funcs().map(Into::into))
            }
        }
    }

    fn display(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let reachable = self.graph.reachable(
            self.iter_root_nodes().filter_map(|node| node.as_op_index()),
        );

        let scope = self.graph.operation_scope(&reachable);

        let inline_expr: Vec<bool> = if self.expand_all_expressions {
            vec![false; self.graph.num_operations()]
        } else {
            let mut num_usage = vec![0; self.graph.num_operations()];
            self.graph
                .iter_ops()
                .filter(|(index, _)| reachable[index.0])
                .flat_map(|(_, op)| op.kind.iter_input_nodes())
                .for_each(|upstream_index| {
                    num_usage[upstream_index.0] += 1;
                });

            let mut used_by_child_scope =
                vec![false; self.graph.num_operations()];
            self.graph
                .iter_ops()
                .filter(|(index, _)| reachable[index.0])
                .flat_map(|(downstream_index, op)| {
                    let op_scope = match &op.kind {
                        ExprKind::Function { .. } => {
                            Scope::Function(downstream_index)
                        }
                        _ => scope[downstream_index.0],
                    };

                    match &op.kind {
                        ExprKind::IfElse {
                            condition,
                            if_branch,
                            else_branch,
                        } => Either::Left(
                            [
                                (*condition, op_scope),
                                (*if_branch, Scope::IfBranch(downstream_index)),
                                (
                                    *else_branch,
                                    Scope::ElseBranch(downstream_index),
                                ),
                            ]
                            .into_iter()
                            .filter_map(
                                |(upstream_value, expected_scope)| {
                                    upstream_value.as_op_index().map(
                                        |upstream_index| {
                                            (upstream_index, expected_scope)
                                        },
                                    )
                                },
                            ),
                        ),
                        other => Either::Right(other.iter_input_nodes().map(
                            move |upstream_index| (upstream_index, op_scope),
                        )),
                    }
                })
                .filter(|(upstream_index, op_scope)| {
                    scope[upstream_index.0] != *op_scope
                })
                .for_each(|(upstream_index, _)| {
                    used_by_child_scope[upstream_index.0] = true;
                });

            num_usage
                .into_iter()
                .enumerate()
                .zip(used_by_child_scope)
                .map(|((i, count), uses_parent_scope)| {
                    let index = OpIndex(i);
                    let op = &self.graph[index];
                    let is_subgraph_root = Some(SymbolicValue::Result(index))
                        == self.root_subgraph_node;
                    is_subgraph_root
                        || (count == 1
                            && op.name.is_none()
                            && !uses_parent_scope)
                })
                .collect()
        };

        let requires_name_prefix = {
            let mut requires_name_prefix =
                vec![self.number_all_expressions; self.graph.num_operations()];
            let mut name_lookup: HashMap<&str, OpIndex> = HashMap::new();
            self.graph
                .iter_ops()
                .filter(|(index, _)| reachable[index.0])
                .for_each(|(index, op)| {
                    if let Some(name) =
                        op.name.as_ref().map(|name| name.as_str())
                    {
                        if let Some(prev_index) = name_lookup.get(name) {
                            requires_name_prefix[index.0] = true;
                            requires_name_prefix[prev_index.0] = true;
                        } else {
                            name_lookup.insert(name, index);
                        }
                    }
                });

            requires_name_prefix
        };

        let sep =
            MaybeZeroWidthSpace(self.insert_zero_width_space_at_breakpoint);

        // A stack of operations to print.  If an operation is not yet
        // printable, such as an operation that uses a function
        // argument prior to encountering the function, it will be
        // moved to `delayed_ops` rather than printed.  After it
        // becomes printable, it will be pushed back onto the stack.
        let mut to_print: Vec<PrintItem> = Vec::new();

        if let Some(root_value) = self.root_subgraph_node {
            to_print
                .push(PrintItem::Expr(root_value, OpPrecedence::MinPrecedence));
        }

        (0..self.graph.num_operations())
            .rev()
            .filter(|i| {
                // Only display operations that are reachable when
                // starting from the displayed node.
                reachable[*i]
            })
            .filter(|i| {
                // Initialize the stack with operations in the global
                // scope.  On encountering a function definition,
                // operations within that function will be added to
                // the stack.
                scope[*i] == Scope::Global
            })
            .filter(|i| {
                // Initialize the stack only with expressions that are
                // displayed as statements.  Expressions that are
                // displayed as in-line parts of other expressions
                // will be pushed onto the stack later.
                !inline_expr[*i]
            })
            .filter(|i| {
                Some(SymbolicValue::Result(OpIndex::new(*i)))
                    != self.root_subgraph_node
            })
            .for_each(|i| {
                let item = PrintItem::Stmt(OpIndex::new(i));
                to_print.push(item);
            });

        enum PrevPrintType {
            None,
            OpenBrace,
            Comma,
            Expr,
            Stmt,
        }

        let mut prev_print_type = PrevPrintType::None;

        let mut current_indent = Indent(0);
        let mut same_line_as_previous_brace = false;

        macro_rules! advance_line {
            () => {
                write!(fmt, "\n{current_indent}")?;
                same_line_as_previous_brace = false;
            };
        }

        let make_index_printer = |index: OpIndex| IndexPrinter {
            graph: self.graph,
            index,
            requires_name_prefix: requires_name_prefix[index.0],
        };

        let extern_func_lookup: HashSet<OpIndex> =
            self.graph.iter_extern_funcs().collect();

        while let Some(print_item) = to_print.pop() {
            match print_item {
                PrintItem::Comma => {
                    write!(fmt, ",")?;
                    prev_print_type = PrevPrintType::Comma;
                }
                PrintItem::OptionalTrailingComma => {
                    // write!(fmt, ",")?;
                }
                PrintItem::Str(text) => {
                    write!(fmt, "{text}")?;
                }
                PrintItem::SymbolicType(ty) => {
                    let ty = TypePrinter {
                        ty,
                        insert_zero_width_space_at_breakpoint: self
                            .insert_zero_width_space_at_breakpoint,
                    };
                    write!(fmt, "{ty}")?;
                }
                PrintItem::ParenOpen => write!(fmt, "(")?,
                PrintItem::ParenClose => write!(fmt, ")")?,
                PrintItem::SquareBracketOpen => write!(fmt, "[")?,
                PrintItem::SquareBracketClose => write!(fmt, "]")?,
                PrintItem::BraceOpen => {
                    write!(fmt, " {{")?;
                    prev_print_type = PrevPrintType::OpenBrace;
                    current_indent += self.indent_width;
                    same_line_as_previous_brace = true;
                }
                PrintItem::BraceClose => {
                    current_indent -= self.indent_width;

                    if same_line_as_previous_brace {
                        write!(fmt, " ")?;
                    } else {
                        advance_line!();
                    }

                    write!(fmt, "}}")?;
                }
                PrintItem::MemberAccess => write!(fmt, "{sep}.")?,
                PrintItem::RuntimeType(ty) => write!(fmt, "{ty}")?,
                PrintItem::PrimType(ty) => write!(fmt, "{ty}")?,
                PrintItem::MethodTablePointer(ty) => write!(fmt, "{ty}")?,
                PrintItem::Stmt(index) => {
                    let op = &self.graph[index];

                    let index_printer = make_index_printer(index);

                    match op.as_ref() {
                        ExprKind::FunctionArg(_) => {
                            // No action needed, handled as part of Function
                        }
                        ExprKind::Function { params, output } => {
                            match prev_print_type {
                                PrevPrintType::OpenBrace
                                | PrevPrintType::Stmt => {
                                    advance_line!();
                                }
                                _ => {}
                            }

                            let is_extern_func =
                                extern_func_lookup.contains(&index);
                            if is_extern_func {
                                write!(fmt, "pub ")?;
                            }

                            write!(fmt, "fn {index_printer}")?;

                            prev_print_type = PrevPrintType::None;

                            std::iter::empty()
                                .chain(Some(PrintItem::ParenOpen))
                                .chain(
                                    params
                                        .iter()
                                        .filter_map(|param| param.as_op_index())
                                        .map(|param| {
                                            PrintItem::ExpandedExpr(
                                                param,
                                                OpPrecedence::MinPrecedence,
                                            )
                                        })
                                        .comma_separated(),
                                )
                                .chain(Some(PrintItem::ParenClose))
                                .chain(Some(PrintItem::FunctionBody {
                                    func_index: index,
                                    output: *output,
                                    is_stmt: true,
                                }))
                                .collect::<Vec<_>>()
                                .into_iter()
                                .rev()
                                .for_each(|item| to_print.push(item));
                        }
                        _ => {
                            match prev_print_type {
                                PrevPrintType::OpenBrace
                                | PrevPrintType::Stmt => {
                                    advance_line!();
                                }

                                _ => {}
                            }

                            write!(fmt, "let {index_printer} = ")?;
                            to_print.push(PrintItem::AssignmentEnd);
                            to_print.push(PrintItem::ExpandedExpr(
                                index,
                                OpPrecedence::MinPrecedence,
                            ));
                            prev_print_type = PrevPrintType::None;
                        }
                    }
                }
                PrintItem::Expr(value, parent_precedence) => {
                    match prev_print_type {
                        PrevPrintType::OpenBrace | PrevPrintType::Comma => {
                            write!(fmt, " ")?
                        }
                        PrevPrintType::Stmt => {
                            advance_line!();
                        }
                        _ => {}
                    }
                    prev_print_type = PrevPrintType::None;

                    match value {
                        SymbolicValue::Result(index)
                            if inline_expr[index.0] =>
                        {
                            to_print.push(PrintItem::ExpandedExpr(
                                index,
                                parent_precedence,
                            ));
                        }
                        SymbolicValue::Result(index) => {
                            write!(fmt, "{}", make_index_printer(index))?;
                            prev_print_type = PrevPrintType::Expr;
                        }
                        _ => {
                            write!(fmt, "{value}")?;
                            prev_print_type = PrevPrintType::Expr;
                        }
                    }
                }

                PrintItem::FunctionBody {
                    func_index,
                    output,
                    is_stmt,
                } => {
                    write!(fmt, " {{")?;

                    current_indent += self.indent_width;

                    to_print.push(PrintItem::FunctionClose { is_stmt });
                    to_print.push(PrintItem::Expr(
                        output,
                        OpPrecedence::MinPrecedence,
                    ));
                    scope
                        .iter()
                        .enumerate()
                        .rev()
                        .filter(|(_, op_scope)| {
                            **op_scope == Scope::Function(func_index)
                        })
                        .filter(|(i, _)| !inline_expr[*i])
                        .for_each(|(i, _)| {
                            let op_index = OpIndex::new(i);
                            to_print.push(PrintItem::Stmt(op_index));
                        });

                    same_line_as_previous_brace = true;
                    prev_print_type = PrevPrintType::OpenBrace;
                }
                PrintItem::FunctionClose { is_stmt, .. } => {
                    current_indent -= self.indent_width;
                    if same_line_as_previous_brace {
                        write!(fmt, " ")?;
                    } else {
                        advance_line!();
                    }

                    write!(fmt, "}}")?;
                    prev_print_type = if is_stmt {
                        PrevPrintType::Stmt
                    } else {
                        PrevPrintType::Expr
                    };
                }
                PrintItem::AssignmentEnd => {
                    write!(fmt, ";")?;
                    prev_print_type = PrevPrintType::Stmt;
                }

                PrintItem::ExpandedExpr(index, parent_precedence) => {
                    match prev_print_type {
                        PrevPrintType::OpenBrace | PrevPrintType::Comma => {
                            write!(fmt, " ")?
                        }
                        PrevPrintType::Stmt => {
                            advance_line!();
                        }
                        _ => {}
                    }

                    macro_rules! handle_binary_op {
                        ($op:literal, $precedence:expr, $lhs:expr, $rhs:expr) => {{
                            let requires_paren =
                                !($precedence >= parent_precedence);

                            std::iter::empty()
                                .chain(
                                    requires_paren
                                        .then(|| PrintItem::ParenOpen),
                                )
                                .chain(Some(PrintItem::Expr(
                                    *$lhs,
                                    $precedence,
                                )))
                                .chain(Some(PrintItem::Str($op)))
                                .chain(Some(PrintItem::Expr(
                                    *$rhs,
                                    $precedence,
                                )))
                                .chain(
                                    requires_paren
                                        .then(|| PrintItem::ParenClose),
                                )
                                .rev()
                                .for_each(|print_item| {
                                    to_print.push(print_item)
                                });
                        }};
                    }

                    match &self.graph[index].kind {
                        ExprKind::None => write!(fmt, "None")?,

                        ExprKind::FunctionArg(RuntimeType::Unknown) => {
                            let index = make_index_printer(index);
                            write!(fmt, "{index}")?;
                        }

                        ExprKind::FunctionArg(ty) => {
                            let index = make_index_printer(index);
                            write!(fmt, "{index}: {ty}")?;
                        }
                        ExprKind::Function { params, output } => {
                            std::iter::empty()
                                .chain(Some(PrintItem::Str("|")))
                                .chain(
                                    params
                                        .iter()
                                        .filter_map(|param| param.as_op_index())
                                        .map(|param| {
                                            PrintItem::ExpandedExpr(
                                                param,
                                                OpPrecedence::MinPrecedence,
                                            )
                                        })
                                        .comma_separated(),
                                )
                                .chain(Some(PrintItem::Str("|")))
                                .chain(Some(PrintItem::FunctionBody {
                                    func_index: index,
                                    output: *output,
                                    is_stmt: false,
                                }))
                                .collect::<Vec<_>>()
                                .into_iter()
                                .rev()
                                .for_each(|item| to_print.push(item));
                        }
                        ExprKind::Map { iterator, map } => {
                            [
                                PrintItem::Expr(
                                    *iterator,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::Str(".map"),
                                PrintItem::ParenOpen,
                                PrintItem::Expr(
                                    *map,
                                    OpPrecedence::MinPrecedence,
                                ),
                                PrintItem::ParenClose,
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::Filter { iterator, filter } => {
                            [
                                PrintItem::Expr(
                                    *iterator,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::Str(".filter"),
                                PrintItem::ParenOpen,
                                PrintItem::Expr(
                                    *filter,
                                    OpPrecedence::MinPrecedence,
                                ),
                                PrintItem::ParenClose,
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::Chain(iter_a, iter_b) => {
                            [
                                PrintItem::Expr(
                                    *iter_a,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::Str(".chain"),
                                PrintItem::ParenOpen,
                                PrintItem::Expr(
                                    *iter_b,
                                    OpPrecedence::MinPrecedence,
                                ),
                                PrintItem::ParenClose,
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::Collect { iterator } => {
                            [
                                PrintItem::Expr(
                                    *iterator,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::Str(".collect()"),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::Reduce {
                            initial,
                            iterator,
                            reduction,
                        } => {
                            [
                                PrintItem::Expr(
                                    *iterator,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::Str(".reduce"),
                                PrintItem::ParenOpen,
                                PrintItem::Expr(
                                    *initial,
                                    OpPrecedence::MinPrecedence,
                                ),
                                PrintItem::Comma,
                                PrintItem::Expr(
                                    *reduction,
                                    OpPrecedence::MinPrecedence,
                                ),
                                PrintItem::OptionalTrailingComma,
                                PrintItem::ParenClose,
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }

                        ExprKind::FunctionCall { func, args } => {
                            std::iter::empty()
                                .chain(Some(PrintItem::Expr(
                                    *func,
                                    OpPrecedence::MaxPrecedence,
                                )))
                                .chain(Some(PrintItem::ParenOpen))
                                .chain(
                                    args.iter()
                                        .map(|arg| {
                                            PrintItem::Expr(
                                                *arg,
                                                OpPrecedence::MinPrecedence,
                                            )
                                        })
                                        .comma_separated(),
                                )
                                .chain(Some(PrintItem::ParenClose))
                                .collect::<Vec<_>>()
                                .into_iter()
                                .rev()
                                .for_each(|item| to_print.push(item));
                        }

                        ExprKind::SimpleReduce {
                            initial,
                            extent,
                            reduction,
                        } => {
                            [
                                PrintItem::Str("(0.."),
                                PrintItem::Expr(
                                    *extent,
                                    OpPrecedence::RangeExtent,
                                ),
                                PrintItem::Str(").reduce"),
                                PrintItem::ParenOpen,
                                PrintItem::Expr(
                                    *initial,
                                    OpPrecedence::MinPrecedence,
                                ),
                                PrintItem::Comma,
                                PrintItem::Expr(
                                    *reduction,
                                    OpPrecedence::MinPrecedence,
                                ),
                                PrintItem::OptionalTrailingComma,
                                PrintItem::ParenClose,
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }

                        ExprKind::Tuple(values) => {
                            std::iter::empty()
                                .chain([PrintItem::ParenOpen])
                                .chain(
                                    values
                                        .iter()
                                        .map(|arg| {
                                            PrintItem::Expr(
                                                *arg,
                                                OpPrecedence::MinPrecedence,
                                            )
                                        })
                                        .comma_separated(),
                                )
                                .chain(Some(PrintItem::ParenClose))
                                .collect::<Vec<_>>()
                                .into_iter()
                                .rev()
                                .for_each(|item| to_print.push(item));
                        }
                        ExprKind::NativeFunction(func) => {
                            write!(fmt, "{func}")?
                        }

                        ExprKind::Range { extent } => {
                            [
                                PrintItem::Str("(0.."),
                                PrintItem::Expr(
                                    *extent,
                                    OpPrecedence::RangeExtent,
                                ),
                                PrintItem::Str(")"),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }

                        ExprKind::StaticField(StaticField {
                            class,
                            field_name,
                        }) => {
                            [
                                PrintItem::SymbolicType(class),
                                PrintItem::MemberAccess,
                                PrintItem::Str(field_name),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::FieldAccess { obj, field } => {
                            [
                                PrintItem::Expr(
                                    *obj,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str(field),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::SymbolicDowncast { obj, ty } => {
                            [
                                PrintItem::Expr(
                                    *obj,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::Str(".as::<"),
                                PrintItem::SymbolicType(ty),
                                PrintItem::Str(">()"),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::IndexAccess { obj, indices } => {
                            std::iter::once(PrintItem::Expr(
                                *obj,
                                OpPrecedence::MaxPrecedence,
                            ))
                            .chain(Some(PrintItem::SquareBracketOpen))
                            .chain(
                                indices
                                    .iter()
                                    .map(|index| {
                                        PrintItem::Expr(
                                            *index,
                                            OpPrecedence::MinPrecedence,
                                        )
                                    })
                                    .comma_separated(),
                            )
                            .chain(Some(PrintItem::SquareBracketClose))
                            .collect::<Vec<_>>()
                            .into_iter()
                            .rev()
                            .for_each(|item| to_print.push(item));
                        }
                        ExprKind::NumArrayElements { array } => {
                            [
                                PrintItem::Expr(
                                    *array,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str("len()"),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::ArrayExtent { array, dim } => {
                            [
                                PrintItem::Expr(
                                    *array,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str("extent"),
                                PrintItem::ParenOpen,
                                PrintItem::Expr(
                                    *dim,
                                    OpPrecedence::MinPrecedence,
                                ),
                                PrintItem::ParenClose,
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::PointerCast { ptr, ty } => {
                            [
                                PrintItem::Expr(
                                    *ptr,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str("ptr_cast::<"),
                                PrintItem::RuntimeType(ty),
                                PrintItem::Str(">"),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::IsSome(value) => {
                            [
                                PrintItem::Expr(
                                    *value,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str("is_some()"),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::IfElse {
                            condition,
                            if_branch,
                            else_branch,
                        } => {
                            let iter_if_statements = scope
                                .iter()
                                .enumerate()
                                .filter(|(_, op_scope)| {
                                    **op_scope == Scope::IfBranch(index)
                                })
                                .filter(|(i, _)| !inline_expr[*i])
                                .map(|(i, _)| PrintItem::Stmt(OpIndex::new(i)));
                            let make_iter_else_statements = || {
                                scope
                                    .iter()
                                    .enumerate()
                                    .filter(|(_, op_scope)| {
                                        **op_scope == Scope::ElseBranch(index)
                                    })
                                    .filter(|(i, _)| !inline_expr[*i])
                                    .map(|(i, _)| {
                                        PrintItem::Stmt(OpIndex::new(i))
                                    })
                            };

                            let print_chained_else_if =
                                make_iter_else_statements().next().is_none()
                                    && else_branch
                                        .as_op_index()
                                        .map(|else_index| {
                                            matches!(
                                                self.graph[else_index].kind,
                                                ExprKind::IfElse { .. }
                                            )
                                        })
                                        .unwrap_or(false);

                            let else_branch_value = PrintItem::Expr(
                                *else_branch,
                                OpPrecedence::MinPrecedence,
                            );

                            let iter_else = if print_chained_else_if {
                                Either::Left(
                                    [PrintItem::Str(" "), else_branch_value]
                                        .into_iter(),
                                )
                            } else {
                                let iter = std::iter::empty()
                                    .chain([PrintItem::BraceOpen])
                                    .chain(make_iter_else_statements())
                                    .chain([
                                        else_branch_value,
                                        PrintItem::BraceClose,
                                    ]);
                                Either::Right(iter)
                            };

                            std::iter::empty()
                                .chain([
                                    PrintItem::Str("if "),
                                    PrintItem::Expr(
                                        *condition,
                                        OpPrecedence::MinPrecedence,
                                    ),
                                    PrintItem::BraceOpen,
                                ])
                                .chain(iter_if_statements)
                                .chain([
                                    PrintItem::Expr(
                                        *if_branch,
                                        OpPrecedence::MinPrecedence,
                                    ),
                                    PrintItem::BraceClose,
                                    PrintItem::Str(" else"),
                                ])
                                .chain(iter_else)
                                .rev()
                                .for_each(|print_item| {
                                    to_print.push(print_item)
                                });
                        }
                        ExprKind::And { lhs, rhs } => {
                            handle_binary_op!(
                                " && ",
                                OpPrecedence::BooleanAnd,
                                lhs,
                                rhs
                            )
                        }
                        ExprKind::Or { lhs, rhs } => {
                            handle_binary_op!(
                                " || ",
                                OpPrecedence::BooleanOr,
                                lhs,
                                rhs
                            )
                        }
                        ExprKind::Not { arg } => {
                            write!(fmt, "!")?;
                            to_print.push(PrintItem::Expr(
                                *arg,
                                OpPrecedence::BooleanNot,
                            ));
                        }
                        ExprKind::Equal { lhs, rhs } => {
                            handle_binary_op!(
                                " == ",
                                OpPrecedence::ComparisonOperator,
                                lhs,
                                rhs
                            )
                        }
                        ExprKind::NotEqual { lhs, rhs } => {
                            handle_binary_op!(
                                " != ",
                                OpPrecedence::ComparisonOperator,
                                lhs,
                                rhs
                            )
                        }
                        ExprKind::GreaterThan { lhs, rhs } => {
                            handle_binary_op!(
                                " > ",
                                OpPrecedence::ComparisonOperator,
                                lhs,
                                rhs
                            )
                        }
                        ExprKind::LessThan { lhs, rhs } => {
                            handle_binary_op!(
                                " < ",
                                OpPrecedence::ComparisonOperator,
                                lhs,
                                rhs
                            )
                        }
                        ExprKind::GreaterThanOrEqual { lhs, rhs } => {
                            handle_binary_op!(
                                " >= ",
                                OpPrecedence::ComparisonOperator,
                                lhs,
                                rhs
                            )
                        }
                        ExprKind::LessThanOrEqual { lhs, rhs } => {
                            handle_binary_op!(
                                " <= ",
                                OpPrecedence::ComparisonOperator,
                                lhs,
                                rhs
                            )
                        }

                        ExprKind::Add { lhs, rhs } => {
                            handle_binary_op!(
                                " + ",
                                OpPrecedence::AddSub,
                                lhs,
                                rhs
                            )
                        }
                        ExprKind::Sub { lhs, rhs } => {
                            handle_binary_op!(
                                " - ",
                                OpPrecedence::AddSub,
                                lhs,
                                rhs
                            )
                        }
                        ExprKind::Mul { lhs, rhs } => {
                            handle_binary_op!(
                                "*",
                                OpPrecedence::MulDiv,
                                lhs,
                                rhs
                            )
                        }
                        ExprKind::Div { lhs, rhs } => {
                            handle_binary_op!(
                                "/",
                                OpPrecedence::MulDiv,
                                lhs,
                                rhs
                            )
                        }
                        ExprKind::Mod { lhs, rhs } => {
                            handle_binary_op!(
                                "%",
                                OpPrecedence::MulDiv,
                                lhs,
                                rhs
                            )
                        }

                        ExprKind::PrimCast { value, prim_type } => {
                            [
                                PrintItem::Expr(
                                    *value,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str("prim_cast::<"),
                                PrintItem::PrimType(prim_type),
                                PrintItem::Str(">()"),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }

                        ExprKind::IsSubclassOf {
                            method_table_ptr,
                            ty,
                        } => {
                            [
                                PrintItem::Expr(
                                    *method_table_ptr,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str("is_subclass_of::<"),
                                PrintItem::MethodTablePointer(*ty),
                                PrintItem::Str(">()"),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }

                        ExprKind::PhysicalDowncast { obj, ty } => {
                            [
                                PrintItem::Expr(
                                    *obj,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str("downcast::<"),
                                PrintItem::MethodTablePointer(*ty),
                                PrintItem::Str(">()"),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::ReadPrim { ptr, prim_type } => {
                            [
                                PrintItem::Expr(
                                    *ptr,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str("read_prim::<"),
                                PrintItem::PrimType(prim_type),
                                PrintItem::Str(">()"),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::ReadBytes(regions) if regions.len() == 1 => {
                            let ptr = regions[0].ptr;
                            let num_bytes = regions[0].num_bytes;
                            [
                                PrintItem::Expr(
                                    ptr,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str("read_bytes"),
                                PrintItem::ParenOpen,
                                PrintItem::Expr(
                                    num_bytes,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::ParenClose,
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::ReadBytes(regions) => {
                            std::iter::empty()
                                .chain([
                                    PrintItem::Str("read_bytes"),
                                    PrintItem::ParenOpen,
                                ])
                                .chain(regions.iter().enumerate().flat_map(
                                    |(i, region)| {
                                        let ptr = PrintItem::Expr(
                                            region.ptr,
                                            OpPrecedence::TupleElement,
                                        );
                                        let num_bytes = PrintItem::Expr(
                                            region.num_bytes,
                                            OpPrecedence::TupleElement,
                                        );
                                        [
                                            Some(ptr),
                                            Some(PrintItem::Comma),
                                            Some(num_bytes),
                                            Some(PrintItem::Comma).filter(
                                                |_| (i + 1) < regions.len(),
                                            ),
                                        ]
                                        .into_iter()
                                        .flatten()
                                    },
                                ))
                                .chain(Some(PrintItem::ParenClose))
                                .rev()
                                .for_each(|print_item| {
                                    to_print.push(print_item)
                                });
                        }
                        ExprKind::CastBytes {
                            bytes,
                            offset,
                            prim_type,
                        } => {
                            [
                                PrintItem::Expr(
                                    *bytes,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str("cast_bytes::<"),
                                PrintItem::PrimType(prim_type),
                                PrintItem::Str(">"),
                                PrintItem::ParenOpen,
                                PrintItem::Expr(
                                    *offset,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::ParenClose,
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                        ExprKind::ReadString { ptr } => {
                            [
                                PrintItem::Expr(
                                    *ptr,
                                    OpPrecedence::MaxPrecedence,
                                ),
                                PrintItem::MemberAccess,
                                PrintItem::Str(".read_string()"),
                            ]
                            .into_iter()
                            .rev()
                            .for_each(|print_item| to_print.push(print_item));
                        }
                    }
                    prev_print_type = PrevPrintType::Expr;
                }
            }
        }

        Ok(())
    }
}

impl<'a> Display for GraphPrinter<'a> {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(fmt)
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

impl<'a> IndexPrinter<'a> {
    pub(crate) fn new(index: OpIndex, graph: &'a SymbolicGraph) -> Self {
        Self {
            graph,
            index,
            requires_name_prefix: true,
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
