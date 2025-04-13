use std::{borrow::Cow, collections::HashMap, ops::Range};

use itertools::Itertools as _;
use thiserror::Error;

use super::expr::{SymbolicGraph, SymbolicType, SymbolicValue};
use crate::{Error, RuntimeType};

pub(crate) struct SymbolicParser<'a> {
    tokens: SymbolicTokenizer<'a>,
    identifiers: HashMap<Cow<'a, str>, SymbolicValue>,
    graph: &'a mut SymbolicGraph,
}

/// Errors in parsing the symbolic chain.  There are enough of these
/// error that they are collected here to avoid
#[derive(Error)]
pub enum ParseError {
    #[error("Unexpected {c:?} at byte {byte_index}")]
    UnexpectedChar { c: char, byte_index: usize },

    #[error(
        "Expected {expected:?} at byte {byte_index}, \
         but found {actual:?}"
    )]
    UnexpectedCharWithKnown {
        expected: char,
        actual: char,
        byte_index: usize,
    },

    #[error("Expected {0}, but found end of string")]
    UnexpectedEndOfString(&'static str),

    #[error(
        "Expected end of string, \
         but found {kind:?} at byte {byte_index}"
    )]
    ExpectedEndOfString { byte_index: usize, kind: TokenKind },

    #[error("Expected identifier at byte {byte_index}, but found {kind:?}")]
    ExpectedIdentifier { byte_index: usize, kind: TokenKind },

    #[error("Method {name} is not a supported operator")]
    UnknownOperator { name: String },

    #[error(
        "Expected single type argument, \
         but found {num_args} at byte index {span:?}"
    )]
    ExpectedSingleTypeArg { num_args: usize, span: Range<usize> },

    #[error(
        "Function call expected {expected} arguments, \
             but received {actual} arguments."
    )]
    UnexpectedNumberOfArguments { expected: usize, actual: usize },

    #[error("Expected {desc} at byte {byte_index}, but found {kind:?}")]
    UnexpectedTokenKind {
        desc: &'static str,
        byte_index: usize,
        kind: TokenKind,
    },

    #[error(
        "Expected primitive type argument to .prim_type, \
             but found {0}"
    )]
    ExpectedPrimType(SymbolicType),

    #[error(
        "Expected a variable definition for {0}, \
         but no such variable has been defined."
    )]
    ExpectedDefinedVariable(String),

    #[error("")]
    RangeExpressionsMustStartAtZero,
}

struct SymbolicTokenizer<'a> {
    text: &'a str,
    offset: usize,
    peek: Option<Token<'a>>,
}

struct Token<'a> {
    kind: TokenKind,
    text: &'a str,
    span: Range<usize>,
}

#[derive(Debug, Clone, Copy)]
pub enum TokenKind {
    Ident,
    Int(usize),
    Punct(Punctuation),
    Keyword(Keyword),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Punctuation {
    Period,
    DoublePeriod,
    Comma,
    LeftParen,
    RightParen,
    Colon,
    DoubleColon,
    LeftAngleBracket,
    RightAngleBracket,
    LeftAngleBracketEquals,
    RightAngleBracketEquals,
    LeftSquareBracket,
    RightSquareBracket,
    LeftBrace,
    RightBrace,
    Semicolon,
    SingleEquals,
    DoubleEquals,
    Bang,
    BangEquals,
    Plus,
    Multiply,
    Pipe,
    DoublePipe,
    Slash,
    Percent,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Let,
    Public,
    Function,
    If,
    Else,
    True,
    False,
    None,
}

#[derive(PartialOrd, PartialEq)]
enum OpPrecedence {
    MaybeTuple,
    TupleElement,
    ComparisonOperator,
    RangeExtent,
    Addition,
    MulDiv,
}

impl TokenKind {
    fn is_punct(&self, punct: Punctuation) -> bool {
        match self {
            Self::Punct(p) => *p == punct,
            _ => false,
        }
    }

    fn is_keyword(&self, keyword: Keyword) -> bool {
        match self {
            Self::Keyword(k) => *k == keyword,
            _ => false,
        }
    }
}

impl<'a> SymbolicParser<'a> {
    pub(crate) fn new(text: &'a str, graph: &'a mut SymbolicGraph) -> Self {
        let tokens = SymbolicTokenizer::new(text);
        let identifiers = graph
            .iter_ops()
            .filter_map(|(index, op)| {
                op.name.as_ref().map(|name| {
                    (name.clone().into(), SymbolicValue::Result(index))
                })
            })
            .collect();
        Self {
            tokens,
            identifiers,
            graph,
        }
    }

    pub fn parse_expr(&mut self) -> Result<SymbolicValue, Error> {
        self.tokens
            .peek()?
            .ok_or(ParseError::UnexpectedEndOfString("Start of expression"))?;
        let expr = self.expect_block_body()?;
        self.expect_end_of_string()?;

        Ok(expr)
    }

    /// Parse a block
    ///
    /// A block consists of an opening left brace '{', a block body,
    /// and a closing right brace '}'.
    fn expect_block(&mut self) -> Result<SymbolicValue, Error> {
        self.expect_punct("opening '{' of block", Punctuation::LeftBrace)?;

        let identifiers = self.identifiers.clone();
        let body = self.expect_block_body()?;
        self.identifiers = identifiers;

        self.expect_punct("closing '}' of block", Punctuation::RightBrace)?;

        Ok(body)
    }

    /// Parse the body of a block
    ///
    /// The body of a block consists of zero or more statements,
    /// followed by an expression.
    fn expect_block_body(&mut self) -> Result<SymbolicValue, Error> {
        let mut block_result = None;
        loop {
            let peek = self.tokens.peek()?.map(|token| token.kind);
            match peek {
                None | Some(TokenKind::Punct(Punctuation::RightBrace)) => {
                    break;
                }
                Some(TokenKind::Keyword(Keyword::Let)) => {
                    block_result = Some(self.expect_assignment()?);
                }
                Some(TokenKind::Keyword(
                    Keyword::Function | Keyword::Public,
                )) => {
                    block_result = Some(self.expect_named_function()?);
                }
                _ => {
                    block_result = Some(self.expect_expr()?);
                    if self
                        .tokens
                        .next_if(|token| {
                            token.kind.is_punct(Punctuation::Semicolon)
                        })?
                        .is_none()
                    {
                        break;
                    }
                }
            }
        }
        let block_result =
            block_result.unwrap_or_else(|| self.graph.tuple(vec![]));
        Ok(block_result)
    }

    fn expect_comma_separated_list<ItemParser, Item>(
        &mut self,
        closing_delimiter: Punctuation,
        item_parser: ItemParser,
    ) -> Result<Vec<Item>, Error>
    where
        ItemParser: Fn(&mut Self) -> Result<Item, Error>,
    {
        let mut output = Vec::new();
        loop {
            if self
                .tokens
                .peek()?
                .map(|peek| peek.kind.is_punct(closing_delimiter))
                .unwrap_or(false)
            {
                break;
            }

            output.push(item_parser(self)?);

            if self
                .tokens
                .next_if(|token| token.kind.is_punct(Punctuation::Comma))?
                .is_none()
            {
                break;
            }
        }
        Ok(output)
    }

    fn expect_expr(&mut self) -> Result<SymbolicValue, Error> {
        self.expect_expr_op_precedence(OpPrecedence::MaybeTuple)
    }

    fn expect_non_tuple_expr(&mut self) -> Result<SymbolicValue, Error> {
        self.expect_expr_op_precedence(OpPrecedence::TupleElement)
    }

    fn expect_expr_op_precedence(
        &mut self,
        precedence: OpPrecedence,
    ) -> Result<SymbolicValue, Error> {
        let mut expr = self.expect_term()?;

        if precedence < OpPrecedence::MulDiv {
            while let Some(token) = self.tokens.next_if(|token| {
                matches!(
                    token.kind,
                    TokenKind::Punct(
                        Punctuation::Multiply
                            | Punctuation::Slash
                            | Punctuation::Percent
                    )
                )
            })? {
                let rhs =
                    self.expect_expr_op_precedence(OpPrecedence::MulDiv)?;
                expr = match token.kind {
                    TokenKind::Punct(Punctuation::Multiply) => {
                        self.graph.mul(expr, rhs)
                    }
                    TokenKind::Punct(Punctuation::Slash) => {
                        self.graph.div(expr, rhs)
                    }
                    TokenKind::Punct(Punctuation::Percent) => {
                        self.graph.modulo(expr, rhs)
                    }
                    _ => unreachable!("Due to earlier check"),
                };
            }
        }

        if precedence < OpPrecedence::Addition {
            while let Some(_) = self
                .tokens
                .next_if(|token| token.kind.is_punct(Punctuation::Plus))?
            {
                let rhs =
                    self.expect_expr_op_precedence(OpPrecedence::Addition)?;
                expr = self.graph.add(expr, rhs);
            }
        }

        if precedence < OpPrecedence::ComparisonOperator {
            if let Some(token) = self.tokens.next_if(|token| {
                matches!(
                    token.kind,
                    TokenKind::Punct(
                        Punctuation::DoubleEquals
                            | Punctuation::BangEquals
                            | Punctuation::LeftAngleBracket
                            | Punctuation::RightAngleBracket
                            | Punctuation::LeftAngleBracketEquals
                            | Punctuation::RightAngleBracketEquals
                    )
                )
            })? {
                let rhs = self.expect_expr_op_precedence(
                    OpPrecedence::ComparisonOperator,
                )?;
                expr = match token.kind {
                    TokenKind::Punct(Punctuation::DoubleEquals) => {
                        self.graph.equal(expr, rhs)
                    }
                    TokenKind::Punct(Punctuation::BangEquals) => {
                        self.graph.not_equal(expr, rhs)
                    }
                    TokenKind::Punct(Punctuation::LeftAngleBracket) => {
                        self.graph.less_than(expr, rhs)
                    }
                    TokenKind::Punct(Punctuation::RightAngleBracket) => {
                        self.graph.greater_than(expr, rhs)
                    }
                    TokenKind::Punct(Punctuation::LeftAngleBracketEquals) => {
                        self.graph.less_than_or_equal(expr, rhs)
                    }
                    TokenKind::Punct(Punctuation::RightAngleBracketEquals) => {
                        self.graph.greater_than_or_equal(expr, rhs)
                    }
                    _ => unreachable!("Due to earlier check"),
                };
            }
        }

        if precedence < OpPrecedence::RangeExtent {
            if let Some(_) = self.tokens.next_if(|token| {
                token.kind.is_punct(Punctuation::DoublePeriod)
            })? {
                match expr {
                    SymbolicValue::Int(0) => {
                        // Ranges must all start with zero.
                    }
                    _ => {
                        return Err(
                            ParseError::RangeExpressionsMustStartAtZero.into(),
                        );
                    }
                }
                let extent =
                    self.expect_expr_op_precedence(OpPrecedence::RangeExtent)?;
                expr = self.graph.range(extent);
            }
        }

        if precedence < OpPrecedence::TupleElement
            && matches!(self.peek_punct()?, Some(Punctuation::Comma))
        {
            let mut elements = vec![expr];
            while let Some(_) = self
                .tokens
                .next_if(|token| token.kind.is_punct(Punctuation::Comma))?
            {
                if matches!(self.peek_punct()?, Some(Punctuation::RightParen)) {
                    // Early break, in case of trailing comma at the
                    // end of the tuple.
                    break;
                }

                let element =
                    self.expect_expr_op_precedence(OpPrecedence::TupleElement)?;
                elements.push(element);
            }
            expr = self.graph.tuple(elements)
        }

        Ok(expr)
    }

    fn expect_term(&mut self) -> Result<SymbolicValue, Error> {
        let peek_token = self
            .tokens
            .peek()?
            .ok_or(ParseError::UnexpectedEndOfString("expression"))?;
        let mut obj = match &peek_token.kind {
            TokenKind::Int(_) => self.expect_int(),
            TokenKind::Keyword(Keyword::True)
            | TokenKind::Keyword(Keyword::False) => self.expect_bool(),

            TokenKind::Keyword(Keyword::None) => self.expect_none(),

            TokenKind::Ident
                if self.identifiers.contains_key(peek_token.text) =>
            {
                // TODO: Implement a way to specify a static field, even if
                // the leading identifier would otherwise resolve to a
                // variable definition.  Maybe with a leading . to specify the
                // global scope?
                self.expect_previously_defined_var()
            }

            TokenKind::Ident => self.next_static_field(),

            TokenKind::Punct(Punctuation::LeftParen) => {
                self.tokens.next()?;
                let expr = self.expect_expr()?;
                self.expect_punct(
                    "Closing ')' of parenthesized expression",
                    Punctuation::RightParen,
                )?;
                Ok(expr)
            }

            TokenKind::Punct(Punctuation::LeftBrace) => self.expect_block(),

            TokenKind::Punct(Punctuation::Pipe | Punctuation::DoublePipe) => {
                self.expect_anonymous_function()
            }

            TokenKind::Keyword(Keyword::If) => self.expect_if_else(),

            _ => Err(ParseError::UnexpectedTokenKind {
                desc: "expression",
                byte_index: peek_token.span.start,
                kind: peek_token.kind,
            }
            .into()),
        }?;

        loop {
            if let Some(field) = self.try_field_name()? {
                obj = self.generate_field_access_or_operation(obj, field)?;
            } else if let Some(indices) = self.try_container_access()? {
                obj = self.graph.access_indices(obj, indices);
            } else if let Some(args) = self.try_function_arguments()? {
                obj = self.graph.function_call(obj, args);
            } else {
                break;
            }
        }

        Ok(obj)
    }

    pub(crate) fn expect_end_of_string(&mut self) -> Result<(), Error> {
        if let Some(token) = self.tokens.next()? {
            Err(ParseError::ExpectedEndOfString {
                kind: token.kind,
                byte_index: token.span.start,
            }
            .into())
        } else {
            Ok(())
        }
    }

    fn peek_punct(&mut self) -> Result<Option<Punctuation>, Error> {
        let opt_keyword = self
            .tokens
            .peek()?
            .map(|token| match token.kind {
                TokenKind::Punct(punct) => Some(punct),
                _ => None,
            })
            .flatten();

        Ok(opt_keyword)
    }

    fn expect_int(&mut self) -> Result<SymbolicValue, Error> {
        let token = self
            .expect_kind("integer", |kind| matches!(kind, TokenKind::Int(_)))?;

        let value = match token.kind {
            TokenKind::Int(value) => SymbolicValue::Int(value),
            _ => unreachable!("Handled by earlier check"),
        };

        Ok(value)
    }

    fn expect_none(&mut self) -> Result<SymbolicValue, Error> {
        self.expect_kind("none", |kind| {
            matches!(kind, TokenKind::Keyword(Keyword::None))
        })?;

        Ok(self.graph.none())
    }

    fn expect_bool(&mut self) -> Result<SymbolicValue, Error> {
        let token = self.expect_kind("boolean", |kind| {
            matches!(
                kind,
                TokenKind::Keyword(Keyword::True)
                    | TokenKind::Keyword(Keyword::False)
            )
        })?;

        let value = match token.kind {
            TokenKind::Keyword(Keyword::True) => SymbolicValue::Bool(true),
            TokenKind::Keyword(Keyword::False) => SymbolicValue::Bool(false),
            _ => unreachable!("Handled by earlier check"),
        };

        Ok(value)
    }

    fn expect_if_else(&mut self) -> Result<SymbolicValue, Error> {
        self.expect_keyword(
            "'if' at start of if/else expressions",
            Keyword::If,
        )?;

        let condition = self.expect_expr()?;
        let if_branch = self.expect_block()?;

        self.expect_keyword(
            "'else' after the end of 'if' block",
            Keyword::Else,
        )?;

        let else_branch = self.expect_block()?;

        let if_else = self.graph.if_else(condition, if_branch, else_branch);
        Ok(if_else)
    }

    fn define_identifier(
        &mut self,
        name: &'a str,
        value: SymbolicValue,
    ) -> Result<(), Error> {
        let name = name.into();

        if SymbolicGraph::is_reserved_name(name) {
            // The variable name is a reserved name.  It may be an
            // anonymous placeholder, or it may be an actual name,
            // prefixed by `_{index}_` to avoid ambiguity.

            let mut char_iter = name.char_indices().peekable();
            char_iter.next();
            while let Some(_) = char_iter.next_if(|(_, c)| c.is_ascii_digit()) {
            }
            char_iter.next();

            if let Some((char_index, _)) = char_iter.next() {
                self.graph.name(value, &name[char_index..])?;
            }
        } else {
            self.graph.name(value, name)?;
        }

        self.identifiers.insert(Cow::Borrowed(name), value);

        Ok(())
    }

    fn expect_assignment(&mut self) -> Result<SymbolicValue, Error> {
        self.expect_keyword(
            "'let' at start of variable binding",
            Keyword::Let,
        )?;
        let var_name = self.expect_ident("variable name")?.text;
        self.expect_punct(
            "'=' after variable name in assignment",
            Punctuation::SingleEquals,
        )?;
        let expr = self.expect_expr()?;
        self.expect_punct(
            "';' after variable assignment",
            Punctuation::Semicolon,
        )?;

        self.define_identifier(var_name, expr)?;

        Ok(expr)
    }

    /// Parses a function definition at the point
    ///
    /// If the function is named, then it gets treated as a Let
    /// binding, and `expect_function` returns `None`.  If the
    /// function is anonymous, then it gets treated as an expression,
    /// and this function returns `Some(func)`.
    fn expect_named_function(&mut self) -> Result<SymbolicValue, Error> {
        let identifiers = self.identifiers.clone();

        let is_extern = self
            .tokens
            .next_if(|token| token.kind.is_keyword(Keyword::Public))?
            .is_some();

        self.expect_keyword(
            "'fn' keyword at start of function",
            Keyword::Function,
        )?;

        let name = self.expect_ident("Function name")?.text;

        self.expect_punct(
            "'(' to start function arguments",
            Punctuation::LeftParen,
        )?;

        let params = self
            .expect_comma_separated_list(Punctuation::RightParen, |parser| {
                parser.expect_function_param()
            })?;

        self.expect_punct(
            "closing ')' of function parameter list",
            Punctuation::RightParen,
        )?;

        let output = self.expect_block()?;

        let func = self.graph.function_def(params, output);
        self.graph.name(func, name)?;
        if is_extern {
            self.graph.mark_extern_func(func)?;
        }

        self.identifiers = identifiers;

        self.define_identifier(name, func)?;
        Ok(func)
    }

    fn expect_anonymous_function(&mut self) -> Result<SymbolicValue, Error> {
        let identifiers = self.identifiers.clone();
        let opening = self.expect_kind(
            "Opening '|' of closure's argument list",
            |kind| {
                kind.is_punct(Punctuation::Pipe)
                    || kind.is_punct(Punctuation::DoublePipe)
            },
        )?;

        let params = match opening.kind {
            TokenKind::Punct(Punctuation::Pipe) => {
                let params = self.expect_comma_separated_list(
                    Punctuation::Pipe,
                    |parser| parser.expect_function_param(),
                )?;

                self.expect_punct(
                    "Closing '|' of closure's argument list",
                    Punctuation::Pipe,
                )?;
                params
            }
            TokenKind::Punct(Punctuation::DoublePipe) => {
                vec![]
            }
            _ => unreachable!("Protected by earlier `expect_kind()`"),
        };

        let output = self.expect_expr()?;

        let func = self.graph.function_def(params, output);

        self.identifiers = identifiers;

        Ok(func)
    }

    fn expect_function_param(&mut self) -> Result<SymbolicValue, Error> {
        let param_name = self.expect_ident("function parameter")?.text;

        let ty: RuntimeType = if self
            .tokens
            .next_if(|token| token.kind.is_punct(Punctuation::Colon))?
            .is_some()
        {
            let symbolic_type = self.expect_type()?;
            let ty = symbolic_type
                .try_prim_type()
                .expect("TODO: Non-primitive params");
            ty.into()
        } else {
            RuntimeType::Unknown
        };

        let arg = self.graph.function_arg(ty);
        self.graph.name(arg, param_name)?;
        self.identifiers.insert(Cow::Borrowed(param_name), arg);
        Ok(arg)
    }

    fn next_static_field(&mut self) -> Result<SymbolicValue, Error> {
        let class = self.expect_ident("class and static field")?.text;
        self.expect_punct(
            "'.' between class and static field name",
            Punctuation::Period,
        )?;
        let field_name = self.expect_ident("static field name")?.text;

        Ok(self.graph.static_field(class, field_name))
    }

    fn try_field_name(&mut self) -> Result<Option<Token<'a>>, Error> {
        if self
            .tokens
            .next_if(|token| token.kind.is_punct(Punctuation::Period))?
            .is_none()
        {
            return Ok(None);
        }

        let ident = self.expect_ident("identifier after period")?;

        Ok(Some(ident))
    }

    fn generate_field_access_or_operation(
        &mut self,
        obj: SymbolicValue,
        field: Token<'a>,
    ) -> Result<SymbolicValue, Error> {
        let is_operation = self
            .tokens
            .peek()?
            .map(|peek| {
                peek.kind.is_punct(Punctuation::DoubleColon)
                    || peek.kind.is_punct(Punctuation::LeftParen)
            })
            .unwrap_or(false);

        if is_operation {
            match field.text {
                "as" | "prim_cast" => {
                    let (type_args, _) =
                        self.expect_function_arguments(1, 0)?;
                    let ty = type_args
                        .into_iter()
                        .next()
                        .expect("Protected by length check");
                    let expr = if field.text == "prim_cast" {
                        let prim_type = ty
                            .try_prim_type()
                            .ok_or_else(|| ParseError::ExpectedPrimType(ty))?;
                        self.graph.prim_cast(obj, prim_type)
                    } else if let Some(prim_type) = ty.try_prim_type() {
                        self.graph.prim_cast(obj, prim_type)
                    } else {
                        self.graph.downcast(obj, ty)
                    };
                    Ok(expr)
                }
                "len" => {
                    self.expect_function_arguments(0, 0)?;
                    Ok(self.graph.num_array_elements(obj))
                }
                "extent" => {
                    let (_, args) = self.expect_function_arguments(0, 1)?;
                    let dim = args
                        .into_iter()
                        .next()
                        .expect("Protected by length check");
                    Ok(self.graph.array_extent(obj, dim))
                }
                "map" => {
                    let (_, args) = self.expect_function_arguments(0, 1)?;
                    let map = args
                        .into_iter()
                        .exactly_one()
                        .expect("Protected by length check");
                    Ok(self.graph.map(obj, map))
                }
                "filter" => {
                    let (_, args) = self.expect_function_arguments(0, 1)?;
                    let filter = args
                        .into_iter()
                        .exactly_one()
                        .expect("Protected by length check");
                    Ok(self.graph.filter(obj, filter))
                }
                "collect" => {
                    let _ = self.expect_function_arguments(0, 0)?;
                    Ok(self.graph.collect(obj))
                }
                "reduce" => {
                    let (_, args) = self.expect_function_arguments(0, 2)?;
                    let (initial, reduction) = args
                        .into_iter()
                        .tuples()
                        .exactly_one()
                        .expect("Protected by length check");
                    Ok(self.graph.reduce(initial, obj, reduction))
                }
                "is_some" => {
                    self.expect_function_arguments(0, 0)?;
                    Ok(self.graph.is_some(obj))
                }
                "read" => {
                    let (type_args, _) =
                        self.expect_function_arguments(1, 0)?;
                    let ty = type_args
                        .into_iter()
                        .next()
                        .expect("Protected by length check");
                    let prim_type = ty
                        .try_prim_type()
                        .ok_or_else(|| ParseError::ExpectedPrimType(ty))?;
                    Ok(self.graph.read_value(obj, prim_type))
                }
                "read_string" => {
                    self.expect_function_arguments(0, 0)?;
                    Ok(self.graph.read_string(obj))
                }
                _ => Err(ParseError::UnknownOperator {
                    name: field.text.to_string(),
                }
                .into()),
            }
        } else {
            Ok(self.graph.access_field(obj, field.text.to_string()))
        }
    }

    fn expect_function_arguments(
        &mut self,
        num_type_args: usize,
        num_args: usize,
    ) -> Result<(Vec<SymbolicType>, Vec<SymbolicValue>), Error> {
        let mut type_args = Vec::new();
        if num_type_args > 0 {
            self.expect_punct(
                "'::<' to start list of type arguments",
                Punctuation::DoubleColon,
            )?;

            self.expect_punct(
                "'::<' to start list of type arguments",
                Punctuation::LeftAngleBracket,
            )?;

            for _ in 0..num_type_args {
                type_args.push(self.expect_type()?);
            }

            self.expect_punct(
                "'>' to close list of type arguments",
                Punctuation::RightAngleBracket,
            )?;
        }

        self.expect_punct(
            "left '(' to start method arguments",
            Punctuation::LeftParen,
        )?;
        let args = self
            .expect_comma_separated_list(Punctuation::RightParen, |parser| {
                parser.expect_non_tuple_expr()
            })?;
        self.expect_punct(
            "right ')' to finish method arguments",
            Punctuation::RightParen,
        )?;

        if args.len() != num_args {
            return Err(ParseError::UnexpectedNumberOfArguments {
                expected: num_args,
                actual: args.len(),
            }
            .into());
        }

        Ok((type_args, args))
    }

    fn try_container_access(
        &mut self,
    ) -> Result<Option<Vec<SymbolicValue>>, Error> {
        if self
            .tokens
            .next_if(|token| {
                token.kind.is_punct(Punctuation::LeftSquareBracket)
            })?
            .is_none()
        {
            return Ok(None);
        }

        let indices = self.expect_comma_separated_list(
            Punctuation::RightSquareBracket,
            |parser| parser.expect_non_tuple_expr(),
        )?;

        self.expect_punct(
            "closing ] of index",
            Punctuation::RightSquareBracket,
        )?;

        Ok(Some(indices))
    }

    fn try_function_arguments(
        &mut self,
    ) -> Result<Option<Vec<SymbolicValue>>, Error> {
        if self
            .tokens
            .next_if(|token| token.kind.is_punct(Punctuation::LeftParen))?
            .is_none()
        {
            return Ok(None);
        }

        let indices = self
            .expect_comma_separated_list(Punctuation::RightParen, |parser| {
                parser.expect_non_tuple_expr()
            })?;

        self.expect_punct(
            "closing ')' of function arguments",
            Punctuation::RightParen,
        )?;

        Ok(Some(indices))
    }

    fn expect_ident(&mut self, desc: &'static str) -> Result<Token<'a>, Error> {
        self.expect_kind(desc, |kind| matches!(kind, TokenKind::Ident))
    }

    fn expect_previously_defined_var(
        &mut self,
    ) -> Result<SymbolicValue, Error> {
        let var_name = self.expect_ident("variable name")?.text;
        let value = self.identifiers.get(var_name).ok_or_else(|| {
            ParseError::ExpectedDefinedVariable(var_name.into())
        })?;
        Ok(*value)
    }

    fn expect_keyword(
        &mut self,
        desc: &'static str,
        expected_keyword: Keyword,
    ) -> Result<Token<'a>, Error> {
        self.expect_kind(desc, |kind| {
            if let TokenKind::Keyword(token_keyword) = kind {
                expected_keyword == *token_keyword
            } else {
                false
            }
        })
    }

    fn expect_punct(
        &mut self,
        desc: &'static str,
        punct: Punctuation,
    ) -> Result<Token<'a>, Error> {
        self.expect_kind(desc, |kind| kind.is_punct(punct))
    }

    fn expect_kind(
        &mut self,
        desc: &'static str,
        pred: impl FnOnce(&TokenKind) -> bool,
    ) -> Result<Token<'a>, Error> {
        let token = self
            .tokens
            .next()?
            .ok_or_else(|| ParseError::UnexpectedEndOfString(desc))?;

        if pred(&token.kind) {
            Ok(token)
        } else {
            Err(ParseError::UnexpectedTokenKind {
                desc,
                byte_index: token.span.start,
                kind: token.kind,
            }
            .into())
        }
    }

    fn expect_type(&mut self) -> Result<SymbolicType, Error> {
        let mut namespace = Vec::new();
        let mut name = self.expect_ident("namespace and name of class")?.text;

        while let Some(_) = self
            .tokens
            .next_if(|token| token.kind.is_punct(Punctuation::Period))?
        {
            namespace.push(name);
            name = self.expect_ident("namespace and name of class")?.text;
        }

        let generics = self.try_generic_type_list()?;

        let full_name =
            namespace.into_iter().chain(std::iter::once(name)).join(".");

        Ok(SymbolicType {
            full_name,
            generics,
        })
    }

    fn try_generic_type_list(&mut self) -> Result<Vec<SymbolicType>, Error> {
        if self
            .tokens
            .peek()?
            .filter(|token| token.kind.is_punct(Punctuation::LeftAngleBracket))
            .is_some()
        {
            self.expect_generic_type_list()
        } else {
            Ok(Vec::new())
        }
    }

    fn expect_generic_type_list(&mut self) -> Result<Vec<SymbolicType>, Error> {
        self.expect_punct(
            "Opening '<' in list of type arguments",
            Punctuation::LeftAngleBracket,
        )?;

        let generic_types = self.expect_comma_separated_list(
            Punctuation::RightAngleBracket,
            |parser| parser.expect_type(),
        )?;

        self.expect_punct(
            "Closing '>' in list of type arguments",
            Punctuation::RightAngleBracket,
        )?;

        Ok(generic_types)
    }
}

impl Keyword {
    fn from_string(ident: &str) -> Option<Self> {
        match ident {
            "let" => Some(Keyword::Let),
            "pub" => Some(Keyword::Public),
            "fn" => Some(Keyword::Function),
            "if" => Some(Keyword::If),
            "else" => Some(Keyword::Else),
            "true" => Some(Keyword::True),
            "false" => Some(Keyword::False),
            "None" => Some(Keyword::None),
            _ => None,
        }
    }
}

impl<'a> SymbolicTokenizer<'a> {
    fn new(text: &'a str) -> Self {
        Self {
            text,
            offset: 0,
            peek: None,
        }
    }

    fn next(&mut self) -> Result<Option<Token<'a>>, Error> {
        self.fill_next()?;
        Ok(self.peek.take())
    }

    fn next_if(
        &mut self,
        func: impl FnOnce(&Token<'a>) -> bool,
    ) -> Result<Option<Token<'a>>, Error> {
        if self.peek()?.filter(|token| func(token)).is_some() {
            self.next()
        } else {
            Ok(None)
        }
    }

    fn peek(&mut self) -> Result<Option<&Token<'a>>, Error> {
        self.fill_next()?;
        Ok(self.peek.as_ref())
    }

    fn fill_next(&mut self) -> Result<(), Error> {
        if self.peek.is_some() {
            return Ok(());
        }

        loop {
            let offset_before_skip = self.offset;
            self.skip_whitespace();
            self.skip_comment();
            if self.offset == offset_before_skip {
                break;
            }
        }

        let start = self.offset;
        if start == self.text.len() {
            return Ok(());
        }

        let (char1, opt_char2) = {
            let mut char_iter = self.text[start..].chars();
            let char1 =
                char_iter.next().expect("End-of-string already handled");
            let opt_char2 = char_iter.next();
            (char1, opt_char2)
        };
        let mut num_bytes = self.text[start..]
            .char_indices()
            .skip(1)
            .next()
            .map(|(i, _)| i)
            .unwrap_or_else(|| self.text.len() - start);

        let kind = match char1 {
            '.' if opt_char2 == Some('.') => {
                num_bytes = 2;
                TokenKind::Punct(Punctuation::DoublePeriod)
            }
            '.' => TokenKind::Punct(Punctuation::Period),
            ',' => TokenKind::Punct(Punctuation::Comma),
            '(' => TokenKind::Punct(Punctuation::LeftParen),
            ')' => TokenKind::Punct(Punctuation::RightParen),
            '<' if opt_char2 == Some('=') => {
                num_bytes = 2;
                TokenKind::Punct(Punctuation::LeftAngleBracketEquals)
            }
            '<' => TokenKind::Punct(Punctuation::LeftAngleBracket),
            '>' if opt_char2 == Some('=') => {
                num_bytes = 2;
                TokenKind::Punct(Punctuation::RightAngleBracketEquals)
            }
            '>' => TokenKind::Punct(Punctuation::RightAngleBracket),
            '[' => TokenKind::Punct(Punctuation::LeftSquareBracket),
            ']' => TokenKind::Punct(Punctuation::RightSquareBracket),
            '{' => TokenKind::Punct(Punctuation::LeftBrace),
            '}' => TokenKind::Punct(Punctuation::RightBrace),
            ';' => TokenKind::Punct(Punctuation::Semicolon),
            '=' if opt_char2 == Some('=') => {
                num_bytes = 2;
                TokenKind::Punct(Punctuation::DoubleEquals)
            }
            '=' => TokenKind::Punct(Punctuation::SingleEquals),
            '!' if opt_char2 == Some('=') => {
                num_bytes = 2;
                TokenKind::Punct(Punctuation::BangEquals)
            }
            '!' => TokenKind::Punct(Punctuation::Bang),
            '+' => TokenKind::Punct(Punctuation::Plus),
            '*' => TokenKind::Punct(Punctuation::Multiply),
            ':' if opt_char2 == Some(':') => {
                num_bytes = 2;
                TokenKind::Punct(Punctuation::DoubleColon)
            }
            ':' => TokenKind::Punct(Punctuation::Colon),
            '|' if opt_char2 == Some('|') => {
                num_bytes = 2;
                TokenKind::Punct(Punctuation::DoublePipe)
            }
            '|' => TokenKind::Punct(Punctuation::Pipe),
            '/' => TokenKind::Punct(Punctuation::Slash),
            '%' => TokenKind::Punct(Punctuation::Percent),
            '0'..='9' => {
                let mut value = 0;
                let mut index = None;

                for (i, c) in self.text[start..].char_indices() {
                    if c.is_ascii_digit() {
                        let digit = c as usize - '0' as usize;
                        value = value * 10 + digit;
                    } else {
                        index = Some(i);
                        break;
                    }
                }
                num_bytes = index.unwrap_or_else(|| self.text.len() - start);

                TokenKind::Int(value)
            }
            '_' | 'a'..='z' | 'A'..='Z' => {
                num_bytes = self.text[start..]
                    .char_indices()
                    .skip(1)
                    .find(|(_, c)| {
                        !matches!(
                        c, '_'|'a'..='z'|'A'..='Z'|'0'..='9'|'`'
                        )
                    })
                    .map(|(i, _)| i)
                    .unwrap_or_else(|| self.text.len() - start);

                Keyword::from_string(&self.text[start..start + num_bytes])
                    .map(|keyword| TokenKind::Keyword(keyword))
                    .unwrap_or(TokenKind::Ident)
            }
            _ => {
                return Err(ParseError::UnexpectedChar {
                    c: char1,
                    byte_index: start,
                }
                .into());
            }
        };

        let end = start + num_bytes;
        self.peek = Some(Token {
            text: &self.text.get(start..end).unwrap_or_else(|| {
                panic!(
                    "Token {kind:?} at indices {start}..{end}, \
                     but these are out of range \
                     for string of length {}.  \
                     num_bytes = {num_bytes}",
                    self.text.len(),
                )
            }),
            kind,
            span: start..end,
        });
        self.offset = end;

        Ok(())
    }

    fn skip_whitespace(&mut self) {
        self.offset = self.text[self.offset..]
            .char_indices()
            .find(|&(_, c)| !c.is_whitespace() && c != '\u{200b}')
            .map(|(i, _)| self.offset + i)
            .unwrap_or_else(|| self.text.len());
    }

    fn skip_comment(&mut self) {
        let is_comment = self.text[self.offset..]
            .chars()
            .take(2)
            .filter(|&c| c == '/')
            .count()
            == 2;
        if is_comment {
            self.offset = self.text[self.offset..]
                .char_indices()
                .skip_while(|&(_, c)| c != '\n')
                .skip(1)
                .next()
                .map(|(i, _)| self.offset + i)
                .unwrap_or_else(|| self.text.len());
        }
    }
}

impl std::fmt::Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
