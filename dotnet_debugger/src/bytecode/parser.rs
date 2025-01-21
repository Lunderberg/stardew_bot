use std::ops::Range;

use itertools::Itertools as _;
use thiserror::Error;

use super::expr::{SymbolicGraph, SymbolicType, SymbolicValue};
use crate::Error;

pub(crate) struct SymbolicParser<'a> {
    tokens: SymbolicTokenizer<'a>,
    graph: &'a mut SymbolicGraph,
}

/// Errors in parsing the symbolic chain.  There are enough of these
/// error that they are collected here to avoid
#[derive(Error)]
pub enum ParseError {
    #[error("Unexpected {c:?} at byte {byte_index}")]
    UnexpectedChar { c: char, byte_index: usize },

    #[error(
        "Expected {expected:?} at byte {byte_index},\
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

#[derive(Debug)]
pub enum TokenKind {
    Ident,
    Int(usize),
    Punct(Punctuation),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Punctuation {
    Period,
    Comma,
    LeftParen,
    RightParen,
    DoubleColon,
    LeftAngleBracket,
    RightAngleBracket,
    LeftSquareBracket,
    RightSquareBracket,
}

impl TokenKind {
    fn is_punct(&self, punct: Punctuation) -> bool {
        match self {
            Self::Punct(p) => *p == punct,
            _ => false,
        }
    }
}

impl<'a> SymbolicParser<'a> {
    pub(crate) fn new(text: &'a str, graph: &'a mut SymbolicGraph) -> Self {
        let tokens = SymbolicTokenizer::new(text);
        Self { tokens, graph }
    }

    pub fn parse_expr(&mut self) -> Result<SymbolicValue, Error> {
        let expr = self.next_expr()?;
        self.expect_end_of_string()?;
        Ok(expr)
    }

    fn next_expr(&mut self) -> Result<SymbolicValue, Error> {
        if let Some(value) = self.try_int()? {
            return Ok(SymbolicValue::Int(value));
        }

        let mut obj = self.next_static_field()?;

        loop {
            if let Some(field) = self.try_field_name()? {
                obj = self.generate_field_access_or_operation(obj, field)?;
            } else if let Some(indices) = self.try_container_access()? {
                obj = self.graph.access_indices(obj, indices);
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

    fn try_int(&mut self) -> Result<Option<usize>, Error> {
        let opt_index = self
            .tokens
            .next_if(|token| matches!(token.kind, TokenKind::Int(_)))?
            .map(|token| match token.kind {
                TokenKind::Int(value) => value,
                _ => unreachable!("Handled by earlier check"),
            });

        Ok(opt_index)
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
                    let _ = self.expect_function_arguments(0, 0)?;
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

        let mut args = Vec::new();
        self.expect_punct(
            "left ( to start method arguments",
            Punctuation::LeftParen,
        )?;
        for _ in 0..num_args {
            args.push(self.next_expr()?);
        }
        self.expect_punct(
            "right ( to finish method arguments",
            Punctuation::RightParen,
        )?;

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

        let mut indices = Vec::new();

        loop {
            // Closing bracket without a trailing comma
            if self
                .tokens
                .peek()?
                .map(|peek| peek.kind.is_punct(Punctuation::RightSquareBracket))
                .unwrap_or(false)
            {
                break;
            }

            indices.push(self.next_expr()?);

            if self
                .tokens
                .next_if(|token| token.kind.is_punct(Punctuation::Comma))?
                .is_none()
            {
                break;
            }
        }

        self.expect_punct(
            "closing ] of index",
            Punctuation::RightSquareBracket,
        )?;

        Ok(Some(indices))
    }

    fn expect_ident(&mut self, desc: &'static str) -> Result<Token<'a>, Error> {
        self.expect_kind(desc, |kind| matches!(kind, TokenKind::Ident))
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
                .map(|(generics, _)| generics)
        } else {
            Ok(Vec::new())
        }
    }

    fn expect_generic_type_list(
        &mut self,
    ) -> Result<(Vec<SymbolicType>, Range<usize>), Error> {
        let start = self
            .expect_punct(
                "'<' to start list of type arguments",
                Punctuation::LeftAngleBracket,
            )?
            .span
            .start;

        let mut generic_types = Vec::new();
        loop {
            if let Some(close) = self.tokens.next_if(|token| {
                token.kind.is_punct(Punctuation::RightAngleBracket)
            })? {
                // Break here handles empty list, and case where there
                // has been a trailing comma after the last type
                // argument.
                return Ok((generic_types, start..close.span.end));
            }

            generic_types.push(self.expect_type()?);

            let token = self.tokens.next()?.ok_or(
                ParseError::UnexpectedEndOfString(
                    "closing '>' of type arguments",
                ),
            )?;

            if token.kind.is_punct(Punctuation::RightAngleBracket) {
                // Break here handles case where the last type
                // argument has no trailing comma.
                return Ok((generic_types, start..token.span.end));
            } else if token.kind.is_punct(Punctuation::Comma) {
                // Do nothing, next loop will break if this is a
                // trailing comma.
            } else {
                return Err(ParseError::UnexpectedTokenKind {
                    desc: "comma or closing '>' of type arguments",
                    kind: token.kind,
                    byte_index: token.span.start,
                }
                .into());
            }
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

        self.skip_whitespace();

        let start = self.offset;
        if start == self.text.len() {
            return Ok(());
        }

        let c = self.text[start..]
            .chars()
            .next()
            .expect("End-of-string already handled");
        let mut num_bytes = self.text[start..]
            .char_indices()
            .skip(1)
            .next()
            .map(|(i, _)| i)
            .unwrap_or_else(|| self.text.len() - start);

        let kind = match c {
            '.' => TokenKind::Punct(Punctuation::Period),
            ',' => TokenKind::Punct(Punctuation::Comma),
            '(' => TokenKind::Punct(Punctuation::LeftParen),
            ')' => TokenKind::Punct(Punctuation::RightParen),
            '<' => TokenKind::Punct(Punctuation::LeftAngleBracket),
            '>' => TokenKind::Punct(Punctuation::RightAngleBracket),
            '[' => TokenKind::Punct(Punctuation::LeftSquareBracket),
            ']' => TokenKind::Punct(Punctuation::RightSquareBracket),
            ':' => {
                let (next_loc, next) = self.text[start..]
                    .char_indices()
                    .skip(1)
                    .next()
                    .ok_or_else(|| {
                        ParseError::UnexpectedEndOfString(
                            "Second ':' of '::' token",
                        )
                    })?;
                if next != ':' {
                    return Err(ParseError::UnexpectedCharWithKnown {
                        expected: ':',
                        actual: next,
                        byte_index: next_loc,
                    }
                    .into());
                }
                num_bytes = 2;
                TokenKind::Punct(Punctuation::DoubleColon)
            }
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
                TokenKind::Ident
            }
            _ => {
                return Err(ParseError::UnexpectedChar {
                    c,
                    byte_index: start,
                }
                .into());
            }
        };

        let end = start + num_bytes;
        self.peek = Some(Token {
            // kind,
            //text: &self.text[start..end],
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
        let remaining = &self.text[self.offset..];
        let new_offset = remaining
            .char_indices()
            .find(|&(_, c)| !c.is_whitespace() && c != '\u{200b}')
            .map(|(i, _)| i);

        if let Some(new_offset) = new_offset {
            self.offset += new_offset;
        } else {
            self.offset = self.text.len();
        }
    }
}

impl std::fmt::Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self)
    }
}
