use std::ops::Range;

use itertools::Itertools as _;
use thiserror::Error;

use crate::{
    CachedReader, Error, SymbolicAccessChain, SymbolicOperation,
    SymbolicStaticField, SymbolicType,
};

pub(crate) struct SymbolicParser<'a> {
    tokens: SymbolicTokenizer<'a>,
    reader: CachedReader<'a>,
}

/// Errors in parsing the symbolic chain.  There are enough of these
/// error that they are collected here to avoid
#[derive(Error)]
pub enum ParseError {
    #[error("Unexpected {c:?} at byte {byte_index}")]
    UnexpectedChar { c: char, byte_index: usize },

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

    #[error("Must start with a static field, but found {0}")]
    ExpectedStaticField(SymbolicOperation),
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
    pub(crate) fn new(text: &'a str, reader: CachedReader<'a>) -> Self {
        let tokens = SymbolicTokenizer::new(text);
        Self { tokens, reader }
    }

    pub(crate) fn next_chain(&mut self) -> Result<SymbolicAccessChain, Error> {
        let static_field = self.next_static_field()?;
        let mut ops = Vec::new();

        while let Some(op) = self.next_op()? {
            ops.push(op);
        }

        self.expect_end_of_string()?;

        Ok(SymbolicAccessChain { static_field, ops })
    }

    fn expect_end_of_string(&mut self) -> Result<(), Error> {
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

    fn next_static_field(&mut self) -> Result<SymbolicStaticField, Error> {
        let class = self.leading_type()?;
        let first_op = self
            .next_op()?
            .ok_or(ParseError::UnexpectedEndOfString("static field name"))?;
        let field_name = match first_op {
            SymbolicOperation::Field(field_name) => Ok(field_name),
            other => Err(ParseError::ExpectedStaticField(other)),
        }?;

        Ok(SymbolicStaticField { class, field_name })
    }

    fn leading_type(&mut self) -> Result<SymbolicType, Error> {
        // The display of static fields is done similar to C#, which
        // uses the same syntax for specifying static fields, and for
        // specifying
        //
        // For example, `A.B.C.D` could be class `B` within namespace
        // `A`, which has a static field `C`, and a subfield `D`.
        // Alternatively, it could be class `C` within namespace
        // `A.B`, which has a static field `C`.
        //
        // Within the CLR, this ambiguity is handled by explicitly
        // specifying both the name and the namespace as separate
        // fields, but the printed-out format (and C# itself) is
        // ambiguous without additional information.
        let mut namespace = Vec::new();

        loop {
            let name_token = self.expect_ident("class and static field")?;
            let name = name_token.text;

            if self.is_valid_class(&namespace, name)? {
                return Ok(SymbolicType {
                    namespace: (!namespace.is_empty())
                        .then(|| namespace.iter().join(".")),
                    name: name.to_string(),
                    generics: Vec::new(),
                });
            }

            namespace.push(name);
            self.expect_punct(
                "period separating namespace/name of static field",
                Punctuation::Period,
            )?;
        }
    }

    fn is_valid_class(
        &self,
        namespace: &[&str],
        name: &str,
    ) -> Result<bool, Error> {
        // Because the symbolic access chain may be written with
        // spaces between the components of a namespace, we cannot
        // just compare the namespace of a slice with the metadata's
        // namespace.
        //
        // Could avoid the allocation/copy with a custom comparison,
        // but I don't expect this to be a bottleneck.
        let namespace = namespace.iter().join(".");

        for res_module_ptr in self.reader.iter_known_modules() {
            let module_ptr = res_module_ptr?;
            let module = self.reader.runtime_module(module_ptr)?;
            let metadata = module.metadata(self.reader)?;
            for row in metadata.type_def_table().iter_rows() {
                let row_namespace = row.namespace()?;
                let row_name = row.name()?;

                if row_namespace == namespace && row_name == name {
                    return Ok(true);
                }
            }
        }

        Ok(false)
    }

    fn next_op(&mut self) -> Result<Option<SymbolicOperation>, Error> {
        let Some(token) = self.tokens.next_if(|token| {
            token.kind.is_punct(Punctuation::Period)
                || token.kind.is_punct(Punctuation::LeftSquareBracket)
        })?
        else {
            return Ok(None);
        };

        let symbolic_operation = match token.kind {
            TokenKind::Punct(Punctuation::Period) => {
                self.expect_field_or_method()?
            }
            TokenKind::Punct(Punctuation::LeftSquareBracket) => {
                let token_index = self.expect_int("integer index")?;
                let TokenKind::Int(index) = token_index.kind else {
                    unreachable!("Already checked for integer in expect_int")
                };
                self.expect_punct(
                    "closing ] of index",
                    Punctuation::RightSquareBracket,
                )?;
                SymbolicOperation::IndexAccess(index)
            }
            _ => unreachable!("Must be . or [ at this point"),
        };

        Ok(Some(symbolic_operation))
    }

    fn expect_field_or_method(&mut self) -> Result<SymbolicOperation, Error> {
        let token = self.expect_ident("identifier after period")?;

        let is_special_operation = self
            .tokens
            .peek()?
            .map(|peek| {
                peek.kind.is_punct(Punctuation::LeftAngleBracket)
                    || peek.kind.is_punct(Punctuation::LeftParen)
            })
            .unwrap_or(false);

        if is_special_operation {
            let symbolic_operation = match token.text {
                "as" => {
                    let (generic_types, span) =
                        self.expect_generic_type_list()?;
                    if generic_types.len() == 1 {
                        Ok(SymbolicOperation::Downcast(
                            generic_types
                                .into_iter()
                                .next()
                                .expect("Protected by length check"),
                        ))
                    } else {
                        Err(ParseError::ExpectedSingleTypeArg {
                            num_args: generic_types.len(),
                            span,
                        })
                    }
                }
                _ => Err(ParseError::UnknownOperator {
                    name: token.text.into(),
                }),
            }?;
            self.expect_punct(
                "left ( to start method arguments",
                Punctuation::LeftParen,
            )?;
            self.expect_punct(
                "right ( to finish method arguments",
                Punctuation::RightParen,
            )?;

            Ok(symbolic_operation)
        } else {
            Ok(SymbolicOperation::Field(token.text.to_string()))
        }
    }

    fn expect_ident(&mut self, desc: &'static str) -> Result<Token<'a>, Error> {
        self.expect_kind(desc, |kind| matches!(kind, TokenKind::Ident))
    }

    fn expect_int(&mut self, desc: &'static str) -> Result<Token<'a>, Error> {
        self.expect_kind(desc, |kind| matches!(kind, TokenKind::Int(_)))
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

        Ok(SymbolicType {
            namespace: (!namespace.is_empty())
                .then(|| namespace.iter().join(".")),
            name: name.to_string(),
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
