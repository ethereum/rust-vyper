#![allow(unused_variables, dead_code, unused_imports)]

pub use fe_common::diagnostics::Label;
use fe_common::diagnostics::{Diagnostic, Severity};
use fe_common::files::SourceFileId;

use crate::ast::Module;
use crate::lexer::{Lexer, Token, TokenKind};
use crate::node::Span;
use std::{error, fmt};

#[derive(Debug)]
pub struct ParseFailed;
impl fmt::Display for ParseFailed {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(fmt, "ParseFailed")
    }
}
impl error::Error for ParseFailed {}

pub type ParseResult<T> = Result<T, ParseFailed>;

/// `Parser` maintains the parsing state, such as the token stream,
/// indent stack, paren stack, diagnostics, etc.
/// Syntax parsing logic is in the [`crate::grammar`] module.
///
/// See [`BTParser`] if you need backtrackable parser.
pub struct Parser<'a> {
    lexer: Lexer<'a>,

    /// Tokens that have been "peeked", or split from a larger token.
    /// Eg. `>>` may be split into two `>` tokens when parsing the end of a
    /// generic type parameter list (eg. `Map<u256, Map<u256, address>>`).
    buffered: Vec<Token<'a>>,

    paren_stack: Vec<Span>,
    indent_stack: Vec<BlockIndent<'a>>,
    indent_style: Option<char>,

    /// The diagnostics (errors and warnings) emitted during parsing.
    pub diagnostics: Vec<Diagnostic>,
    file_id: SourceFileId,
}

impl<'a> Parser<'a> {
    /// Create a new parser for a source code string and associated file id.
    pub fn new(content: &'a str, file_id: SourceFileId) -> Self {
        Parser {
            lexer: Lexer::new(content),
            buffered: vec![],
            paren_stack: vec![],
            indent_stack: vec![BlockIndent {
                context_span: Span::zero(),
                context_name: "module".into(),
                indent: "",
                indent_span: Span::zero(),
            }],
            indent_style: None,
            diagnostics: vec![],
            file_id,
        }
    }

    /// Return as wrapped back tracking parser
    pub fn as_bt_parser<'b>(&'b mut self) -> BTParser<'a, 'b> {
        BTParser::new(self)
    }

    /// Return the next token, or an error if we've reached the end of the file.
    #[allow(clippy::should_implement_trait)] // next() is a nice short name for a common task
    pub fn next(&mut self) -> ParseResult<Token<'a>> {
        // TODO: allow newlines inside square brackets
        // TODO: allow newlines inside angle brackets?
        //   eg `fn f(x: map\n <\n u8\n, ...`
        if !self.paren_stack.is_empty() {
            self.eat_newlines();
        }
        if let Some(tok) = self.next_raw() {
            if tok.kind == TokenKind::ParenOpen {
                self.paren_stack.push(tok.span);
            } else if tok.kind == TokenKind::ParenClose && self.paren_stack.pop().is_none() {
                self.error(tok.span, "Unmatched right parenthesis");
                if self.peek_raw() == Some(TokenKind::ParenClose) {
                    // another unmatched closing paren; fail.
                    return Err(ParseFailed);
                }
            }
            Ok(tok)
        } else {
            self.error(
                Span::new(self.lexer.source().len(), self.lexer.source().len()),
                "unexpected end of file",
            );
            Err(ParseFailed)
        }
    }

    fn next_raw(&mut self) -> Option<Token<'a>> {
        self.buffered.pop().or_else(|| self.lexer.next())
    }

    /// Take a peek at the next token kind without consuming it, or return an
    /// error if we've reached the end of the file.
    pub fn peek_or_err(&mut self) -> ParseResult<TokenKind> {
        if !self.paren_stack.is_empty() {
            self.eat_newlines();
        }
        if let Some(tk) = self.peek_raw() {
            Ok(tk)
        } else {
            let index = self.lexer.source().len();
            self.error(Span::new(index, index), "unexpected end of file");
            Err(ParseFailed)
        }
    }

    /// Take a peek at the next token kind. Returns `None` if we've reached the
    /// end of the file.
    pub fn peek(&mut self) -> Option<TokenKind> {
        if !self.paren_stack.is_empty() {
            self.eat_newlines();
        }
        self.peek_raw()
    }

    /// Peek at the text of the next token, without consuming it. This is useful
    /// for words that are keywords in some contexts, but not others. E.g.
    /// `from` can be used as a variable or field name, but it's the leading
    /// keyword of a `from x import y` statement.
    ///
    /// # Panics
    /// This function must only be used in cases where the kind of the next
    /// token has already been [`Parser::peek`]ed. If there is no next token, it
    /// will panic.
    pub fn peeked_text(&mut self) -> &'a str {
        self.buffered.last().unwrap().text
    }

    fn peek_raw(&mut self) -> Option<TokenKind> {
        if self.buffered.is_empty() {
            if let Some(tok) = self.lexer.next() {
                self.buffered.push(tok);
            } else {
                return None;
            }
        }
        Some(self.buffered.last().unwrap().kind)
    }

    /// Split the next token into two tokens, returning the first. Only supports
    /// splitting the `>>` token into two `>` tokens, specifically for
    /// parsing the closing angle bracket of a generic type argument list
    /// (`Map<x, Map<y, z>>`).
    ///
    /// # Panics
    /// Panics if the next token isn't `>>`
    pub fn split_next(&mut self) -> ParseResult<Token<'a>> {
        let gtgt = self.next()?;
        assert_eq!(gtgt.kind, TokenKind::GtGt);

        let (gt1, gt2) = gtgt.text.split_at(1);
        self.buffered.push(Token {
            kind: TokenKind::Gt,
            text: gt2,
            span: Span::new(gtgt.span.start + 1, gtgt.span.end),
        });

        Ok(Token {
            kind: TokenKind::Gt,
            text: gt1,
            span: Span::new(gtgt.span.start, gtgt.span.end - 1),
        })
    }

    /// Returns `true` if the parser has reached the end of the file.
    pub fn done(&mut self) -> bool {
        self.peek_raw() == None
    }

    /// The leading whitespace string of the last-seen indented line.
    /// This does not include lines inside of parentheses.
    pub fn last_indent(&self) -> &'a str {
        self.indent_stack.last().unwrap().indent
    }

    fn eat_newlines(&mut self) {
        while self.peek_raw() == Some(TokenKind::Newline) {
            self.next_raw();
        }
    }

    /// Assert that the next token kind it matches the expected token
    /// kind, and return it. This should be used in cases where the next token
    /// kind is expected to have been checked already.
    ///
    /// # Panics
    /// Panics if the next token kind isn't `tk`.
    pub fn assert(&mut self, tk: TokenKind) -> Token<'a> {
        let tok = self.next().unwrap();
        assert_eq!(tok.kind, tk, "internal parser error");
        tok
    }

    /// If the next token matches the expected kind, return it. Otherwise emit
    /// an error diagnostic with the given message and return an error.
    pub fn expect<S: Into<String>>(
        &mut self,
        expected: TokenKind,
        message: S,
    ) -> ParseResult<Token<'a>> {
        self.expect_with_notes(expected, message, Vec::new)
    }

    /// Like [`Parser::expect`], but with additional notes to be appended to the
    /// bottom of the diagnostic message. The notes are provided by a
    /// function that returns a `Vec<String>`, to avoid allocations in the
    /// case where the token is as expected.
    pub fn expect_with_notes<Str, NotesFn>(
        &mut self,
        expected: TokenKind,
        message: Str,
        notes_fn: NotesFn,
    ) -> ParseResult<Token<'a>>
    where
        Str: Into<String>,
        NotesFn: FnOnce() -> Vec<String>,
    {
        let tok = self.next()?;
        if tok.kind == expected {
            Ok(tok)
        } else {
            let label = if let Some(symbol) = expected.symbol_str() {
                format!("Unexpected token. Expected `{}`", symbol)
            } else {
                format!(
                    "Unexpected token. Expected {}",
                    expected.friendly_str().unwrap()
                )
            };
            self.fancy_error(
                message.into(),
                vec![Label::primary(tok.span, label)],
                notes_fn(),
            );
            Err(ParseFailed)
        }
    }

    /// Emit an "unexpected token" error diagnostic with the given message.
    pub fn unexpected_token_error<S: Into<String>>(
        &mut self,
        span: Span,
        message: S,
        notes: Vec<String>,
    ) {
        self.fancy_error(
            message,
            vec![Label::primary(span, "unexpected token".to_string())],
            notes,
        );
    }

    /// Enter an indented block, which is expected to be non-empty. This checks
    /// for and consumes the colon that precedes the block. If no colon is
    /// found, it emits an error and keeps parsing. Any number of
    /// newlines are allowed before the first non-empty indented line.
    /// Returns an error if the block has no non-empty indented line.
    ///
    /// # Panics
    /// Panics if called while the parser is inside a set of parentheses.
    pub fn enter_block(&mut self, context_span: Span, context_name: &str) -> ParseResult<()> {
        assert!(self.paren_stack.is_empty());

        let colon = if self.peek_raw() == Some(TokenKind::Colon) {
            self.next_raw()
        } else {
            self.fancy_error(
                format!("missing colon in {}", context_name),
                vec![Label::primary(
                    Span::new(context_span.end, context_span.end),
                    "expected `:` here",
                )],
                vec![],
            );
            None
        };

        self.handle_newline_indent(context_name)?;

        let indent = self.next()?;
        if indent.kind == TokenKind::Indent {
            self.indent_stack.push(BlockIndent {
                context_span,
                context_name: context_name.into(),
                indent: indent.text,
                indent_span: indent.span,
            });
            Ok(())
        } else {
            self.fancy_error(
                format!("failed to parse {} body", context_name),
                vec![Label::primary(
                    context_span + colon.as_ref(),
                    format!(
                        "the body of this {} must be indented and non-empty",
                        context_name,
                    ),
                )],
                vec![],
            );
            Err(ParseFailed)
        }
    }

    /// Expect and consume one or more newlines, without returning them.
    /// Returns an error if the next token isn't a newline or the end of the
    /// file, or if the indent of the next non-empty line doesn't match the
    /// current indentation.
    ///
    /// # Panics
    /// Panics if called while the parser is inside a set of parentheses.
    pub fn expect_newline(&mut self, context_name: &str) -> ParseResult<()> {
        self.handle_newline_indent(context_name)?;
        if self.peek() == Some(TokenKind::Indent) {
            let indent = self.next()?;
            self.fancy_error(
                "unexpected indentation",
                vec![Label::primary(
                    indent.span,
                    "this line indented further than other lines in the current block",
                )],
                vec![],
            );
            Err(ParseFailed)
        } else {
            Ok(())
        }
    }

    fn handle_newline_indent(&mut self, context_name: &str) -> ParseResult<()> {
        assert!(
            self.paren_stack.is_empty(),
            "Parser::handle_newline_indent called within parens"
        );

        match self.peek_raw() {
            None => Ok(()), // eof is an acceptable newline
            Some(TokenKind::Newline) => {
                let mut last_nl = self.next_raw().unwrap();
                while self.peek_raw() == Some(TokenKind::Newline) {
                    last_nl = self.next_raw().unwrap();
                }
                if self.done() {
                    return Ok(());
                }

                let (indent, indent_span) = indent_str(&last_nl);
                self.check_indent_style(indent, indent_span)?;

                if indent.len() > self.last_indent().len() {
                    self.buffered.push(Token {
                        kind: TokenKind::Indent,
                        text: indent,
                        span: indent_span,
                    });
                    return Ok(());
                }
                while indent.len() < self.last_indent().len() {
                    self.buffered.push(Token {
                        kind: TokenKind::Dedent,
                        text: indent,
                        span: indent_span,
                    });
                    self.indent_stack.pop();
                }
                if indent.len() != self.last_indent().len() {
                    self.indentation_error(
                        indent_span,
                        "this indentation doesn't match other lines in the current block or an enclosing block"
                    );
                    return Err(ParseFailed);
                }
                Ok(())
            }
            Some(_) => {
                let tok = self.next()?;
                self.unexpected_token_error(
                    tok.span,
                    format!("unexpected token while parsing {}", context_name),
                    vec!["expected a newline".into()],
                );
                Err(ParseFailed)
            }
        }
    }

    /// Emit an error diagnostic, but don't stop parsing
    pub fn error<S: Into<String>>(&mut self, span: Span, message: S) {
        self.diagnostics.push(Diagnostic {
            severity: Severity::Error,
            message: message.into(),
            labels: vec![Label::primary(span, "")],
            notes: vec![],
        })
    }

    /// Emit a "fancy" error diagnostic with any number of labels and notes,
    /// but don't stop parsing.
    pub fn fancy_error<S: Into<String>>(
        &mut self,
        message: S,
        labels: Vec<Label>,
        notes: Vec<String>,
    ) {
        self.diagnostics.push(Diagnostic {
            severity: Severity::Error,
            message: message.into(),
            labels,
            notes,
        })
    }

    fn indentation_error<S: Into<String>>(&mut self, span: Span, message: S) {
        self.fancy_error(
            "inconsistent indentation",
            vec![Label::primary(span, message.into())],
            vec![],
        );
    }

    fn check_indent_style(&mut self, indent: &str, span: Span) -> ParseResult<()> {
        if indent.is_empty() {
            Ok(())
        } else if indent.find(' ').is_some() && indent.find('\t').is_some() {
            self.indentation_error(span, "this indent contains both tabs and spaces");
            Err(ParseFailed)
        } else if self.indent_style.is_none() {
            self.indent_style = Some(indent.chars().next().unwrap());
            Ok(())
        } else if indent.chars().next() != self.indent_style {
            self.indentation_error(
                span,
                format!(
                    "This line is indented with {}s, while prior lines are indented with {}s.",
                    indent_char_name(indent.chars().next().unwrap()),
                    indent_char_name(self.indent_style.unwrap())
                ),
            );
            Err(ParseFailed)
        } else {
            Ok(())
        }
    }
}

/// A thin wrapper that makes [`Parser`] backtrackable.
pub struct BTParser<'a, 'b> {
    snapshot: &'b mut Parser<'a>,
    parser: Parser<'a>,
}

impl<'a, 'b> BTParser<'a, 'b> {
    pub fn new(snapshot: &'b mut Parser<'a>) -> Self {
        let parser = Parser {
            lexer: snapshot.lexer.clone(),
            buffered: snapshot.buffered.clone(),
            paren_stack: snapshot.paren_stack.clone(),
            indent_stack: snapshot.indent_stack.clone(),
            indent_style: snapshot.indent_style,
            diagnostics: Vec::new(),
            file_id: snapshot.file_id,
        };
        Self { snapshot, parser }
    }

    pub fn accept(self) {
        self.snapshot.lexer = self.parser.lexer;
        self.snapshot.buffered = self.parser.buffered;
        self.snapshot.paren_stack = self.parser.paren_stack;
        self.snapshot.indent_stack = self.parser.indent_stack;
        self.snapshot.indent_style = self.parser.indent_style;
        self.snapshot.diagnostics.extend(self.parser.diagnostics);
    }
}

impl<'a, 'b> std::ops::Deref for BTParser<'a, 'b> {
    type Target = Parser<'a>;

    fn deref(&self) -> &Self::Target {
        &self.parser
    }
}

impl<'a, 'b> std::ops::DerefMut for BTParser<'a, 'b> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.parser
    }
}

fn indent_char_name(c: char) -> &'static str {
    match c {
        ' ' => "space",
        '\t' => "tab",
        _ => panic!(),
    }
}

fn indent_str<'a>(tok: &Token<'a>) -> (&'a str, Span) {
    assert_eq!(tok.kind, TokenKind::Newline);
    let text = tok.text.trim_start_matches(&['\r', '\n'][..]);
    let span = Span::new(tok.span.start + (tok.text.len() - text.len()), tok.span.end);
    (text, span)
}

#[derive(Clone)]
struct BlockIndent<'a> {
    context_span: Span,
    context_name: String,
    indent: &'a str,
    indent_span: Span,
}
