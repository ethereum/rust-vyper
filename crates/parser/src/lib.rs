pub mod ast;
pub mod grammar;
pub mod lexer;
pub use lexer::{Token, TokenKind};
mod parser;
pub use parser::{Label, ParseFailed, ParseResult, Parser};
pub mod node;

use ast::Module;
use fe_common::diagnostics::Diagnostic;

/// Parse a [`Module`] from the file content string.
///
/// If there was no fatal error during parsing, it returns the parsed module and
/// a vector of [`Diagnostic`]s, which should be printed. If any of the returned
/// diagnostics are errors, the compilation of this file should ultimately fail.
///
/// If the parser does reach a fatal error, this returns the list of generated
/// diagnostics.
///
/// A [`SourceFileId`] is required to associate any diagnostics with the
/// underlying file.
pub fn parse_file(src: &str) -> Result<(Module, Vec<Diagnostic>), Vec<Diagnostic>> {
    let mut parser = Parser::new(src);
    match crate::grammar::module::parse_module(&mut parser) {
        Err(_) => Err(parser.diagnostics),
        Ok(node) => Ok((node.kind, parser.diagnostics)),
    }
}

/// Apply the given parsing function to the code string, returning the result.
/// If the parsing fails, the parser's diagnostics will be printed.
/// This function is provided for easy testing of later compiler stages.
pub fn parse_code_chunk<F, T>(mut parse_fn: F, src: &str) -> ParseResult<T>
where
    F: FnMut(&mut Parser) -> ParseResult<T>,
{
    let mut files = fe_common::files::FileStore::new();
    let id = files.add_file("parse_code_chunk test snippet", src);
    let mut parser = Parser::new(src);
    if let Ok(node) = parse_fn(&mut parser) {
        Ok(node)
    } else {
        fe_common::diagnostics::print_diagnostics(&parser.diagnostics, id, &files);
        Err(ParseFailed)
    }
}
