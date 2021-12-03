//!
//! Parses the source tokens using recursive descent

use std::vec;

use peekmore::{PeekMore, PeekMoreIterator};

use crate::error::{CompilerError, Span};
use crate::lexer::tokens::Token;
use crate::parse::ast::Program;

pub mod ast;
mod helper;
mod parser;

#[cfg(test)]
mod test;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
struct Parser {
    tokens: PeekMoreIterator<vec::IntoIter<Token>>,
    depth: usize,
    /// For restricting uses of the "break" statement
    in_while_depth: usize,
    /// For restricting uses of the "return" statement
    in_fn_depth: usize,
}

impl Parser {
    fn new(tokens: vec::IntoIter<Token>) -> Self {
        Parser {
            tokens: tokens.peekmore(),
            depth: 0,
            in_while_depth: 0,
            in_fn_depth: 0,
        }
    }
}

pub fn parse(tokens: vec::IntoIter<Token>) -> ParseResult<Program> {
    let mut parser = Parser::new(tokens);
    parser.program()
}

#[derive(Debug)]
pub struct ParseError {
    span: Span,
    message: String,
}

impl CompilerError for ParseError {
    fn span(&self) -> Span {
        self.span
    }

    fn message(&self) -> String {
        self.message.clone()
    }

    fn note(&self) -> Option<String> {
        None
    }
}
