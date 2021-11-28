use crate::error::Span;
use crate::lexer::tokens::Token;
use crate::parse::ast::Program;
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::vec;

pub mod ast;
mod helper;
mod parser;

#[cfg(test)]
mod test;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
struct Parser {
    tokens: Peekable<vec::IntoIter<Token>>,
    depth: usize,
}

impl Parser {
    fn new(tokens: vec::IntoIter<Token>) -> Self {
        Parser {
            tokens: tokens.peekable(),
            depth: 0,
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

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.message)
    }
}

impl std::error::Error for ParseError {}
