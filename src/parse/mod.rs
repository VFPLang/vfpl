#![allow(dead_code)]
use crate::error::Span;
use crate::lexer::tokens::Token;
use crate::parse::ast::{Body, Program, Stmt, TypedIdent};
use std::fmt::{Display, Formatter};
use std::iter::Peekable;
use std::vec::IntoIter;

pub mod ast;
mod parser;
#[cfg(test)]
mod test;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
struct Parser {
    tokens: Peekable<IntoIter<Token>>,
    depth: usize,
}

pub fn parse(tokens: IntoIter<Token>) -> ParseResult<Program> {
    let mut parser = Parser {
        tokens: tokens.peekable(),
        depth: 0,
    };
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
