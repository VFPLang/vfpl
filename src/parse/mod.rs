//!
//! Parses the source tokens using recursive descent

use std::rc::Rc;
use std::vec;

use crate::global::Session;
use peekmore::{PeekMore, PeekMoreIterator};

use crate::lexer::tokens::Token;
use crate::parse::ast::Program;
use crate::parse::error::ParseError;
use crate::VfplError;

pub mod ast;
mod helper;
mod parser;

mod error;
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
    session: Rc<Session>,
}

impl Parser {
    fn new(tokens: vec::IntoIter<Token>, session: Rc<Session>) -> Self {
        Parser {
            tokens: tokens.peekmore(),
            depth: 0,
            in_while_depth: 0,
            in_fn_depth: 0,
            session,
        }
    }
}

///
/// Parses the tokens into an AST
pub fn parse(tokens: vec::IntoIter<Token>, session: Rc<Session>) -> Result<Program, VfplError> {
    let mut parser = Parser::new(tokens, session);
    parser.program().map_err(|err| err.into())
}
