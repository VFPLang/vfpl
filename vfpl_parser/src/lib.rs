//!
//! Parses the source tokens using recursive descent

use std::cell::RefCell;
use std::rc::Rc;
use std::vec;

use crate::error::ParseError;
use peekmore::{PeekMore, PeekMoreIterator};
use vfpl_ast::Program;
use vfpl_error::VfplError;
use vfpl_global::GlobalCtx;
use vfpl_lexer::tokens::Token;

mod helper;
mod parser;

mod error;
#[cfg(test)]
mod test;

type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug, Clone)]
struct Parser {
    tokens: PeekMoreIterator<vec::IntoIter<Token>>,
    depth: usize,
    /// For restricting uses of the "break" statement
    in_while_depth: usize,
    /// For restricting uses of the "return" statement
    in_fn_depth: usize,
    global_ctx: Rc<RefCell<GlobalCtx>>,
}

impl Parser {
    fn new(tokens: vec::IntoIter<Token>, global_ctx: Rc<RefCell<GlobalCtx>>) -> Self {
        Parser {
            tokens: tokens.peekmore(),
            depth: 0,
            in_while_depth: 0,
            in_fn_depth: 0,
            global_ctx,
        }
    }
}

///
/// Parses the tokens into an AST
pub fn parse(
    tokens: vec::IntoIter<Token>,
    global_ctx: Rc<RefCell<GlobalCtx>>,
) -> Result<Program, VfplError> {
    let mut parser = Parser::new(tokens, global_ctx);
    parser.program().map_err(|err| err.into())
}
