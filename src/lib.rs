use crate::lexer::tokens::Token;
use crate::lexer::{Lexer, LexerError};
use std::rc::Rc;

mod error;
mod global;
mod interpret_ast;
mod lexer;
mod parse;

pub use error::display_error;
pub use global::Session;
pub use interpret_ast::run;
pub use interpret_ast::Vm;
pub use parse::parse;

/// Lexes an input stream into Tokens
pub fn lex(code: &str, session: Rc<Session>) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(code, session);

    lexer.compute_tokens()
}
