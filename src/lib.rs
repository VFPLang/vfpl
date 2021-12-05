use crate::lexer::tokens::Token;
use crate::lexer::{Lexer, LexerError};

mod error;
mod interpret_ast;
mod lexer;
mod parse;

pub use error::display_error;
pub use interpret_ast::run;
pub use interpret_ast::Vm;
pub use parse::parse;

/// Lexes an input stream into Tokens
pub fn lex(code: &str) -> Result<Vec<Token>, LexerError> {
    let mut lexer = Lexer::new(code);

    lexer.compute_tokens()
}
