use crate::lexer::tokens::Token;
use crate::lexer::{Lexer, LexerError};

mod error;
mod interpret_ast;
mod lexer;
mod parse;

pub use error::display_error;
pub use interpret_ast::run;
pub use parse::parse;

/// Lexes an input stream into Tokens
pub fn lex(code: &str) -> Result<Vec<Token>, LexerError> {
    let lower = code.to_lowercase();
    let mut lexer = Lexer::new(&lower);

    lexer.compute_tokens()
}
