use crate::lexer::tokens::Token;
use crate::lexer::{Lexer, LexerError};

mod error;
mod interpret_ast;
mod lexer;
mod parse;

pub use error::display_error;
pub use interpret_ast::run;
pub use parse::parse;

/// this is mainly to remove the dead code warnings on code that is not actually dead
pub fn test_parse(code: &str) {
    let tokens = Lexer::new(code).compute_tokens();

    if let Ok(tokens) = tokens {
        let _ = parse::parse(tokens.into_iter());
    }
}

pub fn lex(code: &str) -> Result<Vec<Token>, LexerError> {
    let lower = code.to_lowercase();
    let mut lexer = Lexer::new(&lower);

    lexer.compute_tokens()
}
