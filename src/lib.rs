mod error;
mod global;
mod lexer;
mod parse;

pub use error::{display_error, VfplError};
pub use global::Session;
pub use vfpl_ast_interpreter::::run;
pub use vfpl_ast_interpreter::::Vm;
pub use lexer::lex;
pub use parse::parse;
