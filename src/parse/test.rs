//// helpers

use crate::error::Span;
use crate::lexer::tokens::{
    Token,
    TokenType::{self, *},
};
use crate::parse::Parser;

fn token(kind: TokenType) -> Token {
    Token {
        span: Span::dummy(),
        kind,
    }
}

fn parse<T, F, R>(tokens: T, f: F) -> R
where
    T: Into<Vec<Token>>,
    F: FnOnce(&mut Parser) -> R,
{
    let vec = tokens.into();
    let mut parser = Parser {
        tokens: vec.into_iter().peekable(),
        depth: 0,
    };
    let result = f(&mut parser);
    result
}

//// tests

#[test]
fn it_works() {
    let snap = "test";
    insta::assert_debug_snapshot!(snap);
}

// #[test]
// fn single_string_literal() {
//     let tokens = [String("string".to_string())].map(token);
//     let string = parse(tokens, Parser::literal);
//     insta::assert_debug_snapshot!(string);
// }
