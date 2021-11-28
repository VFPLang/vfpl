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

fn parse<T, F, R>(tokens: T, parse_rule_fn: F) -> R
where
    T: Into<Vec<Token>>,
    F: FnOnce(&mut Parser) -> R,
{
    let vec = tokens.into();
    let mut parser = Parser {
        tokens: vec.into_iter().peekable(),
        depth: 0,
    };
    parse_rule_fn(&mut parser)
}

//// tests

#[test]
fn it_works() {
    let snap = "test";
    insta::assert_debug_snapshot!(snap);
}

#[test]
fn single_string_literal() {
    let tokens = [String("string".to_string())].map(token);
    let parsed = parse(tokens, Parser::literal);
    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn different_literals() {
    let literals = [
        [String("test".to_string())].map(token),
        [Int(425)].map(token),
        [Float(2465.67)].map(token),
        [Absent].map(token),
        [NoValue].map(token),
        [Null].map(token),
        [Undefined].map(token),
        [True].map(token),
        [False].map(token),
    ];

    let parsed = literals.map(|tokens| parse(tokens, Parser::literal));

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn ident_ty() {
    let tokens = [Ident("Integer".to_string())].map(token);
    let parsed = parse(tokens, Parser::ty);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn nullable_ty() {
    let tys = [
        [Absent].map(token),
        [NoValue].map(token),
        [Null].map(token),
        [Undefined].map(token),
    ];
    let parsed = tys.map(|tokens| parse(tokens, Parser::ty));

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn literal_call_expr() {
    let tokens = [String("string".to_string())].map(token);
    let parsed = parse(tokens, Parser::call_expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn literal_expr() {
    let tokens = [String("string".to_string())].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn not_equal_expr() {
    let tokens = [Absent, Does, Not, Have, The, Value, Null].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn equal_expr() {
    let tokens = [Absent, Has, The, Value, Absent].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn greater_than_expr() {
    let tokens = [Absent, Is, Greater, Than, Undefined].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn less_than_expr() {
    let tokens = [Absent, Is, Less, Than, Undefined].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn greater_equal_than_expr() {
    let tokens = [Absent, Is, Greater, Or, Equal, Than, Undefined].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn less_equal_than_expr() {
    let tokens = [Absent, Is, Less, Or, Equal, Than, Undefined].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn typed_ident_absent() {
    let tokens = [Ident("name".to_string()), As, Absent].map(token);
    let parsed = parse(tokens, Parser::typed_ident);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn init_variable_string() {
    let tokens = [
        Initialize,
        Variable,
        Ident("name".to_string()),
        As,
        Ident("string".to_string()),
        With,
        The,
        Value,
        Of,
        String("Ferris".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::var_init);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn set_variable_string() {
    let tokens = [
        Set,
        The,
        Variable,
        Ident("name".to_string()),
        To,
        The,
        Value,
        Of,
        String("Ferris".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::var_set);

    insta::assert_debug_snapshot!(parsed);
}
