use crate::tokens::Token;
use crate::{Lexer, LexerResult};
use vfpl_global::GlobalCtx;

fn lex(code: &str) -> LexerResult<Vec<Token>> {
    Lexer::new(&code.to_lowercase(), GlobalCtx::test_ctx()).compute_tokens()
}

#[test]
fn one_number() {
    insta::assert_debug_snapshot!(lex("1"));
}

#[test]
fn leading_zeros() {
    insta::assert_debug_snapshot!(lex("000012"));
}

#[test]
fn minus_number() {
    insta::assert_debug_snapshot!(lex("-12"));
}

#[test]
fn minus_with_leading_zeros() {
    insta::assert_debug_snapshot!(lex("-0000012"));
}

#[test]
fn alphabetical_char_in_number() {
    insta::assert_debug_snapshot!(lex("12a"));
}

#[test]
fn decimal() {
    insta::assert_debug_snapshot!(lex("13.35253"));
}

#[test]
fn decimal_with_leading_zeros() {
    insta::assert_debug_snapshot!(lex("000001.15"));
}

#[test]
fn decimal_with_minus_and_leading_zeros() {
    insta::assert_debug_snapshot!(lex("-00000012.23"));
}

#[test]
fn decimal_with_alphabetical_char() {
    insta::assert_debug_snapshot!(lex("00001.as244"));
}

#[test]
fn please_keyword() {
    insta::assert_debug_snapshot!(lex("please"));
}

#[test]
fn string() {
    insta::assert_debug_snapshot!(lex("\"Hello all\""));
}

#[test]
fn string_with_escape_chars() {
    insta::assert_debug_snapshot!(lex(r#""What is a \"unit test\"?""#));
}

#[test]
fn string_no_end() {
    insta::assert_debug_snapshot!(lex("\"Hahaha this is going to destroy it"));
}

#[test]
fn should_be_ident() {
    insta::assert_debug_snapshot!(lex("please hugo"));
}

#[test]
fn all_keywords() {
    let tokens = [
        "as",
        "add",
        "sub",
        "mul",
        "div",
        "modulo",
        "initialize",
        "variable",
        "with",
        "the",
        "value",
        "of",
        "set",
        "to",
        "from",
        "by",
        "take",
        "end",
        "check",
        "whether",
        "then",
        "do",
        "otherwise",
        "break",
        "out",
        "this",
        "create",
        "function",
        "parameter",
        "parameters",
        "that",
        "returns",
        "call",
        "no",
        "argument",
        "arguments",
        "and",
        "go",
        "sleep",
        "absent",
        "null",
        "noValue",
        "undefined",
        "true",
        "false",
        "does",
        "has",
        "is",
        "not",
        "have",
        "greater",
        "less",
        "than",
        "equal",
        "or",
        "repeat",
        "return",
        "while",
        "structure",
        "field",
        "fields",
        "define",
    ]
    .map(lex);
    insta::assert_debug_snapshot!(tokens);
}

#[test]
fn normal_line_of_code() {
    insta::assert_debug_snapshot!(lex(
        "please initialize variable x as Integer with the value of 15.\n"
    ));
}

#[test]
fn normal_line_of_code_decimal() {
    insta::assert_debug_snapshot!(lex(
        "please initialize variable x as Integer with the value of 15.3.\n"
    ));
}
