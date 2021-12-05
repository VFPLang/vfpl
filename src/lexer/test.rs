use crate::lexer::tokens::Token;
use crate::lexer::{Lexer, LexerResult};
use crate::Session;

fn lex(code: &str) -> LexerResult<Vec<Token>> {
    Lexer::new(&code.to_lowercase(), Session::test_session()).compute_tokens()
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
        "As",
        "Add",
        "Sub",
        "Mul",
        "Div",
        "Modulo",
        "Initialize",
        "Variable",
        "With",
        "The",
        "Value",
        "Of",
        "Set",
        "To",
        "From",
        "By",
        "Take",
        "End",
        "Check",
        "Whether",
        "Then",
        "Do",
        "Otherwise",
        "Break",
        "Out",
        "This",
        "Create",
        "Function",
        "Parameter",
        "Parameters",
        "That",
        "Returns",
        "Call",
        "No",
        "Argument",
        "Arguments",
        "And",
        "Go",
        "Sleep",
        "Absent",
        "Null",
        "NoValue",
        "Undefined",
        "True",
        "False",
        "Does",
        "Has",
        "Is",
        "Not",
        "Have",
        "Greater",
        "Less",
        "Than",
        "Equal",
        "Or",
        "Repeat",
        "Return",
        "While",
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
