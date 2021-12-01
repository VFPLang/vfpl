use crate::lexer::tokens::Token;
use crate::lexer::{Lexer, LexerResult};

fn lex(code: &str) -> LexerResult<Vec<Token>> {
    Lexer::new(&code.to_lowercase()).compute_tokens()
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
    insta::assert_debug_snapshot!(lex("000001.15"))
}

#[test]
fn decimal_with_minus_and_leading_zeros() {
    insta::assert_debug_snapshot!(lex("-00000012.23"))
}

#[test]
fn decimal_with_alphabetical_char() {
    insta::assert_debug_snapshot!(lex("00001.as244"))
}

#[test]
fn please_keyword() {
    insta::assert_debug_snapshot!(lex("please"))
}

#[test]
fn string() {
    insta::assert_debug_snapshot!(lex("\"Hello all\""))
}

#[test]
fn string_with_escape_chars() {
    insta::assert_debug_snapshot!(lex("\"What is a \\\"unit test\\\"?\""))
}

#[test]
fn string_no_end() {
    insta::assert_debug_snapshot!(lex("\"Hahaha this is going to destroy it"))
}

#[test]
fn should_be_ident() {
    insta::assert_debug_snapshot!(lex("please hugo"))
}

#[test]
fn all_keywords() {
    insta::assert_debug_snapshot!(lex("As"));
    insta::assert_debug_snapshot!(lex("Add"));
    insta::assert_debug_snapshot!(lex("Sub"));
    insta::assert_debug_snapshot!(lex("Mul"));
    insta::assert_debug_snapshot!(lex("Div"));
    insta::assert_debug_snapshot!(lex("Mod"));
    insta::assert_debug_snapshot!(lex("Initialize"));
    insta::assert_debug_snapshot!(lex("Variable"));
    insta::assert_debug_snapshot!(lex("With"));
    insta::assert_debug_snapshot!(lex("The"));
    insta::assert_debug_snapshot!(lex("Value"));
    insta::assert_debug_snapshot!(lex("Of"));
    insta::assert_debug_snapshot!(lex("Set"));
    insta::assert_debug_snapshot!(lex("To"));
    insta::assert_debug_snapshot!(lex("From"));
    insta::assert_debug_snapshot!(lex("By"));
    insta::assert_debug_snapshot!(lex("Take"));
    insta::assert_debug_snapshot!(lex("End"));
    insta::assert_debug_snapshot!(lex("Check"));
    insta::assert_debug_snapshot!(lex("Whether"));
    insta::assert_debug_snapshot!(lex("Then"));
    insta::assert_debug_snapshot!(lex("Do"));
    insta::assert_debug_snapshot!(lex("Otherwise"));
    insta::assert_debug_snapshot!(lex("Break"));
    insta::assert_debug_snapshot!(lex("Out"));
    insta::assert_debug_snapshot!(lex("This"));
    insta::assert_debug_snapshot!(lex("Create"));
    insta::assert_debug_snapshot!(lex("Function"));
    insta::assert_debug_snapshot!(lex("Parameter"));
    insta::assert_debug_snapshot!(lex("Parameters"));
    insta::assert_debug_snapshot!(lex("That"));
    insta::assert_debug_snapshot!(lex("Returns"));
    insta::assert_debug_snapshot!(lex("Call"));
    insta::assert_debug_snapshot!(lex("No"));
    insta::assert_debug_snapshot!(lex("Argument"));
    insta::assert_debug_snapshot!(lex("Arguments"));
    insta::assert_debug_snapshot!(lex("And"));
    insta::assert_debug_snapshot!(lex("Go"));
    insta::assert_debug_snapshot!(lex("Sleep"));
    insta::assert_debug_snapshot!(lex("Absent"));
    insta::assert_debug_snapshot!(lex("Null"));
    insta::assert_debug_snapshot!(lex("NoValue"));
    insta::assert_debug_snapshot!(lex("Undefined"));
    insta::assert_debug_snapshot!(lex("True"));
    insta::assert_debug_snapshot!(lex("False"));
    insta::assert_debug_snapshot!(lex("Does"));
    insta::assert_debug_snapshot!(lex("Has"));
    insta::assert_debug_snapshot!(lex("Is"));
    insta::assert_debug_snapshot!(lex("Not"));
    insta::assert_debug_snapshot!(lex("Have"));
    insta::assert_debug_snapshot!(lex("Greater"));
    insta::assert_debug_snapshot!(lex("Less"));
    insta::assert_debug_snapshot!(lex("Than"));
    insta::assert_debug_snapshot!(lex("Equal"));
    insta::assert_debug_snapshot!(lex("Or"));
    insta::assert_debug_snapshot!(lex("Repeat"));
    insta::assert_debug_snapshot!(lex("Return"));
    insta::assert_debug_snapshot!(lex("While"));
}
