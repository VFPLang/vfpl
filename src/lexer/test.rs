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
fn decimal_with_alphabetical_char() {
    insta::assert_debug_snapshot!(lex("00001.as244"))
}
