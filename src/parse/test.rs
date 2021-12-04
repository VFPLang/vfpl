//// helpers

use crate::error::Span;
use crate::lexer::tokens::{
    Token,
    TokenKind::{self, *},
};
use crate::parse::Parser;

fn token(kind: TokenKind) -> Token {
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
    let mut parser = Parser::new(vec.into_iter());
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
    let tokens = [Ident("Test".to_string())].map(token);
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
fn other_tys() {
    let tys = [
        [Ident("Boolean".to_string())].map(token),
        [Ident("Integer".to_string())].map(token),
        [Ident("String".to_string())].map(token),
        [Ident("Float".to_string())].map(token),
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
fn ident_literal() {
    let tokens = [Ident("uwu".to_string())].map(token);
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

#[test]
fn add_number() {
    let tokens = [Add, Int(0), To, Ident("counter".to_string())].map(token);
    let parsed = parse(tokens, Parser::add);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn subtract_number() {
    let tokens = [Sub, Int(1), From, Ident("counter".to_string())].map(token);
    let parsed = parse(tokens, Parser::subtract);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn divide_number() {
    let tokens = [Div, Ident("counter".to_string()), By, Int(2)].map(token);
    let parsed = parse(tokens, Parser::divide);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn multiply_number() {
    let tokens = [Mul, Ident("counter".to_string()), With, Int(3)].map(token);
    let parsed = parse(tokens, Parser::multiply);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn mod_number() {
    let tokens = [Take, Ident("counter".to_string()), Mod, Int(4)].map(token);
    let parsed = parse(tokens, Parser::modulo);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn terminate() {
    let tokens = [Go, To, Sleep].map(token);
    let parsed = parse(tokens, Parser::terminate);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_no_args() {
    let tokens = [Call, Ident("run".to_string()), With, No, Arguments].map(token);
    let parsed = parse(tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_single_arg() {
    let tokens = [
        Call,
        Ident("print".to_string()),
        With,
        The,
        Argument,
        Int(0),
        As,
        Ident("printable".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_two_args() {
    let tokens = [
        Call,
        Ident("add".to_string()),
        With,
        The,
        Arguments,
        Int(2),
        As,
        Ident("a".to_string()),
        And,
        Int(3),
        As,
        Ident("b".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_three_args() {
    let tokens = [
        Call,
        Ident("ternary".to_string()),
        With,
        The,
        Arguments,
        True,
        As,
        Ident("cond".to_string()),
        Comma,
        Int(0),
        As,
        Ident("then".to_string()),
        And,
        Int(3),
        As,
        Ident("else".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn add_stmt() {
    let tokens = [Please, Add, Int(0), To, Ident("A".to_string()), Dot].map(token);
    let parsed = parse(tokens, Parser::stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn if_empty_body_true_cond() {
    let tokens = [Check, Whether, True, Comma, Then, Do, Please, End, Check].map(token);
    let parsed = parse(tokens, Parser::if_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn if_empty_body_complex_cond() {
    let tokens = [
        Check, Whether, True, Has, The, Value, Absent, Comma, Then, Do, Please, End, Check,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::if_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn if_single_stmt_body() {
    let tokens = [
        Check, Whether, True, Has, The, Value, Absent, Comma, Then, Do, Please, Go, To, Sleep, Dot,
        Please, End, Check,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::if_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn if_multi_stmt_body() {
    let tokens = [
        Check,
        Whether,
        True,
        Has,
        The,
        Value,
        Absent,
        Comma,
        Then,
        Do,
        Please,
        Go,
        To,
        Sleep,
        Dot,
        Please,
        Add,
        Int(5),
        To,
        Ident("A".to_string()),
        Dot,
        Please,
        End,
        Check,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::if_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn return_number() {
    let tokens = [Return, Int(9), From, The, Function].map(token);
    let parsed = parse(tokens, Parser::return_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn return_stmt() {
    let tokens = [Please, Return, Int(9), From, The, Function, Dot].map(token);
    let parsed = parse(tokens, Parser::stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn r#break() {
    let tokens = [Break, Out, Of, This, While].map(token);
    let parsed = parse(tokens, Parser::break_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn break_stmt() {
    let tokens = [Please, Break, Out, Of, This, While, Dot].map(token);
    let parsed = parse(tokens, Parser::stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn while_loop_no_body() {
    let tokens = [Repeat, While, True, Do, Please, End, While].map(token);
    let parsed = parse(tokens, Parser::while_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn while_stmt_with_body() {
    let tokens = [
        Please,
        Repeat,
        While,
        Absent,
        Has,
        The,
        Value,
        False,
        Do,
        Please,
        Add,
        Int(9),
        To,
        Ident("A".to_string()),
        Dot,
        Please,
        End,
        While,
        Dot,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn return_ty_absent() {
    let tokens = [That, Returns, Absent].map(token);
    let parsed = parse(tokens, Parser::fn_return);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn fn_decl_no_params_empty_body() {
    let tokens = [
        Create,
        Function,
        Ident("void".to_string()),
        With,
        No,
        Parameters,
        That,
        Returns,
        Absent,
        Please,
        End,
        Function,
        Ident("void".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::fn_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn fn_decl_single_param_emtpy_body() {
    let tokens = [
        Create,
        Function,
        Ident("print".to_string()),
        With,
        The,
        Parameter,
        Ident("_".to_string()),
        As,
        Absent,
        That,
        Returns,
        Absent,
        Please,
        End,
        Function,
        Ident("print".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::fn_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn fn_decl_two_param_emtpy_body() {
    let tokens = [
        Create,
        Function,
        Ident("add".to_string()),
        With,
        The,
        Parameters,
        Ident("_".to_string()),
        As,
        Absent,
        And,
        Ident("_hi".to_string()),
        As,
        Null,
        That,
        Returns,
        Absent,
        Please,
        End,
        Function,
        Ident("add".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::fn_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn fn_decl_three_param_with_body() {
    let tokens = [
        Create,
        Function,
        Ident("add".to_string()),
        With,
        The,
        Parameters,
        Ident("_".to_string()),
        As,
        Absent,
        Comma,
        Ident("a".to_string()),
        As,
        NoValue,
        And,
        Ident("_hi".to_string()),
        As,
        Null,
        That,
        Returns,
        Absent,
        Please,
        Add,
        Int(5),
        To,
        Ident("a".to_string()),
        Dot,
        Please,
        End,
        Function,
        Ident("add".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::fn_decl);

    insta::assert_debug_snapshot!(parsed);
}
