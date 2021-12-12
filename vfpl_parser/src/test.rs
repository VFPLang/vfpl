//// helpers

use super::Parser;
use vfpl_error::Span;
use vfpl_global::{GlobalCtx, Spur};
use vfpl_lexer::tokens::CondKeyword::*;
use vfpl_lexer::tokens::TokenKind::*;
use vfpl_lexer::tokens::{Token, TokenKind};

fn token(kind: TokenKind) -> Token {
    Token {
        span: Span::dummy(),
        kind,
    }
}

#[cfg(test)]
fn ident(str: &str) -> Spur {
    let mut global_context = vfpl_global::GLOBAL_TEST_CTX.lock().unwrap();
    global_context.intern_string(str)
}

/// parses CondKw(The) tokens and appends an EOF token
fn parse<T, F, R>(tokens: T, parse_rule_fn: F) -> R
where
    T: Into<Vec<Token>>,
    F: FnOnce(&mut Parser) -> R,
{
    let mut vec = tokens.into();
    vec.push(Token::eof());
    let mut parser = Parser::new(vec.into_iter(), GlobalCtx::test_ctx());
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
    let tokens = [Ident(ident("Test"))].map(token);
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
        [Ident(ident("Boolean"))].map(token),
        [Ident(ident("Integer"))].map(token),
        [Ident(ident("String"))].map(token),
        [Ident(ident("Float"))].map(token),
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
    let tokens = [Ident(ident("uwu"))].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn ident_cond_keyword_literal() {
    let tokens = [CondKw(From)].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn not_equal_expr() {
    let tokens = [
        Absent,
        CondKw(Does),
        Not,
        CondKw(Have),
        CondKw(The),
        CondKw(Value),
        Null,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn equal_expr() {
    let tokens = [Absent, CondKw(Has), CondKw(The), CondKw(Value), Absent].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn greater_than_expr() {
    let tokens = [Absent, CondKw(Is), CondKw(Greater), CondKw(Than), Undefined].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn less_than_expr() {
    let tokens = [Absent, CondKw(Is), CondKw(Less), CondKw(Than), Undefined].map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn greater_equal_than_expr() {
    let tokens = [
        Absent,
        CondKw(Is),
        CondKw(Greater),
        Or,
        CondKw(Equal),
        CondKw(Than),
        Undefined,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn less_equal_than_expr() {
    let tokens = [
        Absent,
        CondKw(Is),
        CondKw(Less),
        Or,
        CondKw(Equal),
        CondKw(Than),
        Undefined,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn typed_ident_absent() {
    let tokens = [Ident(ident("name")), As, Absent].map(token);
    let parsed = parse(tokens, Parser::typed_ident);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn init_variable_string() {
    let tokens = [
        Initialize,
        Variable,
        Ident(ident("name")),
        As,
        Ident(ident("string")),
        CondKw(With),
        CondKw(The),
        CondKw(Value),
        CondKw(Of),
        String("Ferris".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::var_init);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn set_variable_string() {
    let tokens = [
        CondKw(Set),
        CondKw(The),
        Variable,
        Ident(ident("name")),
        CondKw(To),
        CondKw(The),
        CondKw(Value),
        CondKw(Of),
        String("Ferris".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::var_set);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn add_number() {
    let tokens = [CondKw(Add), Int(0), CondKw(To), Ident(ident("counter"))].map(token);
    let parsed = parse(tokens, Parser::term);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn subtract_number() {
    let tokens = [CondKw(Sub), Int(1), CondKw(From), Ident(ident("counter"))].map(token);
    let parsed = parse(tokens, Parser::term);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn divide_number() {
    let tokens = [CondKw(Div), Ident(ident("counter")), CondKw(By), Int(2)].map(token);
    let parsed = parse(tokens, Parser::factor);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn multiply_number() {
    let tokens = [CondKw(Mul), Ident(ident("counter")), CondKw(With), Int(3)].map(token);
    let parsed = parse(tokens, Parser::factor);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn mod_number() {
    let tokens = [CondKw(Take), Ident(ident("counter")), CondKw(Mod), Int(4)].map(token);
    let parsed = parse(tokens, Parser::factor);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn nested_add_number() {
    let tokens = [
        CondKw(Add),
        Int(5),
        CondKw(To),
        ParenOpen,
        CondKw(Add),
        Int(6),
        CondKw(To),
        Int(7),
        ParenClose,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::term);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn terminate() {
    let tokens = [CondKw(Go), CondKw(To), CondKw(Sleep)].map(token);
    let parsed = parse(tokens, Parser::terminate);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_no_args() {
    let tokens = [
        Call,
        Ident(ident("run")),
        CondKw(With),
        CondKw(No),
        CondKw(Arguments),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_single_arg() {
    let tokens = [
        Call,
        Ident(ident("print")),
        CondKw(With),
        CondKw(The),
        CondKw(Argument),
        Int(0),
        As,
        Ident(ident("printable")),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_two_args() {
    let tokens = [
        Call,
        Ident(ident("add")),
        CondKw(With),
        CondKw(The),
        CondKw(Arguments),
        Int(2),
        As,
        Ident(ident("a")),
        And,
        Int(3),
        As,
        Ident(ident("b")),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_three_args() {
    let tokens = [
        Call,
        Ident(ident("ternary")),
        CondKw(With),
        CondKw(The),
        CondKw(Arguments),
        True,
        As,
        Ident(ident("cond")),
        Comma,
        Int(0),
        As,
        Ident(ident("then")),
        And,
        Int(3),
        As,
        Ident(ident("else")),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn add_stmt() {
    let tokens = [
        Please,
        CondKw(Add),
        Int(0),
        CondKw(To),
        Ident(ident("A")),
        Dot,
    ]
    .map(token);
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
        Check,
        Whether,
        True,
        CondKw(Has),
        CondKw(The),
        CondKw(Value),
        Absent,
        Comma,
        Then,
        Do,
        Please,
        End,
        Check,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::if_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn if_single_stmt_body() {
    let tokens = [
        Check,
        Whether,
        True,
        CondKw(Has),
        CondKw(The),
        CondKw(Value),
        Absent,
        Comma,
        Then,
        Do,
        Please,
        CondKw(Go),
        CondKw(To),
        CondKw(Sleep),
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
fn if_multi_stmt_body() {
    let tokens = [
        Check,
        Whether,
        True,
        CondKw(Has),
        CondKw(The),
        CondKw(Value),
        Absent,
        Comma,
        Then,
        Do,
        Please,
        CondKw(Go),
        CondKw(To),
        CondKw(Sleep),
        Dot,
        Please,
        CondKw(Add),
        Int(5),
        CondKw(To),
        Ident(ident("A")),
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
    let tokens = [Return, Int(9), CondKw(From), CondKw(The), Function].map(token);
    let parsed = parse(tokens, Parser::return_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn return_stmt() {
    let tokens = [
        Please,
        Return,
        Int(9),
        CondKw(From),
        CondKw(The),
        Function,
        Dot,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn r#break() {
    let tokens = [Break, CondKw(Out), CondKw(Of), This, While].map(token);
    let parsed = parse(tokens, Parser::break_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn break_stmt() {
    let tokens = [Please, Break, CondKw(Out), CondKw(Of), This, While, Dot].map(token);
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
        CondKw(Has),
        CondKw(The),
        CondKw(Value),
        False,
        Do,
        Please,
        CondKw(Add),
        Int(9),
        CondKw(To),
        Ident(ident("A")),
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
    let tokens = [CondKw(That), CondKw(Returns), Absent].map(token);
    let parsed = parse(tokens, Parser::fn_return);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn fn_decl_no_params_empty_body() {
    let tokens = [
        Create,
        Function,
        Ident(ident("void")),
        CondKw(With),
        CondKw(No),
        CondKw(Parameters),
        CondKw(That),
        CondKw(Returns),
        Absent,
        Please,
        End,
        Function,
        Ident(ident("void")),
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
        Ident(ident("print")),
        CondKw(With),
        CondKw(The),
        CondKw(Parameter),
        Ident(ident("_")),
        As,
        Absent,
        CondKw(That),
        CondKw(Returns),
        Absent,
        Please,
        End,
        Function,
        Ident(ident("print")),
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
        Ident(ident("add")),
        CondKw(With),
        CondKw(The),
        CondKw(Parameters),
        Ident(ident("_")),
        As,
        Absent,
        And,
        Ident(ident("_hi")),
        As,
        Null,
        CondKw(That),
        CondKw(Returns),
        Absent,
        Please,
        End,
        Function,
        Ident(ident("add")),
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
        Ident(ident("add")),
        CondKw(With),
        CondKw(The),
        CondKw(Parameters),
        Ident(ident("_")),
        As,
        Absent,
        Comma,
        Ident(ident("a")),
        As,
        NoValue,
        And,
        Ident(ident("_hi")),
        As,
        Null,
        CondKw(That),
        CondKw(Returns),
        Absent,
        Please,
        CondKw(Add),
        Int(5),
        CondKw(To),
        Ident(ident("a")),
        Dot,
        Please,
        End,
        Function,
        Ident(ident("add")),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::fn_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn multiple_body_statements() {
    let tokens = [
        Please,
        CondKw(Add),
        Int(5),
        CondKw(To),
        Int(6),
        Dot,
        Please,
        CondKw(Go),
        CondKw(To),
        CondKw(Sleep),
        Dot,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::body);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn cond_keyword_var_name() {
    let tokens = [
        Initialize,
        Variable,
        CondKw(Add),
        As,
        Ident(ident("string")),
        CondKw(With),
        CondKw(The),
        CondKw(Value),
        CondKw(Of),
        String("Ferris".to_string()),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::var_init);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_no_field() {
    let tokens = [
        Define,
        Structure,
        Ident(ident("Unit")),
        CondKw(With),
        Please,
        End,
        Define,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::struct_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_single_field() {
    let tokens = [
        Define,
        Structure,
        Ident(ident("NonNullInt")),
        CondKw(With),
        CondKw(The),
        CondKw(Field),
        Ident(ident("inner")),
        As,
        Ident(ident("Integer")),
        Please,
        End,
        Define,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::struct_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_two_fields() {
    let tokens = [
        Define,
        Structure,
        Ident(ident("Person")),
        CondKw(With),
        CondKw(The),
        CondKw(Field),
        Ident(ident("name")),
        As,
        Ident(ident("String")),
        And,
        CondKw(The),
        CondKw(Field),
        Ident(ident("age")),
        As,
        Ident(ident("Integer")),
        Please,
        End,
        Define,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::struct_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_three_fields() {
    let tokens = [
        Define,
        Structure,
        Ident(ident("Person")),
        CondKw(With),
        CondKw(The),
        CondKw(Field),
        Ident(ident("first_name")),
        As,
        Ident(ident("String")),
        Comma,
        CondKw(The),
        CondKw(Field),
        Ident(ident("last_name")),
        As,
        Ident(ident("String")),
        And,
        CondKw(The),
        CondKw(Field),
        Ident(ident("age")),
        As,
        Ident(ident("Integer")),
        Please,
        End,
        Define,
    ]
    .map(token);
    let parsed = parse(tokens, Parser::struct_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_literal_no_fields() {
    let tokens = [
        Ident(ident("Unit")),
        CondKw(With),
        CondKw(No),
        CondKw(Fields),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::struct_literal);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_literal_one_field() {
    let tokens = [
        Ident(ident("NonZeroInt")),
        CondKw(With),
        CondKw(The),
        CondKw(Field),
        Int(3),
        As,
        Ident(ident("inner")),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::struct_literal);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_literal_two_fields() {
    let tokens = [
        Ident(ident("Person")),
        CondKw(With),
        CondKw(The),
        CondKw(Fields),
        Ident(ident("Hugo")),
        As,
        Ident(ident("name")),
        And,
        Int(5),
        As,
        Ident(ident("age")),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::struct_literal);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_literal_three_fields() {
    let tokens = [
        Ident(ident("Person")),
        CondKw(With),
        CondKw(The),
        CondKw(Fields),
        Ident(ident("Hugo")),
        As,
        Ident(ident("first_name")),
        Comma,
        Ident(ident("Boss")),
        As,
        Ident(ident("first_name")),
        And,
        Int(5),
        As,
        Ident(ident("age")),
    ]
    .map(token);
    let parsed = parse(tokens, Parser::struct_literal);

    insta::assert_debug_snapshot!(parsed);
}
