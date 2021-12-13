//// helpers

use super::Parser;
use peekmore::PeekMore;
use vfpl_error::Span;
use vfpl_global::{GlobalCtx, SpurCtx};
use vfpl_lexer::tokens::CondKeyword::*;
use vfpl_lexer::tokens::TokenKind::*;
use vfpl_lexer::tokens::{Token, TokenKind};

impl Parser {
    fn intern(&mut self, str: &str) -> SpurCtx {
        let spur = self.global_ctx.borrow_mut().intern_string(str);
        SpurCtx::new(spur, self.global_ctx.clone())
    }
}

fn token(kind: TokenKind) -> Token {
    Token {
        span: Span::dummy(),
        kind,
    }
}

fn parser() -> Parser {
    let parser = Parser::new(vec![].into_iter(), GlobalCtx::test_ctx());
    parser
}

/// parses CondKw(The) tokens and appends an EOF token
fn parse<T, F, R>(mut parser: Parser, tokens: T, parse_rule_fn: F) -> R
where
    T: Into<Vec<Token>>,
    F: FnOnce(&mut Parser) -> R,
{
    let mut vec = tokens.into();
    vec.push(Token::eof());
    parser.tokens = vec.into_iter().peekmore();
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
    let parser = parser();
    let tokens = [String("string".to_string())].map(token);
    let parsed = parse(parser, tokens, Parser::literal);
    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn different_literals() {
    let parser = parser();
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

    let parsed = literals.map(|tokens| parse(parser.clone(), tokens, Parser::literal));

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn ident_ty() {
    let mut parser = parser();
    let tokens = [Ident(parser.intern("Test"))].map(token);
    let parsed = parse(parser, tokens, Parser::ty);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn nullable_ty() {
    let parser = parser();
    let tys = [
        [Absent].map(token),
        [NoValue].map(token),
        [Null].map(token),
        [Undefined].map(token),
    ];
    let parsed = tys.map(|tokens| parse(parser.clone(), tokens, Parser::ty));

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn other_tys() {
    let mut parser = parser();
    let tys = [
        [Ident(parser.intern("Boolean"))].map(token),
        [Ident(parser.intern("Integer"))].map(token),
        [Ident(parser.intern("String"))].map(token),
        [Ident(parser.intern("Float"))].map(token),
    ];
    let parsed = tys.map(|tokens| parse(parser.clone(), tokens, Parser::ty));

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn literal_call_expr() {
    let parser = parser();
    let tokens = [String("string".to_string())].map(token);
    let parsed = parse(parser, tokens, Parser::call_expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn literal_expr() {
    let parser = parser();
    let tokens = [String("string".to_string())].map(token);
    let parsed = parse(parser, tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn ident_literal() {
    let mut parser = parser();
    let tokens = [Ident(parser.intern("uwu"))].map(token);
    let parsed = parse(parser, tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn ident_cond_keyword_literal() {
    let parser = parser();
    let tokens = [CondKw(From)].map(token);
    let parsed = parse(parser, tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn not_equal_expr() {
    let parser = parser();
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
    let parsed = parse(parser, tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn equal_expr() {
    let parser = parser();
    let tokens = [Absent, CondKw(Has), CondKw(The), CondKw(Value), Absent].map(token);
    let parsed = parse(parser, tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn greater_than_expr() {
    let parser = parser();
    let tokens = [Absent, CondKw(Is), CondKw(Greater), CondKw(Than), Undefined].map(token);
    let parsed = parse(parser, tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn less_than_expr() {
    let parser = parser();
    let tokens = [Absent, CondKw(Is), CondKw(Less), CondKw(Than), Undefined].map(token);
    let parsed = parse(parser, tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn greater_equal_than_expr() {
    let parser = parser();
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
    let parsed = parse(parser, tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn less_equal_than_expr() {
    let parser = parser();
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
    let parsed = parse(parser, tokens, Parser::expr);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn typed_ident_absent() {
    let mut parser = parser();
    let tokens = [Ident(parser.intern("name")), As, Absent].map(token);
    let parsed = parse(parser, tokens, Parser::typed_ident);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn init_variable_string() {
    let mut parser = parser();
    let tokens = [
        Initialize,
        Variable,
        Ident(parser.intern("name")),
        As,
        Ident(parser.intern("string")),
        CondKw(With),
        CondKw(The),
        CondKw(Value),
        CondKw(Of),
        String("Ferris".to_string()),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::var_init);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn set_variable_string() {
    let mut parser = parser();
    let tokens = [
        CondKw(Set),
        CondKw(The),
        Variable,
        Ident(parser.intern("name")),
        CondKw(To),
        CondKw(The),
        CondKw(Value),
        CondKw(Of),
        String("Ferris".to_string()),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::var_set);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn add_number() {
    let mut parser = parser();
    let tokens = [
        CondKw(Add),
        Int(0),
        CondKw(To),
        Ident(parser.intern("counter")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::term);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn subtract_number() {
    let mut parser = parser();
    let tokens = [
        CondKw(Sub),
        Int(1),
        CondKw(From),
        Ident(parser.intern("counter")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::term);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn divide_number() {
    let mut parser = parser();
    let tokens = [
        CondKw(Div),
        Ident(parser.intern("counter")),
        CondKw(By),
        Int(2),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::factor);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn multiply_number() {
    let mut parser = parser();
    let tokens = [
        CondKw(Mul),
        Ident(parser.intern("counter")),
        CondKw(With),
        Int(3),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::factor);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn mod_number() {
    let mut parser = parser();
    let tokens = [
        CondKw(Take),
        Ident(parser.intern("counter")),
        CondKw(Mod),
        Int(4),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::factor);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn nested_add_number() {
    let parser = parser();
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
    let parsed = parse(parser, tokens, Parser::term);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn terminate() {
    let parser = parser();
    let tokens = [CondKw(Go), CondKw(To), CondKw(Sleep)].map(token);
    let parsed = parse(parser, tokens, Parser::terminate);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_no_args() {
    let mut parser = parser();
    let tokens = [
        Call,
        Ident(parser.intern("run")),
        CondKw(With),
        CondKw(No),
        CondKw(Arguments),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_single_arg() {
    let mut parser = parser();
    let tokens = [
        Call,
        Ident(parser.intern("print")),
        CondKw(With),
        CondKw(The),
        CondKw(Argument),
        Int(0),
        As,
        Ident(parser.intern("printable")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_two_args() {
    let mut parser = parser();
    let tokens = [
        Call,
        Ident(parser.intern("add")),
        CondKw(With),
        CondKw(The),
        CondKw(Arguments),
        Int(2),
        As,
        Ident(parser.intern("a")),
        And,
        Int(3),
        As,
        Ident(parser.intern("b")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn call_three_args() {
    let mut parser = parser();
    let tokens = [
        Call,
        Ident(parser.intern("ternary")),
        CondKw(With),
        CondKw(The),
        CondKw(Arguments),
        True,
        As,
        Ident(parser.intern("cond")),
        Comma,
        Int(0),
        As,
        Ident(parser.intern("then")),
        And,
        Int(3),
        As,
        Ident(parser.intern("else")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::call);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn add_stmt() {
    let mut parser = parser();
    let tokens = [
        Please,
        CondKw(Add),
        Int(0),
        CondKw(To),
        Ident(parser.intern("A")),
        Dot,
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn if_empty_body_true_cond() {
    let parser = parser();
    let tokens = [Check, Whether, True, Comma, Then, Do, Please, End, Check].map(token);
    let parsed = parse(parser, tokens, Parser::if_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn if_empty_body_complex_cond() {
    let parser = parser();
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
    let parsed = parse(parser, tokens, Parser::if_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn if_single_stmt_body() {
    let parser = parser();
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
    let parsed = parse(parser, tokens, Parser::if_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn if_multi_stmt_body() {
    let mut parser = parser();
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
        Ident(parser.intern("A")),
        Dot,
        Please,
        End,
        Check,
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::if_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn return_number() {
    let parser = parser();
    let tokens = [Return, Int(9), CondKw(From), CondKw(The), Function].map(token);
    let parsed = parse(parser, tokens, Parser::return_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn return_stmt() {
    let parser = parser();
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
    let parsed = parse(parser, tokens, Parser::stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn r#break() {
    let parser = parser();
    let tokens = [Break, CondKw(Out), CondKw(Of), This, While].map(token);
    let parsed = parse(parser, tokens, Parser::break_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn break_stmt() {
    let parser = parser();
    let tokens = [Please, Break, CondKw(Out), CondKw(Of), This, While, Dot].map(token);
    let parsed = parse(parser, tokens, Parser::stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn while_loop_no_body() {
    let parser = parser();
    let tokens = [Repeat, While, True, Do, Please, End, While].map(token);
    let parsed = parse(parser, tokens, Parser::while_stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn while_stmt_with_body() {
    let mut parser = parser();
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
        Ident(parser.intern("A")),
        Dot,
        Please,
        End,
        While,
        Dot,
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::stmt);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn return_ty_absent() {
    let parser = parser();
    let tokens = [CondKw(That), CondKw(Returns), Absent].map(token);
    let parsed = parse(parser, tokens, Parser::fn_return);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn fn_decl_no_params_empty_body() {
    let mut parser = parser();
    let tokens = [
        Create,
        Function,
        Ident(parser.intern("void")),
        CondKw(With),
        CondKw(No),
        CondKw(Parameters),
        CondKw(That),
        CondKw(Returns),
        Absent,
        Please,
        End,
        Function,
        Ident(parser.intern("void")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::fn_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn fn_decl_single_param_emtpy_body() {
    let mut parser = parser();
    let tokens = [
        Create,
        Function,
        Ident(parser.intern("print")),
        CondKw(With),
        CondKw(The),
        CondKw(Parameter),
        Ident(parser.intern("_")),
        As,
        Absent,
        CondKw(That),
        CondKw(Returns),
        Absent,
        Please,
        End,
        Function,
        Ident(parser.intern("print")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::fn_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn fn_decl_two_param_emtpy_body() {
    let mut parser = parser();
    let tokens = [
        Create,
        Function,
        Ident(parser.intern("add")),
        CondKw(With),
        CondKw(The),
        CondKw(Parameters),
        Ident(parser.intern("_")),
        As,
        Absent,
        And,
        Ident(parser.intern("_hi")),
        As,
        Null,
        CondKw(That),
        CondKw(Returns),
        Absent,
        Please,
        End,
        Function,
        Ident(parser.intern("add")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::fn_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn fn_decl_three_param_with_body() {
    let mut parser = parser();
    let tokens = [
        Create,
        Function,
        Ident(parser.intern("add")),
        CondKw(With),
        CondKw(The),
        CondKw(Parameters),
        Ident(parser.intern("_")),
        As,
        Absent,
        Comma,
        Ident(parser.intern("a")),
        As,
        NoValue,
        And,
        Ident(parser.intern("_hi")),
        As,
        Null,
        CondKw(That),
        CondKw(Returns),
        Absent,
        Please,
        CondKw(Add),
        Int(5),
        CondKw(To),
        Ident(parser.intern("a")),
        Dot,
        Please,
        End,
        Function,
        Ident(parser.intern("add")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::fn_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn multiple_body_statements() {
    let parser = parser();
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
    let parsed = parse(parser, tokens, Parser::body);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn cond_keyword_var_name() {
    let mut parser = parser();
    let tokens = [
        Initialize,
        Variable,
        CondKw(Add),
        As,
        Ident(parser.intern("string")),
        CondKw(With),
        CondKw(The),
        CondKw(Value),
        CondKw(Of),
        String("Ferris".to_string()),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::var_init);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_no_field() {
    let mut parser = parser();
    let tokens = [
        Define,
        Structure,
        Ident(parser.intern("Unit")),
        CondKw(With),
        Please,
        End,
        Define,
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::struct_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_single_field() {
    let mut parser = parser();
    let tokens = [
        Define,
        Structure,
        Ident(parser.intern("NonNullInt")),
        CondKw(With),
        CondKw(The),
        CondKw(Field),
        Ident(parser.intern("inner")),
        As,
        Ident(parser.intern("Integer")),
        Please,
        End,
        Define,
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::struct_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_two_fields() {
    let mut parser = parser();
    let tokens = [
        Define,
        Structure,
        Ident(parser.intern("Person")),
        CondKw(With),
        CondKw(The),
        CondKw(Field),
        Ident(parser.intern("name")),
        As,
        Ident(parser.intern("String")),
        And,
        CondKw(The),
        CondKw(Field),
        Ident(parser.intern("age")),
        As,
        Ident(parser.intern("Integer")),
        Please,
        End,
        Define,
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::struct_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_three_fields() {
    let mut parser = parser();
    let tokens = [
        Define,
        Structure,
        Ident(parser.intern("Person")),
        CondKw(With),
        CondKw(The),
        CondKw(Field),
        Ident(parser.intern("first_name")),
        As,
        Ident(parser.intern("String")),
        Comma,
        CondKw(The),
        CondKw(Field),
        Ident(parser.intern("last_name")),
        As,
        Ident(parser.intern("String")),
        And,
        CondKw(The),
        CondKw(Field),
        Ident(parser.intern("age")),
        As,
        Ident(parser.intern("Integer")),
        Please,
        End,
        Define,
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::struct_decl);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_literal_no_fields() {
    let mut parser = parser();
    let tokens = [
        Ident(parser.intern("Unit")),
        CondKw(With),
        CondKw(No),
        CondKw(Fields),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::struct_literal);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_literal_one_field() {
    let mut parser = parser();
    let tokens = [
        Ident(parser.intern("NonZeroInt")),
        CondKw(With),
        CondKw(The),
        CondKw(Field),
        Int(3),
        As,
        Ident(parser.intern("inner")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::struct_literal);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_literal_two_fields() {
    let mut parser = parser();
    let tokens = [
        Ident(parser.intern("Person")),
        CondKw(With),
        CondKw(The),
        CondKw(Fields),
        Ident(parser.intern("Hugo")),
        As,
        Ident(parser.intern("name")),
        And,
        Int(5),
        As,
        Ident(parser.intern("age")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::struct_literal);

    insta::assert_debug_snapshot!(parsed);
}

#[test]
fn structure_literal_three_fields() {
    let mut parser = parser();
    let tokens = [
        Ident(parser.intern("Person")),
        CondKw(With),
        CondKw(The),
        CondKw(Fields),
        Ident(parser.intern("Hugo")),
        As,
        Ident(parser.intern("first_name")),
        Comma,
        Ident(parser.intern("Boss")),
        As,
        Ident(parser.intern("first_name")),
        And,
        Int(5),
        As,
        Ident(parser.intern("age")),
    ]
    .map(token);
    let parsed = parse(parser, tokens, Parser::struct_literal);

    insta::assert_debug_snapshot!(parsed);
}
