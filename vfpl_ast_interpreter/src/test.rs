use crate::Vm;
use std::cell::RefCell;
use std::rc::Rc;
use vfpl_ast::{
    Body, Call, CallArg, CallArgs, Expr, Literal, LiteralKind, Program, Stmt, Ty, TyKind,
    TypedIdent, VarInit,
};
use vfpl_error::Span;
use vfpl_global::Session;

/// Runs the code and returns stdout
fn run_code(ast: &Program) -> String {
    let vec = Rc::new(RefCell::new(Vec::new()));

    let stdout = Rc::clone(&vec);

    let mut vm = Vm::with_stdout(stdout, Session::test_session());
    vm.start(ast).unwrap();

    let cloned_vec = RefCell::borrow(&vec);

    String::from_utf8((cloned_vec).clone()).unwrap()
}

fn var_init(name: &str, ty: TyKind, val: LiteralKind) -> Stmt {
    Stmt::VarInit(VarInit {
        span: Default::default(),
        name: TypedIdent {
            span: Default::default(),
            name: name.to_string(),
            ty: Ty {
                span: Default::default(),
                kind: ty,
            },
        },
        init: Expr::Literal(Literal {
            span: Default::default(),
            kind: val,
        }),
    })
}

fn println(name: &str) -> Stmt {
    Stmt::Expr(Expr::Call(Call {
        span: Default::default(),
        fn_name: "println".to_string(),
        args: CallArgs {
            span: Default::default(),
            args: vec![CallArg {
                span: Default::default(),
                expr: Expr::Literal(Literal {
                    span: Default::default(),
                    kind: LiteralKind::Ident(name.to_string()),
                }),
                name: "x".to_string(),
            }],
        },
    }))
}

#[test]
fn initialize_global_variable() {
    let ast = Body {
        span: Span::default(),
        stmts: vec![
            var_init("uwu", TyKind::Integer, LiteralKind::Int(5)),
            println("uwu"),
        ],
    };

    let state = run_code(&ast);

    insta::assert_debug_snapshot!(state);
}

#[test]
fn double_initialize_global_variable() {
    let ast = Body {
        span: Span::default(),
        stmts: vec![
            var_init("uwu", TyKind::Integer, LiteralKind::Int(5)),
            var_init("uwu", TyKind::Absent, LiteralKind::Absent),
            println("uwu"),
        ],
    };

    let state = run_code(&ast);

    insta::assert_debug_snapshot!(state);
}
