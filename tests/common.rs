use std::cell::RefCell;
use std::rc::Rc;
use vfpl::Vm;
use vfpl_global::GlobalCtx;

///
/// Runs a test case, asserts that it doesn't work and returns the output of the error formatting
pub fn _err_test(code: &str) -> String {
    let global_ctx = GlobalCtx::test_ctx();
    let mut stderr = Vec::new();

    match vfpl::lex(code, global_ctx.clone()) {
        Err(err) => {
            vfpl::display_error(code, err, &mut stderr, false, global_ctx).unwrap();
        }
        Ok(tokens) => match vfpl::parse(tokens.into_iter(), global_ctx.clone()) {
            Err(err) => {
                vfpl::display_error(code, err, &mut stderr, false, global_ctx).unwrap();
            }
            Ok(ast) => match vfpl::run(&ast, global_ctx.clone()) {
                Err(err) => {
                    vfpl::display_error(code, err, &mut stderr, false, global_ctx).unwrap();
                }
                Ok(_) => panic!("Program did not fail!"),
            },
        },
    }

    String::from_utf8(stderr).expect("Invalid UTF-8")
}

///
/// Runs a test case and returns the stdout of the interpreter
///
/// Fails if there is an error
pub fn _run_test(code: &str) -> String {
    let global_ctx = GlobalCtx::test_ctx();

    let tokens = vfpl::lex(code, global_ctx.clone()).unwrap();

    let ast = vfpl::parse(tokens.into_iter(), global_ctx.clone()).unwrap();

    let stdout_vec = Rc::new(RefCell::new(Vec::new()));

    let stdout = Rc::clone(&stdout_vec);
    let mut vm = Vm::with_stdout(stdout, global_ctx);
    vm.run(&ast).unwrap();

    let cloned_vec = RefCell::borrow(&stdout_vec).clone();

    String::from_utf8(cloned_vec).unwrap()
}

#[macro_export]
macro_rules! vfpl_error_test {
    ($name:ident, $code:expr) => {
        #[test]
        fn $name() {
            let code = $code;
            let stderr = common::_err_test(code);
            insta::assert_display_snapshot!(stderr);
        }
    };
    (ignore, $name:ident, $code:expr) => {
        #[test]
        #[ignore]
        fn $name() {
            let code = $code;
            let stderr = common::_err_test(code);
            insta::assert_display_snapshot!(stderr);
        }
    };
}

#[macro_export]
macro_rules! vfpl_run_test {
    ($name:ident, $code:expr) => {
        #[test]
        fn $name() {
            let code = $code;
            let stderr = common::_run_test(code);
            insta::assert_display_snapshot!(stderr);
        }
    };
    (ignore, $name:ident, $code:expr) => {
        #[test]
        #[ignore]
        fn $name() {
            let code = $code;
            let stderr = common::_run_test(code);
            insta::assert_display_snapshot!(stderr);
        }
    };
}
