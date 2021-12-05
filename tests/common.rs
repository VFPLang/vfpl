use std::cell::RefCell;
use std::rc::Rc;
use vfpl::{Session, Vm};

///
/// Runs a test case, asserts that it doesn't work and returns the output of the error formatting
pub fn _err_test(code: &str) -> String {
    let session = Session::test_session();
    let mut stderr = Vec::new();

    match vfpl::lex(code, session.clone()) {
        Err(err) => {
            vfpl::display_error(code, err, &mut stderr, false, session).unwrap();
        }
        Ok(tokens) => match vfpl::parse(tokens.into_iter(), session.clone()) {
            Err(err) => {
                vfpl::display_error(code, err, &mut stderr, false, session).unwrap();
            }
            Ok(ast) => match vfpl::run(&ast, session.clone()) {
                Err(err) => {
                    vfpl::display_error(code, err, &mut stderr, false, session).unwrap();
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
    let session = Session::test_session();

    let tokens = vfpl::lex(code, session.clone()).unwrap();

    let ast = vfpl::parse(tokens.into_iter(), session.clone()).unwrap();

    let stdout_vec = Rc::new(RefCell::new(Vec::new()));

    let stdout = Rc::clone(&stdout_vec);
    let mut vm = Vm::with_stdout(stdout, session);
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
