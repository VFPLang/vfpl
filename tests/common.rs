///
/// Runs a test case, asserts that it doesn't work and returns the output of the error formatting
pub fn run_err_test(code: &str) -> String {
    let mut stderr = Vec::new();

    match vfpl::lex(code) {
        Err(err) => {
            vfpl::display_error(code, err, &mut stderr, false).unwrap();
        }
        Ok(tokens) => match vfpl::parse(tokens.into_iter()) {
            Err(err) => {
                vfpl::display_error(code, err, &mut stderr, false).unwrap();
            }
            Ok(ast) => match vfpl::run(ast) {
                Err(err) => {
                    vfpl::display_error(code, err, &mut stderr, false).unwrap();
                }
                Ok(_) => panic!("Program did not fail!"),
            },
        },
    }

    String::from_utf8(stderr).expect("Invalid UTF-8")
}

#[macro_export]
macro_rules! vfpl_test {
    ($name:ident, $code:expr) => {
        #[test]
        fn $name() {
            let code = $code;
            let stderr = common::run_err_test(code);
            insta::assert_display_snapshot!(stderr);
        }
    };
}
