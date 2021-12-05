mod common;

vfpl_error_test!(unterminated_string_literal, r#""hello"#);

vfpl_error_test!(
    statement_without_dot,
    "please initialize variable x as Integer with the value of 15"
);

vfpl_error_test!(add_expr_no_rhs, "please add 5 to");

vfpl_error_test!(
    unclosed_check_stmt,
    "
    please check whether 5 has the value absent, then do
        please add 5 to 6.
"
);

vfpl_error_test!(
    incomplete_check_close_stmt,
    "
    please check whether 5 has the value absent, then do
        please add 5 to 6.
    please end
"
);
