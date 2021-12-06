mod common;

vfpl_error_test!(
    variable_init_number_name,
    "please initialize variable 5 as Integer with the value of 15."
);

vfpl_run_test!(
    cond_keywords_in_ident_names,
    "
    please initialize variable add as Integer with the value of 10.
    please initialize variable sub as Integer with the value of 10.
    please initialize variable mul as Integer with the value of 10.
    please initialize variable div as Integer with the value of 10.
    please initialize variable mod as Integer with the value of 10.
    please initialize variable with as Integer with the value of 10.
    please initialize variable the as Integer with the value of 10.
    please initialize variable value as Integer with the value of 10.
    please initialize variable of as Integer with the value of 10.
    please initialize variable set as Integer with the value of 10.
    please initialize variable to as Integer with the value of 10.
    please initialize variable from as Integer with the value of 10.
    please initialize variable by as Integer with the value of 10.
    please initialize variable take as Integer with the value of 10.
    please initialize variable out as Integer with the value of 10.
    please initialize variable parameter as Integer with the value of 10.
    please initialize variable parameters as Integer with the value of 10.
    please initialize variable that as Integer with the value of 10.
    please initialize variable returns as Integer with the value of 10.
    please initialize variable no as Integer with the value of 10.
    please initialize variable argument as Integer with the value of 10.
    please initialize variable arguments as Integer with the value of 10.
    please initialize variable go as Integer with the value of 10.
    please initialize variable sleep as Integer with the value of 10.
    please initialize variable does as Integer with the value of 10.
    please initialize variable has as Integer with the value of 10.
    please initialize variable is as Integer with the value of 10.
    please initialize variable have as Integer with the value of 10.
    please initialize variable greater as Integer with the value of 10.
    please initialize variable less as Integer with the value of 10.
    please initialize variable than as Integer with the value of 10.
    please initialize variable equal as Integer with the value of 10.

    please go to sleep.
    "
);

// deny normal keywords

vfpl_error_test!(
    deny_keyword_please_as_ident_name,
    "please initialize variable please as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_as_as_ident_name,
    "please initialize variable as as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_initialize_as_ident_name,
    "please initialize variable initialize as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_variable_as_ident_name,
    "please initialize variable variable as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_end_as_ident_name,
    "please initialize variable end as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_check_as_ident_name,
    "please initialize variable check as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_whether_as_ident_name,
    "please initialize variable whether as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_then_as_ident_name,
    "please initialize variable then as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_do_as_ident_name,
    "please initialize variable do as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_otherwise_as_ident_name,
    "please initialize variable otherwise as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_break_as_ident_name,
    "please initialize variable break as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_this_as_ident_name,
    "please initialize variable this as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_create_as_ident_name,
    "please initialize variable create as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_function_as_ident_name,
    "please initialize variable function as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_call_as_ident_name,
    "please initialize variable call as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_and_as_ident_name,
    "please initialize variable and as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_absent_as_ident_name,
    "please initialize variable absent as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_null_as_ident_name,
    "please initialize variable null as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_novalue_as_ident_name,
    "please initialize variable novalue as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_undefined_as_ident_name,
    "please initialize variable undefined as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_true_as_ident_name,
    "please initialize variable true as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_false_as_ident_name,
    "please initialize variable false as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_not_as_ident_name,
    "please initialize variable not as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_or_as_ident_name,
    "please initialize variable or as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_repeat_as_ident_name,
    "please initialize variable repeat as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_return_as_ident_name,
    "please initialize variable return as Integer with the value of 10."
);
vfpl_error_test!(
    deny_keyword_while_as_ident_name,
    "please initialize variable while as Integer with the value of 10."
);
