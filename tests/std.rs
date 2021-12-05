mod common;

vfpl_run_test!(
    print_different_types,
    r#"
    
    please create function void with no parameters that returns absent
        please return absent from the function.
    please end function void.
    
    please call println with the argument True as x.
    please call println with the argument False as x.
    please call println with the argument 532 as x.
    please call println with the argument -5532 as x.
    please call println with the argument -3245.435 as x.
    please call println with the argument 0.435 as x.
    please call println with the argument "test" as x.
    please call println with the argument absent as x.
    please call println with the argument null as x.
    please call println with the argument undefined as x.
    please call println with the argument novalue as x.
    please call println with the argument println as x.
    please call println with the argument void as x.
    
    please go to sleep.
    "#
);

vfpl_run_test!(
    time_without_error,
    "
    please initialize variable our_time as Integer with the value of (call time with no arguments).
    <!-- we cannot print the value here, we have to trust that it returned the correct time. -->
    please go to sleep.
    "
);
