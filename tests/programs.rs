mod common;

vfpl_run_test!(
    #[ignore],
    fizzbuzz,
    r#"please initialize variable Number as Integer with the value of 1.

    please repeat while Number is less or equal than 100 do
        please check whether (take number modulo 15) is equal to 0, then do
            please print "FizzBuzz" to the console.
        otherwise, check whether (take number modulo 5) is equal to 5, then do
            please print "Buzz" to the console.
        otherwise, check whether (take number modulo 3) is equal to 3, then do
            please print "Fizz" to the console.
        otherwise,
            please print number to the console.
        please end check.
        
        please set the variable Number to the value of (add 1 to Number).
    please end while.

    please go to sleep."#
);
