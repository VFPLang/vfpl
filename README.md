# vfpl

Pronounced "Veepl", the f is silent  

A politely verbose programming language for building next-gen reliable applications

See the docs on [https://vfplang.github.io/docs/](https://vfplang.github.io/docs/)

# Examples

## Hello World

```
please call println with the argument "Hello, World!" as x.

please go to sleep.
```

## Fizzbuzz

```
please initialize variable Number as Integer with the value of 1.
    
please repeat while Number is less or equal than 100 do
    please check whether (take number modulo 15) has the value 0, then do
        please call println with the argument "FizzBuzz" as x.
    otherwise, check whether (take number modulo 5) has the value 0, then do
        please call println with the argument "Buzz" as x.
    otherwise, check whether (take number modulo 3) has the value 0, then do
        please call println with the argument "Fizz" as x.
    otherwise,
        please call println with the argument number as x.
    please end check.

    please set the variable Number to the value of (add 1 to Number).
please end while.

please go to sleep.
```

# Features

* Polite and helpful error messages
* A clear and unambigious syntax
* 4 null values
* Case-insensitivity
