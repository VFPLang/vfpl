# vfpl

Pronounced "Veepl", the f is silent  

A politely verbose programming language for building next-gen reliable applications

See the docs on [https://vfplang.github.io/docs/](https://vfplang.github.io/docs/)

# Syntax

```
please initialize variable X as Integer with the value of 15.
please set the variable X to the value of 5.
please set the variable X to the value of (add 5 to X).

please create function print_x with the parameter x as Integer that returns absent
    please call println with the argument x as x.
    please return absent from the function.
please end function print_x.

please call print_x with the argument x as x.

please create function add with the parameters A as Integer and b as Integer that returns Integer
    please set the variable A to the value (add b to A).
    please return a from the function.
please end function add.

please initialize variable add_result as Integer with the value of
    (call add with the arguments 5 as a and 8 as b).

please repeat while 5 is smaller than 6 do
    please call println with the argument "I want to stop!!!" as x.
    please break out of this while.
please end while.

please go to sleep.
```

VFPL has null value*s*

```
please initialize variable x1 as Integer with the value of 1.
please initialize variable x2 as String with the value of "undefined".
please initialize variable x3 as Boolean with the value of True.
please initialize variable x4 as Novalue with the value of noValue.

please check whether x1 has the value absent, then do
    please call println with the argument "oh oh" as x.
otherwise, check whether x1 has the value 5, then do
    please call println with the argument ":)" as x.
otherwise,
    please call println with the argument "uwu" as x.
please end check.

please check whether x2 has the value undefined, then do
    please call println with the argument "yes yes" as x.
please end check.

<!--
this is a comment.
this is pain.
-->

please 4.
```

The language is case-insensitive, but it's a convention to refer to variables in Uppercase when mutating them.

`please initialize the variable x as Integer with the value of 5.`  
`please add 1 to X.`

`please go to sleep.` terminates the program

If the program ends but is not explicitly terminated, the Interpreter hangs and reminds the user of it's presence (the interpreter never sleeps unless asked to)

Indentation is *strongly* recommend

It's recommended to wrap subexpression in parentheses (())

It's recommended to write "True" and "False"

The interpreter suggests returning completely random values from funcitons if you forget the return

```
please create function void with no parameters that returns absent

please end function void.
```

`[..] I thought about your intentions, and came to the conclusion that you probably want to return 5 from the function. I suggest you add it.`

bug: invalid handle of string literals

# note
we are aware that this language is lacking any way to structure data together, and we are working on addressing this problem.
we're currently working on making the fizzbuzzable mvp


# Reference

## Native global functions

Prints a value to stdout

`please create function print with the parameter x as <Any> that returns absent`