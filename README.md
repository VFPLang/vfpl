# vfpl

Pronounced "Veepl", the f is silent  

A politely verbose programming language for building next-gen reliable applications

# Syntax

```
please initialize variable x as Integer with the value of 15.
please set the variable X to the value of 5.
please add 5 to X.

please create function print_x with the parameter x as Integer that returns absent.
    please print x to the console.
    please return absent from the function.
please end function print_x.

please call print_x with the argument x as x.

please create function add with the parameters a as Integer and b as Integer that returns Integer.
    please add b to A.
    please return a from the function.
please end function add.

please initialize variable add_result as Integer with the value of 
    (call add with the arguments 5 as a and 8 as b).

please repeat while 5 is smaller than 6 do
    please print "I want to stop!!!" to the console.
    please break out of this while.
please end while.

please go to sleep.
```

VFPL has null value*s*

```
please initialize variable x1 as Integer with the value of null.
please initialize variable x2 as String with the value of undefined.
please initialize variable x3 as Boolean with the value of absent.
please initialize variable x4 as Boolean with the value of no value.

please check whether x1 has the value absent, if it does please do
    please print "oh oh" to the console.
otherwise, check whether x1 has the value 5, if it does please do
    please print ":)" to the console.
otherwise, do
    please print "uwu" to the console.
please end check.

please check whether x2 has the value undefined, if it does please do
    please print "yes yes" to the console.
please end check.

<!--
this is a comment.
this is pain.
-->

please 4.
```

The language is case-insensitive, but it's a convention to refer to them in UPPERCASE when mutating them.

`please go to sleep.` terminates the program

If the program ends but is not explicitly terminated, the Interpreter hangs and reminds the user of it's presence (the interpreter never sleeps unless asked to)

Indentation is *strongly* recommend

It's recommended to wrap subexpression in parentheses (())

It's recommended to write "True" and "False"

The interpreter suggests returning completely random values from funcitons if you forget the return

```
please create function void with no parameters that returns absent.

please end function void.
```

`[..] I thought about your intentions, and cam to the conclusion that you probably want to return 5 from the function. I suggest you add it.`

# note
we are aware that this language is lacking any way to structure data together, and we are working on addressing this problem.
we're currently working on making the fizzbuzzable mvp
