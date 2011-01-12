=== Introduction

This project present a test suite and two implementations of  Lisp-1 kernel interpreter.

The first implementation I use a more classic approach using regular function. 

In the second implementation I encapsulate the components. To do it, I hide the details implementations in the closure scope, and use a dispatcher to route the method call. As the old koan said "Objects are the poor man closures" [1]

Happy hacking.

[1] http://okmij.org/ftp/Scheme/oop-in-fp.txt
