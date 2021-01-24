# Interpreter

This is a Silly Basic language interpreter in Scheme(a dynamically typed (mostly) functional language). The interpreter will read in an intermediate language program, parse it, and then interpret it. No looping constructs are used, rather tail-end recursion to avoid overloading the call stack.
The interpreter reads in an SBIR program from the file whose name is specified in the argument list, stores it in a list, and then interprets that intermediate representation. During interpretation, numbers are read from the standard input and results written to the standard output.

This is a class project for CMPS 112 at UCSC
