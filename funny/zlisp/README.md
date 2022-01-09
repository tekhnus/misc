**zlisp**.

To build:
```
cmake -B build
cmake --build build
```

To run the repl build and then run:
```
zlisp-run module/cli/main.lisp
```

To run the tests build and then run:
```
cmake --build build --target test ARGS=-V
```

Almost all header files are generated.
To regenerate them, run
```
cmake --build build --target codegen
```

To (re)generate the compilation database:
```
cmake -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=1
```

Mind that a correct environment is needed for some of those commands.
The `.envrc` file takes care of that.

History:

April 2013: started working on a small lisp implementation,
but abandoned it almost immediately.

June 2021: started writing a new lisp from scratch.
The most basic stuff is done: functions, macros,
quasiquotes, etc. Also, dynamic loading of shared libraries and
calling functions from them is implemented.

August 2021: coroutines are added.
In order to achieve that, the evaluation is completely
redesigned and mostly separated into two phases:
compilation into a bytecode for a simple stack machine
and bytecode evaluation.
Classical macros are replaced by preprocessing.

January 2022: required modules and subroutines
are now compiled at compile-time instead of run-time;
thus, bytecode compilation and evaluation are completely separated.
The frontend (perfming module searching and source code preprocessing)
is separated from the compiler.
The base language, the bytecode and the process of compiling
the former into the latter are now host-agnostic.
