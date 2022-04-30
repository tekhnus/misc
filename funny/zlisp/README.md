**zlisp**.

To build and test:
```
./scripts/build-and-test
```

After this you can run the repl:
```
ZLISP=./module ./build/host-ffi/zlisp-run module/cli/main.lisp
```

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

April 2022: the pointer type is eliminated in favour of plain integers;
the FFI-related functions are moved from the base implementation to host implementation
and are significantly refactored.
Bytecode generator now writes the whole program onto a linear memory slice.
Bytecode-to-datum converter is implemented.
Python interpreter is implemented.
