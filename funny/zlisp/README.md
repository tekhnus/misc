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

In April 2013, I started working on a small lisp implementation,
but I abandoned it almost immediately.

In June 2021, I started writing a new lisp from scratch.
The most basic stuff is implemented: functions, macros,
quasiquotes, etc. Dynamically loading shared libraries and
calling functions from them is also possible.
