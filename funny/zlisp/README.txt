In April 2013, I started working on a small lisp implementation,
but I abandoned it almost immediately.

In June 2021, I started writing a new lisp from scratch.
The most basic stuff is implemented: functions, macros,
quasiquotes, etc. Dynamically loading shared libraries and
calling functions from them is also possible.

To build and run the REPL:
```
cmake -B build -S .
cmake -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=1
cmake --build build
cmake --build build --target test ARGS=-V
build/zlisp-cli-bootstrap/zlisp-run module/cli/main.lisp
```
