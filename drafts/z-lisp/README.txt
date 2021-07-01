In April 2013, I started working on a small lisp implementation,
but I abandoned it almost immediately.

In June 2021, I started writing a new lisp from scratch.
The most basic stuff is implemented: functions, macros,
quasiquotes, etc. Dynamically loading shared libraries and
calling functions from them is also possible.

To run a lisp script: `target/zlisp-cli <path-to-the-script>`.

There is a basic repl: `target/zlisp-cli src/main/repl.lisp`
