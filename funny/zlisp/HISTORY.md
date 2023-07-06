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

May 2022: the routine types are eliminated;
the routines are represented by simple types (a datum with state
and an index of the routine start on the code slice).
The linker is separated from the compiler and diamond dependencies reusage is implemented.

June-October 2022: the runtime now doesn't have "variables"; there is only stack now;
variable offsets are calculated by the compiler.

October 2022-January 2023:
- routine now can have recieve and yield multiple values natively (without packing/unpacking);
- the interpreter is reimplemented:
  - the call structure is now flat (instead of being a stack of stacks)
  - the child frame now lives in the parent frame and is mutated in-place;
- the routine is now a special type in C interpreter again (so that we can copy it correctly);
- there are no more closures; the routine now stores only its own frame;
  the parent frames are explicitly specified during the call and they are borrowed;
  when we need to capture a value, we use coroutines

January-April 2023:
- migrated everything from linked lists to vectors
- C code became more sane; less mallocs and pointers
- host calls and panics became yields
- moved from "preprocessor" model to "compiler extensions" model
- variable assignment implemented
- while loop implemented

May 2023:
- the bytecode is now mostly "linear" instead of "jumping"
- the bytecode is now position-independent
- the syntax is redesigned: the curly brackets are introduced;
  the statements are now not surrounded by brackets
- the auto-formatter is implemented

June 2023:
- error handling is improved; no more early exits
- the source code is now sanitized (except leak sanitizer)
- the FFI is reworked; binary blobs are used on the boundary instead of integers
