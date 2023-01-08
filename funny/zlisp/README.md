### About

This is a playground where I'm toying with a small lisp-like language
for educational purposes.

Currently it's barely usable.

Things I'm trying to figure out experimentally:
- How small can an implementation be?
  How much can be internalized, i.e. implemented via macros
  and/or compiler extensions?
- How small can a language specification get?
  Is it a good idea to specify only the most fundamental stuff,
  deferring most of decisions to implementations?
- Can we have imperative syntax and statefulness
  yet be able to reason about side-effects with simplicity
  inherent to pure-functional programs?
- Can we avoid garbage collection and reference counting
  yet still have fun?
- Coroutines are such a great idea, can we have them everywhere?

There is an implementation-agnostic bytecode compiler (written in C)
and two implementations:
- host-ffi: written in C, allows to call native functions via FFI;
- host-python: written in Python, allows to call Python.

### Stuff to be done yet

- Implement loops and/or tail recursive calls
- Implement intercepting arbitrary yields from called routine, for example, host calls

- Unite preprocessor with quasiquotes
- Come up with a less awkward macro syntax
- Implement sanitized macros and/or self-describing identifiers and/or implicit imports

- Achieve zero memory leakage
- Switch from linked lists to slices in FFI implementation
- Make environment handling in FFI impementation error-resistant and zero-cost
- Implement closures by value
- Implement references

- Introduce optional static typing

- Try hosting by a compiled language
- Try hosting by a constrained environment like bare metal or shader

### Building and running

**Mandatory requirements:** C compiler and cmake.

**Optional requirements:** Python 3 with some libraries (`pip install -r requirements.txt`).

**To build and test:**
```
./scripts/build-and-test
```

**To run the repl:**
```
LD_LIBRARY_PATH=./build/builder ./build/host-ffi/zlisp-run <(ZLISP=./module ./build/builder/zlisp-build c-prelude module/cli/main.lisp)
```

### Project structure

```
core/
  types.h           type definitions
  datum.c           manipulating data
  reading.c         parsing data
  compiling.c       a bytecode compiler (implementation-agnostic)
  running.c         a generic interpreter
builder/            a tool for preprocessing, compiling and linking (implementation-agnostic)
host-ffi/           an implementation supporting dlopen() and FFI function calls
host-python/        an implementation supporting Python evaluation
module/             lisp modules
tools/
  makeheaders.c     copied from the Fossil project; used to generate .h-files from .c-files
```
