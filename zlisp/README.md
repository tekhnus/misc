### About

This is a playground where I'm toying with a small lisp-like language
for educational purposes.

Currently it's barely usable.

Things I'm trying to figure out experimentally:
- Can a programming language specification be very general yet useful?
  What will we end up with if we'll specify only things which are meaningful
  for any kind of computation, be it bare-metal programs, distributed programs,
  package definitions, algorithms from mathematical logic books or math proofs?
- How small can a language implementation be?
  How much can be internalized, i.e. implemented via
  compiler extensions written in the same language?
- Can we have imperative syntax and statefulness
  yet be able to easily reason about side-effects of any line of the code?
- Can we avoid garbage collection and reference counting
  yet end up with fun-to-use and fast language?
- Coroutines are such a great idea, can we have them everywhere?

There is an implementation-agnostic bytecode compiler (written in C)
and two implementations:
- host-ffi: written in C, allows to call native functions via FFI;
- host-python: written in Python, allows to call Python.

### Stuff to be done yet

- Implement intercepting arbitrary yields from called routine, for example, host calls
- Implement program hashing, e.g. getting a hash of the given computation
- Support parallelism

- Implement sanitized extensions and/or self-describing identifiers and/or implicit imports

- Achieve zero memory leakage
- Get rid of keywords; evaluate them into integers
- Make environment handling in FFI impementation error-resistant and zero-cost
- Implement closures by value
- Implement references

- Introduce optional static typing

- Try hosting by a compiled language
- Try hosting by a constrained environment like bare metal or shader

- Make a DAP-powered debugger
- Make an interceptor which records all host calls and can replay them during debugging
- Make an RPC
- Make a C++ debugger which is scriptable with zlisp
- Make a shell which supports caching, reproducibility and transparent remote execution

### Building and running

**Mandatory requirements:** C compiler and cmake.

**Optional requirements:** Python 3 with some libraries (`pip install -r requirements.txt`).

**To build and test:**
```
./scripts/build-and-test
```

**To run the repl:**
```
LD_LIBRARY_PATH=./build/builder DYLD_LIBRARY_PATH=./build/builder ./build/host-ffi/zlisp-run <(ZLISP=./module ./build/builder/zlisp-build c-prelude module/cli/main.lisp)
```

### Project structure

```
core/
  types.h           type definitions
  datum.c           manipulating data
  reading.c         parsing data
  compiling.c       an extensible compiler (host-agnostic)
  running.c         a generic interpreter
  extending.c       tools for writing compiler extensions in lisp
builder/            a CLI and a library for compiling and linking (host-agnostic)
host-ffi/           an interpreter supporting dlopen() and FFI function calls
host-python/        an interpreter supporting Python evaluation
module/             lisp modules
tools/
  makeheaders.c     copied from the Fossil project; used to generate .h-files from .c-files
```
