**zlisp**.

To build and test:
```
./scripts/build-and-test
```

After this you can run the repl:
```
ZLISP=./module LD_LIBRARY_PATH=./build/builder ./build/host-ffi/zlisp-run <(./build/builder/zlisp-build c-prelude module/cli/main.lisp)
```

Project structure:
```
core/
  types.h           type definitions
  datum.c           manipulating data
  reading.c         parsing data
  compiling.c       compiling lisp to bytecode
  running.c         a generic interpreter
host-ffi/
  main.c            an interpreter supporting dlopen() and FFI function calls
host-python/
  zlisp-run-py      an interpreter supporting Python evaluation
builder/
  main.c            a build tool which preprocesses, compiles and links lisp programs
module/             lisp modules
tools/
  makeheaders.c     copied from the Fossil project; used to generate header files
```

**Open problems and TODOs:**
- Optional static typing

- Make less awkward macro syntax
- Unite preprocessor with quasiquotes
- Sanitized macros; self-describing identifiers?; implicit imports?

- Switch from linked lists to slices
- Sane approach to memory allocation/deallocation
- Enclosing frames should be error-resistant and zero-cost
- Implement closure by value?

- Loops
- Tail recursive calls
- The yield structure should be flexible; i.e. possibility to call a routine and intercept its host calls

- Try hosting by a compiled language
- Try hosting by a constrained environment like bare metal or shader
