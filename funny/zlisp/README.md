**zlisp**.

To build and test:
```
./scripts/build-and-test
```

After this you can run the repl:
```
ZLISP=./module ./build/host-ffi/zlisp-run <(./build/builder/zlisp-build c-prelude module/cli/main.lisp)
```

Project structure:
```
common/
  types.h           data types
  main.c            data manipulation, reading and writing
  compiling.c       compiling lisp to bytecode
  building.c        linking modules together
  running.c         a common interpreter
host-ffi/
  main.c            an interpreter supporting dlopen() and FFI function calls
host-python/
  zlisp-run-py      an interpreter supporting Python evaluation
builder/
  main.c            build tool which preprocesses, compiles and links lisp programs
module/             lisp modules
tools/
  makeheaders.c     copied from the Fossil project; used to generate header files
```

**Open problems and TODOs:**
- Optional static typing

- Make less awkward macro syntax
- Unite preprocessor with quasiquotes
- Sanitized macros

- Prohibit self-referential datums (currently used for representing recursive functions)
- Fix the size of module representation which is quadratic w.r.t. the function count
- Switch from linked lists to slices
- Sane approach to memory allocation/deallocation

- Loops
- Tail recursive calls

- Try hosting by a compiled language
- Try hosting by a constrained environment like bare metal or shader
