**zlisp**.

To build and test:
```
./scripts/build-and-test
```

After this you can run the repl:
```
ZLISP=./module ./build/host-ffi/zlisp-run module/cli/main.lisp
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
  main.c            takes a lisp file, writes bytecode to stdout
  asm_to_python     an interpreter supporting Python evaluation
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
- Single common stack?

- Loops
- Tail recursive calls

- Separate the builder and the linker

- Try hosting by a compiled language
- Try hosting by a constrained environment like bare metal or shader
