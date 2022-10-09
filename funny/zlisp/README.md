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
1) The bytecode should contain debug info.
2) As a workaround, functions are represented by self-referential datums in order to support recursion:(
3) Hosting by a compiled language should be tried.
4) Hosting by a constrained environment like bare metal or shader should be tried.
5) The base data structure is a linked list, which has little use. Switch to slices?
6) Compile-time computation syntax feels awkward to use:( Also it might be merged with quasiquotes?
7) No loops:(
8) Optional static typing.
