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
2) Linking is coupled with compilation; diamond dependencies are re-runned:(
3) As a workaround, functions are represented by self-referential datums in order to support recursion:(
4) Hosting by a compiled language should be tried.
5) The bytecode manages variables by their names, but it could enumerate them; maybe vars and stack can be united.
6) The base data structure is a linked list, which has little use. Switch to slices?
7) Compile-time computation syntax feels awkward to use:( Also it might be merged with quasiquotes?
8) No loops:(
