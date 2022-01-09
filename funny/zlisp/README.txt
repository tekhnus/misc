In April 2013, I started working on a small lisp implementation,
but I abandoned it almost immediately.

In June 2021, I started writing a new lisp from scratch.
The most basic stuff is implemented: functions, macros,
quasiquotes, etc. Dynamically loading shared libraries and
calling functions from them is also possible.

To build and run the REPL:
```
cmake -B build
cmake -B build -DCMAKE_EXPORT_COMPILE_COMMANDS=1
cmake --build build --target codegen
cmake --build build
cmake --build build --target test ARGS=-V
build/zlisp-cli-bootstrap/zlisp-run module/cli/main.lisp

# To use clang from homebrew (WIP):
cmake -B build -DCMAKE_C_COMPILER=/usr/local/opt/llvm/bin/clang -DCMAKE_C_FLAGS='-I/usr/local/opt/llvm/include -L/usr/local/opt/llvm/lib -Wl,-rpath,/usr/local/opt/llvm/lib' -DCMAKE_OSX_SYSROOT=/ -DCMAKE_OSX_DEPLOYMENT_TARGET=''
```

специальные формы:
- (shared-library name)
- (extern-pointer lib name)
- (call-pointer ptr-to-fn args)
типы:
- pointer

специальные формы:
- (builtin "dlopen")
- (builtin "dlsym")
- (builtin "dlerror")
- (call-pointer ptr-to-fn args)
типы:
- pointer

go:
специальные формы:
- (go-symbol "package" "symname")
- (go-call sym args)
типы:
- go-value

tis tiny lisp
lit little lisp
interlisp
