/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
fdatum routine_run_and_get_value_c_host(prog_slice sl,state **ctxt,size_t prg);
#include <stdint.h>
state *state_make_builtins();
#include <unistd.h>
char *prog_build_c_host(prog_slice *sl,prog *p,datum *source);
fdatum file_source(char *fname);
