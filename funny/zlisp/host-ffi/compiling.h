/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp-impl/zlisp-impl.h>
#include <string.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <ffi.h>
fdatum routine_run_and_get_value_c_host(state **ctxt,prog *p);
#define LOCAL static
#include <unistd.h>
LOCAL fdatum datum_expand(datum *e,state **ctxt);
#include <stdint.h>
state *state_make_builtins();
fdatum file_source(char *fname);
fdatum module_source(char *module);
char *prog_init_submodule_c_host(prog_slice *sl,prog *p,datum *source);
char *module_routine(prog_slice *sl,prog *p,char *module);
char *prog_init_module_c_host(prog_slice *sl,prog *p,datum *source);
#define INTERFACE 0
