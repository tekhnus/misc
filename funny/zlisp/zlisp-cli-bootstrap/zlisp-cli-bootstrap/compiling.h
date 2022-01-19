/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp-impl/zlisp-impl.h>
#include <string.h>
#include <dlfcn.h>
#include <stdlib.h>
fdatum routine_run_and_get_value_c_host(state **ctxt,prog *p);
#include <unistd.h>
#define LOCAL static
LOCAL fdatum datum_expand(datum *e,state **ctxt);
fdatum file_source(char *fname);
fdatum module_source(char *module);
#include <stdint.h>
state *state_make_builtins();
LOCAL prog *module_prog(char *module);
char *prog_init_submodule_c_host(prog *p,datum *source);
routine module_routine(char *module);
char *prog_init_module_c_host(prog *p,datum *source);
#define INTERFACE 0
