/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
fdatum routine_run_and_get_value_c_host(prog_slice sl,state **ctxt,prog *p);
#define LOCAL static
#include <unistd.h>
LOCAL fdatum datum_expand(datum *e,prog_slice *sl,state **ctxt);
#include <stdint.h>
state *state_make_builtins();
fdatum file_source(char *fname);
char *module_to_filename(char *module);
fdatum module_source(char *module);
fdatum prog_init_submodule_c_host(prog_slice *sl,prog *p,datum *source);
char *prog_init_one_c_host(prog_slice *sl,prog *p,datum *source);
fdatum module_routine(prog_slice *sl,prog *p,char *module);
char *prog_init_module_c_host(prog_slice *sl,prog *p,datum *source);
#define INTERFACE 0
