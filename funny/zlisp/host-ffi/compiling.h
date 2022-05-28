/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
fdatum routine_run_and_get_value_c_host(prog_slice sl,state **ctxt,size_t prg);
#define LOCAL static
#include <unistd.h>
LOCAL fdatum datum_expand(datum *e,prog_slice *sl,state **ctxt);
#include <stdint.h>
state *state_make_builtins();
fdatum file_source(char *fname);
char *module_to_filename(char *module);
fdatum module_source(char *module);
char *prog_build_one_c_host(prog_slice *sl,size_t p,datum *source);
fdatum module_routine(prog_slice *sl,size_t *p,char *module);
char *prog_build_c_host(prog_slice *sl,size_t p,datum *source);
#define INTERFACE 0
