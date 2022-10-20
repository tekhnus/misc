/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
fdatum routine_run_and_get_value_c_host_new(prog_slice sl,datum **r0d);
#define LOCAL static
#include <unistd.h>
LOCAL fdatum datum_expand(datum *e,prog_slice *sl,datum **routine,size_t *p,datum **compdata,size_t *bp,datum **builder_compdata);
fdatum file_source(char *fname);
char *module_to_filename(char *module);
fdatum module_source(char *module);
fdatum module_routine(char *module);
char *prog_build_c_host(prog_slice *sl,size_t *p,size_t *bp,datum *source,datum **compdata,datum **builder_compdata);
#define INTERFACE 0
