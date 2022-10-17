/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#define LOCAL static
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <zlisp/common.h>
LOCAL fdatum datum_expand(datum *e,prog_slice *sl,datum **routine,datum **compdata,size_t *bp,datum **builder_compdata);
#include <dlfcn.h>
#include <ffi.h>
fdatum routine_run_and_get_value_c_host_new(prog_slice sl,datum **r0d);
fdatum file_source(char *fname);
char *module_to_filename(char *module);
fdatum module_source(char *module);
char *prog_build_one_c_host_2(prog_slice *sl,size_t *p,size_t *bp,datum *source,datum **compdata,datum **builder_compdata);
char *prog_build_one_c_host(prog_slice *sl,size_t p,datum *source,datum **compdata);
char *module_routine(prog_slice *sl,size_t *p,char *module);
char *prog_build_c_host(prog_slice *sl,size_t *p,size_t *bp,datum *source,datum **compdata,datum **builder_compdata);
#define INTERFACE 0
