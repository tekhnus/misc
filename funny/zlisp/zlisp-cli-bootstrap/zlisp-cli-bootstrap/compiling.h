/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp-impl/zlisp-impl.h>
#include <string.h>
#include <dlfcn.h>
#include <stdlib.h>
fdatum perform_host_instruction(datum *name,datum *arg);
#include <unistd.h>
#define LOCAL static
LOCAL fdatum datum_expand(datum *e,state **ctxt);
fdatum file_source(char *fname);
fdatum module_source(char *module);
#include <stdint.h>
state *state_make_builtins();
LOCAL prog *module_prog(char *module);
routine module_routine(char *module);
char *prog_init_module_c_host(prog *p,datum *source);
#define INTERFACE 0
