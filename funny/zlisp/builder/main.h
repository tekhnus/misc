/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
fdatum module_source(char *module);
#define LOCAL static
LOCAL fdatum datum_expand(datum *e,prog_slice *sl,datum **routine,size_t *p,datum **compdata,size_t *bp,datum **builder_compdata);
fdatum module_routine(char *module,datum *settings);
char *prog_build_c_host(prog_slice *sl,size_t *p,size_t *bp,datum *source,datum **compdata,datum **builder_compdata);
char *module_to_filename(char *module);
LOCAL fdatum preprocessed_module_source(char *module,char *prelude_module_name);
LOCAL fdatum compile_module(char *module,datum *settings);
char *relocate_and_build(prog_slice *sl,size_t *ep,size_t *bdr_p,datum *bytecode,fdatum(*module_bytecode)(char *,datum *),datum **builder_compdata,datum *settings);
fdatum file_source(char *fname);
#define INTERFACE 0
