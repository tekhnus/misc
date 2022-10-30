/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
#define LOCAL static
LOCAL fdatum datum_expand(datum *e,prog_slice *sl,datum **routine,size_t *p,datum **compdata,size_t *bp,datum **builder_compdata);
LOCAL char *module_to_filename(char *module);
LOCAL fdatum compile_module(char *module,datum *settings);
char *prog_link_deps(prog_slice *sl,size_t *bdr_p,datum **builder_compdata,size_t p,fdatum(*module_bytecode)(char *,datum *),datum *settings);
char *prog_slice_relocate(prog_slice *dst,size_t *p,datum *src);
#define EXPORT
char *prog_build(prog_slice *sl,size_t *p,size_t *bp,datum *source,datum **compdata,datum **builder_compdata,datum *settings);
size_t prog_build_init(prog_slice *sl,size_t *ep,size_t *bdr_p,datum **compdata,datum **builder_compdata);
LOCAL fdatum file_source(char *fname);
#define INTERFACE 0
