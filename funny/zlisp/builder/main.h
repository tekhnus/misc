/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
#define LOCAL static
LOCAL char *module_to_filename(char *module);
LOCAL fdatum compile_module(char *module,datum *settings);
char *prog_link_deps(prog_slice *sl,size_t *bdr_p,datum **builder_compdata,size_t p,fdatum(*module_bytecode)(char *,datum *),datum *settings);
char *prog_slice_relocate(prog_slice *dst,size_t *p,datum *src);
datum *get_host_ffi_settings();
#define EXPORT
char *prog_build(prog_slice *sl,size_t *p,size_t *bp,datum *source,datum **compdata,datum **builder_compdata,datum *settings);
size_t prog_build_init(prog_slice *sl,size_t *ep,size_t *bdr_p,datum **compdata,datum **builder_compdata);
fdatum file_source(char *fname);
#define INTERFACE 0
