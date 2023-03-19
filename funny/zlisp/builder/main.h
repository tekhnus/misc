/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
#define LOCAL static
LOCAL void module_to_filename(char *fname,char *module);
LOCAL fdatum compile_module(char *module,datum *settings);
char *prog_link_deps(vec *sl,size_t *bdr_p,datum *builder_compdata,size_t p,fdatum(*module_bytecode)(char *,datum *),datum *settings);
char *vec_relocate(vec *dst,size_t *p,datum *src);
LOCAL char *prog_append_backquoted_statement(vec *sl,size_t *begin,datum *stmt,datum *compdata,extension_fn *ext);
char *call_ext(vec *sl,size_t *begin,datum *stmt,datum *compdata,struct extension_fn *ext);
datum *get_host_ffi_settings();
#define EXPORT
char *prog_build(vec *sl,size_t *p,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings);
size_t prog_build_init(vec *sl,size_t *ep,size_t *bdr_p,datum *compdata,datum *builder_compdata);
fdatum file_source(char *fname);
#define INTERFACE 0
