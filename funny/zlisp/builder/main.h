/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
void module_to_filename(char *fname,char *module);
#define LOCAL static
LOCAL fdatum compile_module(char *module,datum *settings,extension *extension);
char *prog_link_deps(vec *sl,size_t *bdr_p,datum *builder_compdata,size_t p,fdatum(*module_bytecode)(char *,datum *,extension *),datum *settings,extension *ext);
datum *get_host_ffi_settings();
#define EXPORT
char *prog_build(vec *sl,size_t *p,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension *ext);
struct lisp_extension standard_extension_make();
size_t prog_build_init(vec *sl,datum *compdata,datum *builder_compdata);
fdatum file_source(char *fname);
#define INTERFACE 0
