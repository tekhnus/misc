/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include "zlisp/common.h"
char *prog_build(vec *sl,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension *ext);
fdatum file_source(char *fname);
void module_to_filename(char *fname,char *module);
size_t prog_build_init(vec *sl,datum *compdata,datum *builder_compdata,context *ctxt);
#define LOCAL static
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
extension *standard_extension_alloc_make();
LOCAL char *standard_extension_init(vec *program,datum *routine_,datum *compdata);
struct lisp_extension standard_extension_make();
#define EXPORT
#define EXPORT_INTERFACE 0
