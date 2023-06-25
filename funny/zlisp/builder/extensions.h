/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include "zlisp/common.h"
size_t prog_build(vec *sl,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension *ext,context *ctxt);
datum file_source(char *fname,context *ctxt);
void module_to_filename(char *fname,char *module,context *ctxt);
size_t prog_build_init(vec *sl,datum *compdata,datum *builder_compdata,context *ctxt);
#define LOCAL static
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
LOCAL void standard_extension_init(vec *program,datum *routine_,datum *compdata,context *ctxt);
struct lisp_extension standard_extension_make(context *ctxt);
#define EXPORT
#define EXPORT_INTERFACE 0
