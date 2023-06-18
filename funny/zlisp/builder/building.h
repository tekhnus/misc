/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include "zlisp/common.h"
datum *get_host_ffi_settings();
void prog_link_deps(vec *sl,datum *builder_compdata,datum *input_meta,datum(*module_bytecode)(char *,datum *,extension *,context *ctxt),datum *settings,extension *ext,context *ctxt);
datum *extract_meta(vec sl,size_t run_main_off);
void prog_build(vec *sl,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension *ext,context *ctxt);
datum compile_module(char *module,datum *settings,extension *extension,context *ctxt);
void module_to_filename(char *fname,char *module);
fdatum file_source(char *fname);
#define EXPORT
#define INTERFACE 0
