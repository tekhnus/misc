/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include "zlisp/common.h"
datum *get_host_ffi_settings();
char *prog_link_deps(vec *sl,datum *builder_compdata,datum *input_meta,fdatum(*module_bytecode)(char *,datum *,extension *),datum *settings,extension *ext,context *ctxt);
datum *extract_meta(vec sl,size_t run_main_off);
char *prog_build(vec *sl,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension *ext);
fdatum compile_module(char *module,datum *settings,extension *extension);
void module_to_filename(char *fname,char *module);
fdatum file_source(char *fname);
#define EXPORT
#define INTERFACE 0
