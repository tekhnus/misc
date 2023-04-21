/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
#define LOCAL static
LOCAL void module_to_filename(char *fname,char *module);
LOCAL fdatum compile_module(char *module,datum *settings,extension *extension);
char *prog_link_deps(vec *sl,size_t *bdr_p,datum *builder_compdata,size_t p,fdatum(*module_bytecode)(char *,datum *,extension *),datum *settings,extension *ext);
char *vec_relocate(vec *dst,size_t *p,datum *src);
datum *get_host_ffi_settings();
#define EXPORT
char *prog_build(vec *sl,size_t *p,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension *extension);
typedef struct lisp_extension lisp_extension;
struct lisp_extension {
  struct extension base;
  vec lisp_extension_sl;
  size_t lisp_extension_prg;
  datum lisp_extension_routine;
  datum lisp_extension_compdata;
  extension lisp_extension_ext;
};
struct lisp_extension lisp_extension_make();
size_t prog_build_init(vec *sl,size_t *ep,size_t *bdr_p,datum *compdata,datum *builder_compdata);
LOCAL fdatum file_source(char *fname);
#define INTERFACE 0
