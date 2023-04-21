/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
char *prog_build(vec *sl,size_t *p,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension *extension);
#define LOCAL static
LOCAL extension extension_make_trivial();
size_t prog_build_init(vec *sl,size_t *ep,size_t *bdr_p,datum *compdata,datum *builder_compdata);
typedef struct lisp_extension lisp_extension;
struct lisp_extension {
  struct extension base;
  vec lisp_extension_sl;
  size_t lisp_extension_prg;
  datum lisp_extension_routine;
  datum lisp_extension_compdata;
  extension lisp_extension_ext;
};
LOCAL fdatum lisp_extension_run(datum *e,struct lisp_extension *est);
LOCAL char *call_ext(struct extension *self_,vec *sl,size_t *begin,datum *stmt,datum *compdata);
LOCAL char *prog_append_backquoted_statement(vec *sl,size_t *begin,datum *stmt,datum *compdata,extension *ext);
LOCAL char *call_ext_trivial(struct extension *self,vec *sl,size_t *begin,datum *stmt,datum *compdata);
struct lisp_extension lisp_extension_make();
extension *extension_alloc_make();
#define EXPORT
#define EXPORT_INTERFACE 0
