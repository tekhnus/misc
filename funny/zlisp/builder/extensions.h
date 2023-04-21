/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#define LOCAL static
LOCAL char *trivial_extension_call(struct extension *self,vec *sl,size_t *begin,datum *stmt,datum *compdata);
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
LOCAL char *prog_append_backquoted_statement(vec *sl,size_t *begin,datum *stmt,datum *compdata,extension *ext);
extension *lisp_extension_alloc_make();
#include <zlisp/host-ffi.h>
char *prog_build(vec *sl,size_t *p,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension *ext);
LOCAL extension trivial_extension_make();
size_t prog_build_init(vec *sl,size_t *ep,size_t *bdr_p,datum *compdata,datum *builder_compdata);
LOCAL char *lisp_extension_call(struct extension *self_,vec *sl,size_t *begin,datum *stmt,datum *compdata);
struct lisp_extension lisp_extension_make();
#define EXPORT
#define EXPORT_INTERFACE 0
