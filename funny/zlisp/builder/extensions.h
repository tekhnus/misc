/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#define LOCAL static
LOCAL char *call_ext_trivial(vec *sl,size_t *begin,datum *stmt,datum *compdata,struct extension_fn *ext);
#include <zlisp/host-ffi.h>
char *prog_build(vec *sl,size_t *p,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension_fn *trivial_extension);
LOCAL extension_fn extension_make_trivial();
size_t prog_build_init(vec *sl,size_t *ep,size_t *bdr_p,datum *compdata,datum *builder_compdata);
typedef struct lisp_extension_state lisp_extension_state;
LOCAL fdatum lisp_extension_run(datum *e,struct lisp_extension_state *est);
LOCAL char *prog_append_backquoted_statement(vec *sl,size_t *begin,datum *stmt,datum *compdata,extension_fn *ext);
extension_fn *extension_alloc_make();
LOCAL char *call_ext(vec *sl,size_t *begin,datum *stmt,datum *compdata,struct extension_fn *ext);
struct lisp_extension_state {
  vec lisp_extension_sl;
  size_t lisp_extension_prg;
  datum lisp_extension_routine;
  datum lisp_extension_compdata;
  extension_fn lisp_extension_ext;
};
LOCAL struct lisp_extension_state lisp_extension_state_make();
extension_fn extension_make();
#define EXPORT
#define EXPORT_INTERFACE 0
