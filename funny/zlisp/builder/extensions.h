/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
char *prog_build(vec *sl,size_t *p,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension *trivial_extension);
#define LOCAL static
LOCAL extension extension_make_trivial();
size_t prog_build_init(vec *sl,size_t *ep,size_t *bdr_p,datum *compdata,datum *builder_compdata);
typedef struct lisp_extension_state lisp_extension_state;
LOCAL fdatum lisp_extension_run(datum *e,struct lisp_extension_state *est);
LOCAL char *prog_append_backquoted_statement(vec *sl,size_t *begin,datum *stmt,datum *compdata,extension *ext);
LOCAL char *call_ext_trivial(vec *sl,size_t *begin,datum *stmt,datum *compdata,struct extension *ext);
extension *extension_alloc_make();
LOCAL char *call_ext(vec *sl,size_t *begin,datum *stmt,datum *compdata,struct extension *ext);
LOCAL struct lisp_extension_state lisp_extension_state_make();
extension extension_make();
#define EXPORT
#define INTERFACE 0
