/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#define LOCAL static
LOCAL char *null_extension_call(struct extension *self,vec *sl,size_t *begin,datum *stmt,datum *compdata);
LOCAL char *trivial_extension_call(struct extension *self,vec *sl,size_t *begin,datum *stmt,datum *compdata);
#include <zlisp/host-ffi.h>
char *prog_compile_and_relocate(vec *sl,size_t *p,datum *source,datum *compdata,extension *ext);
LOCAL extension null_extension_make();
typedef struct lisp_extension lisp_extension;
struct lisp_extension {
  struct extension base;
  vec program;
  size_t instruction;
  datum routine_;
  datum compdata;
  fdatum (*yield_handler)(datum *, datum *);
};
LOCAL fdatum lisp_extension_run(datum *e,struct lisp_extension *est);
LOCAL char *prog_append_backquoted_statement(vec *sl,size_t *begin,datum *stmt,datum *compdata,extension *ext);
extension *standard_extension_alloc_make();
char *prog_build(vec *sl,size_t *p,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension *ext);
LOCAL extension trivial_extension_make();
size_t prog_build_init(vec *sl,size_t *ep,size_t *bdr_p,datum *compdata,datum *builder_compdata);
LOCAL char *lisp_extension_call(struct extension *self_,vec *sl,size_t *begin,datum *stmt,datum *compdata);
struct lisp_extension lisp_extension_make(vec program,size_t instruction,datum routine_,datum compdata,fdatum(*yield_handler)(datum *,datum *));
LOCAL char *standard_extension_init(vec *program,size_t *instruction,datum *routine_,datum *compdata);
struct lisp_extension standard_extension_make();
#define EXPORT
#define EXPORT_INTERFACE 0
