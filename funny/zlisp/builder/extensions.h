/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#define LOCAL static
LOCAL char *call_ext_for_macros(vec *sl,size_t *begin,datum *stmt,datum *compdata,struct extension_fn *ext);
#include <zlisp/host-ffi.h>
char *prog_build(vec *sl,size_t *p,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension_fn *trivial_extension);
LOCAL extension_fn extension_for_macros_make();
size_t prog_build_init(vec *sl,size_t *ep,size_t *bdr_p,datum *compdata,datum *builder_compdata);
typedef struct expander_state expander_state;
LOCAL fdatum datum_expand(datum *e,struct expander_state *est);
LOCAL char *prog_append_backquoted_statement(vec *sl,size_t *begin,datum *stmt,datum *compdata,extension_fn *ext);
extension_fn *extension_alloc_make();
LOCAL char *call_ext(vec *sl,size_t *begin,datum *stmt,datum *compdata,struct extension_fn *ext);
struct expander_state {
  vec expander_sl;
  size_t expander_prg;
  datum expander_routine;
  datum expander_compdata;
  extension_fn expander_ext;
};
LOCAL struct expander_state expander_state_make();
extension_fn extension_make();
#define EXPORT
#define EXPORT_INTERFACE 0
