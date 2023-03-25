/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
char *prog_build(vec *sl,size_t *p,size_t *bp,datum *source,datum *compdata,datum *builder_compdata,datum *settings,extension_fn *trivial_extension);
typedef struct expander_state expander_state;
fdatum datum_expand(datum *e,struct expander_state *est,char *tag);
fdatum file_source(char *fname);
char *call_ext(vec *sl,size_t *begin,datum *stmt,datum *compdata,struct extension_fn *ext);
size_t prog_build_init(vec *sl,size_t *ep,size_t *bdr_p,datum *compdata,datum *builder_compdata);
struct expander_state {
  vec expander_sl;
  size_t expander_prg;
  size_t expander_builder_prg;
  datum expander_routine;
  datum expander_compdata;
  datum expander_builder_compdata;
  extension_fn expander_ext;
};
struct expander_state expander_state_make();
#define EXPORT
#define EXPORT_INTERFACE 0
