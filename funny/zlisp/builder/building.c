#include <stdlib.h>
#if INTERFACE
#include "zlisp/common.h"
#endif
#include "building.h"
#include "zlisp/common.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

EXPORT datum file_source(char *fname, context *ctxt) {
  FILE *stre = fopen(fname, "r");
  if (stre == NULL) {
    perror("file_source");
    abortf(ctxt, "Module not found: %s", fname);
    return (datum){};
  }

  read_result rr = datum_read_all(stre);
  if (read_result_is_panic(rr)) {
    abortf(ctxt, "%s", rr.panic_message);
    return (datum){};
  }
  assert(read_result_is_ok(rr));
  return (rr.ok_value);
}

EXPORT void module_to_filename(char *fname, char *module, context *ctxt) {
  char *zlisp_home = getenv("ZLISP");
  if (zlisp_home == NULL) {
    abortf(ctxt, "ZLISP variable not defined");
    return;
  }
  strcat(fname, zlisp_home);
  strcat(fname, "/");
  strcat(fname, module);
  strcat(fname, "/main.lisp");
}

EXPORT datum compile_module(char *module, datum *settings,
                             extension *extension, context *ctxt) {
  if (!datum_is_bytestring(settings)) {
    abortf(ctxt, "settings should be a string");
    return (datum_make_nil());
  }
  if (!strcmp(module, "prelude")) {
    module = settings->bytestring_value;
  }
  char fname[1024] = {'\0'};
  module_to_filename(fname, module, ctxt);
  if (ctxt->aborted) {
    return (datum_make_nil());
  }
  datum src = file_source(fname, ctxt);
  if (ctxt->aborted) {
    return (datum_make_nil());
  }
  datum compdata = compdata_make();
  vec sl = vec_create_slice();
  prog_compile(&sl, &src, &compdata, extension, ctxt);
  if (ctxt->aborted) {
    return (datum_make_nil());
  }
  return (datum_make_list(sl));
}

EXPORT void prog_build(vec *sl, size_t *bp, datum *source, datum *compdata,
                        datum *builder_compdata, datum *settings,
                        extension *ext, context *ctxt) {
  size_t start_p = prog_get_next_index(sl);
  prog_compile(sl, source, compdata, ext, ctxt);
  if (ctxt->aborted) {
    return;
  }
  datum *input_meta = extract_meta(*sl, start_p);

  vec return_expr = vec_make_of(
      datum_make_nil(), datum_make_symbol(":="), datum_make_symbol("return"),
      datum_make_symbol("at"), datum_make_list_of(datum_make_symbol("halt")),
      datum_make_symbol("at"), datum_make_list_of(datum_make_int(0)),
      datum_make_symbol("flat"), datum_make_nil());
  datum ret_exp = datum_make_list(return_expr);
  prog_compile(sl, &ret_exp, builder_compdata, ext, ctxt);
  if (ctxt->aborted) {
    return;
  }
  size_t p_end = prog_get_next_index(sl);
  ptrdiff_t *p_end_ = prog_append_jmp(sl); // filled below.
  assert(bp != NULL);
  ptrdiff_t *builder_jmp = prog_get_jmp_delta(sl, *bp);
  *builder_jmp = prog_get_next_index(sl) - *bp;

  prog_link_deps(sl, builder_compdata, input_meta, compile_module,
                             settings, ext, ctxt);
  if (ctxt->aborted) {
    return;
  }
  *bp = prog_get_next_index(sl);
  // filled in next build
  prog_append_jmp(sl);
  size_t ind = prog_get_next_index(sl);
  *p_end_ = (ind - p_end);
  return;
}

EXPORT datum *get_host_ffi_settings() { // used in lisp
  datum *res = malloc(sizeof(datum));
  *res = datum_make_bytestring("c-prelude");
  return res;
}
