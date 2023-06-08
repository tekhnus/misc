#include <stdlib.h>
#if INTERFACE
#include "zlisp/common.h"
#endif
#include "building.h"
#include "zlisp/common.h"
#include <assert.h>
#include <stdio.h>
#include <string.h>

EXPORT fdatum file_source(char *fname) {
  FILE *stre = fopen(fname, "r");
  if (stre == NULL) {
    perror("file_source");
    char err[1024];
    err[0] = 0;
    strcat(err, "Module not found: ");
    strcat(err, fname);
    return fdatum_make_panic(err);
  }

  read_result rr = datum_read_all(stre);
  if (read_result_is_panic(rr)) {
    return fdatum_make_panic(rr.panic_message);
  }
  assert(read_result_is_ok(rr));
  return fdatum_make_ok(rr.ok_value);
}

EXPORT void module_to_filename(char *fname, char *module) {
  char *zlisp_home = getenv("ZLISP");
  if (zlisp_home == NULL) {
    fprintf(stderr, "ZLISP variable not defined");
    exit(EXIT_FAILURE);
  }
  strcat(fname, zlisp_home);
  strcat(fname, "/");
  strcat(fname, module);
  strcat(fname, "/main.lisp");
}

EXPORT fdatum compile_module(char *module, datum *settings,
                             extension *extension) {
  if (!datum_is_bytestring(settings)) {
    return fdatum_make_panic("settings should be a string");
  }
  if (!strcmp(module, "prelude")) {
    module = settings->bytestring_value;
  }
  char fname[1024] = {'\0'};
  module_to_filename(fname, module);
  fdatum src = file_source(fname);
  if (fdatum_is_panic(src)) {
    return src;
  }
  datum compdata = compdata_make();
  return prog_compile(&src.ok_value, &compdata, extension);
}

EXPORT char *prog_build(vec *sl, size_t *bp, datum *source, datum *compdata,
                        datum *builder_compdata, datum *settings,
                        extension *ext) {
  size_t start_p = prog_get_next_index(sl);
  char *res = prog_compile_and_relocate(sl, source, compdata, ext);
  if (res != NULL) {
    return res;
  }
  datum *input_meta = extract_meta(*sl, start_p);

  vec return_expr = vec_make_of(
      datum_make_nil(), datum_make_symbol(":="), datum_make_symbol("return"),
      datum_make_symbol("at"), datum_make_list_of(datum_make_symbol("halt")),
      datum_make_symbol("at"), datum_make_list_of(datum_make_int(0)),
      datum_make_symbol("flat"), datum_make_nil());
  datum ret_exp = datum_make_list(return_expr);
  char *res2 = prog_compile_and_relocate(sl, &ret_exp, builder_compdata, ext);
  if (res2 != NULL) {
    fprintf(stderr, "%s\n", res2);
    exit(EXIT_FAILURE);
  }
  size_t p_end = prog_get_next_index(sl);
  ptrdiff_t *p_end_ = prog_append_jmp(sl); // filled below.
  assert(bp != NULL);
  ptrdiff_t *builder_jmp = prog_get_jmp_delta(sl, *bp);
  *builder_jmp = prog_get_next_index(sl) - *bp;

  res = prog_link_deps(sl, builder_compdata, input_meta, compile_module,
                       settings, ext);
  if (res != NULL) {
    return res;
  }
  *bp = prog_get_next_index(sl);
  // filled in next build
  prog_append_jmp(sl);
  size_t ind = prog_get_next_index(sl);
  *p_end_ = (ind - p_end);
  return NULL;
}

EXPORT datum *get_host_ffi_settings() { // used in lisp
  datum *res = malloc(sizeof(datum));
  *res = datum_make_bytestring("c-prelude");
  return res;
}
