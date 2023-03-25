// a basic CLI for zlisp interpreter.
#if INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
#endif
#include <main.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <assert.h>

int main(int argc, char **argv) {
  if (argc != 3) {
    printf("usage: %s <prelude> <script>\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  char filename_copy[1024] = {0};

  if (argv[2][0] != '/') {
    char *res = getcwd(filename_copy, sizeof(filename_copy));
    if (res == NULL) {
      perror("while getting cwd");
      return EXIT_FAILURE;
    }
    strcat(filename_copy, "/");
  }
  strcat(filename_copy, argv[2]);
  fdatum src = file_source(filename_copy);
  if (fdatum_is_panic(src)) {
    fprintf(stderr, "file error: %s\n", src.panic_message);
    return EXIT_FAILURE;
  }
  vec sl = vec_make(16 * 1024);
  // the interpreter will start from the first instruction,
  // so the first call of append_new must be for the starting point.
  size_t bp = vec_append_new(&sl);
  size_t p = vec_append_new(&sl);
  datum compdata = compdata_make();
  datum builder_compdata = compdata_make();
  prog_build_init(&sl, &p, &bp, &compdata, &builder_compdata);
  datum set = datum_make_bytestring(argv[1]);
  struct expander_state exps = expander_state_make();
  struct extension_fn trivial_extension = {call_ext, &exps};
  char *err = prog_build(&sl, &p, &bp, &src.ok_value, &compdata,
                         &builder_compdata, &set, &trivial_extension);
  if (err != NULL) {
    fprintf(stderr, "compilation error: %s\n", err);
    return EXIT_FAILURE;
  }
  datum d = vec_to_datum(&sl);
  printf("%s\n", datum_repr_bounded(&d, 128));
  return EXIT_SUCCESS;
}

EXPORT extension_fn *extension_alloc_make() {
  // For Lisp.
  extension_fn *res = malloc(sizeof(extension_fn));
  *res = (extension_fn){call_ext, NULL};
  return res;
}

EXPORT datum *get_host_ffi_settings() { // used in lisp
  datum *res = malloc(sizeof(datum));
  *res = datum_make_bytestring("c-prelude");
  return res;
}

EXPORT char *call_ext_for_macros(vec *sl, size_t *begin,
              datum *stmt, datum *compdata, struct extension_fn *ext) {
  datum *op = list_at(stmt, 0);
  if (datum_is_the_symbol(op, "backquote")) {
    if (list_length(stmt) != 2) {
      return "backquote should have a single arg";
    }
    return prog_append_backquoted_statement(
                                            sl, begin, list_at(stmt, 1), compdata, ext);
  }
  return "<not an extension>";
}

EXPORT char *call_ext(vec *sl, size_t *begin,
              datum *stmt, datum *compdata, struct extension_fn *ext) {
  datum *op = list_at(stmt, 0);
  if (datum_is_the_symbol(op, "backquote")) {
    if (list_length(stmt) != 2) {
      return "backquote should have a single arg";
    }
    return prog_append_backquoted_statement(
                                            sl, begin, list_at(stmt, 1), compdata, ext);
  }
  if (datum_is_the_symbol(op, "bang")) {
    assert(ext->state);
    if (list_length(stmt) != 2) {
      return "bang should have a single arg";
    }
    fdatum res = datum_expand(list_at(stmt, 1), ext->state);
    if (fdatum_is_panic(res)) {
      return res.panic_message;
    }
    assert(datum_is_list(&res.ok_value));
    for (int i = 0; i < list_length(&res.ok_value); ++i) {
      char *err = prog_append_statement(sl, begin, list_at(&res.ok_value, i), compdata, ext);
      if (err) {
        return err;
      }
    }
    return NULL;
  }
  return "<not an extension>";
}

LOCAL char *prog_append_backquoted_statement(vec *sl, size_t *begin,
                                             datum *stmt, datum *compdata, extension_fn *ext) {
  if (!datum_is_list(stmt)) {
    prog_append_put_const(sl, begin, stmt, compdata);
    return NULL;
  }
  for (int i = 0; i < list_length(stmt); ++i) {
    datum *elem = list_at(stmt, i);
    char *err;
    if (datum_is_list(elem) && list_length(elem) == 2 &&
        datum_is_the_symbol(list_at(elem, 0), "tilde")) {
      err = prog_append_statement(sl, begin, list_at(elem, 1),
                                  compdata, ext);
    } else {
      err = prog_append_backquoted_statement(sl, begin, elem, compdata, ext);
    }
    if (err != NULL) {
      return err;
    }
  }
  prog_append_collect(sl, list_length(stmt), begin, compdata);
  return NULL;
}

EXPORT char *prog_build(vec *sl, size_t *p, size_t *bp, datum *source,
                        datum *compdata, datum *builder_compdata,
                        datum *settings, extension_fn *trivial_extension) {
  fdatum bytecode = prog_compile(source, compdata, trivial_extension);
  if (fdatum_is_panic(bytecode)) {
    return bytecode.panic_message;
  }
  // this is a hack in order to make the relocation possible.
  prog_append_nop(sl, p);
  size_t start_p = *p;
  char *res = vec_relocate(sl, p, &bytecode.ok_value);
  if (res != NULL) {
    return res;
  }
  int yield_count = compdata_has_value(compdata) ? 1 : 0;
  datum nil = datum_make_nil();
  prog_append_yield(sl, p, datum_make_symbol("halt"), yield_count, 0,
                    nil, compdata);
  if (!bp) {
    return NULL;
  }
  return prog_link_deps(sl, bp, builder_compdata, start_p, compile_module,
                        settings, trivial_extension);
}

LOCAL fdatum compile_module(char *module, datum *settings, extension_fn *trivial_extension) {
  if (!datum_is_bytestring(settings)) {
    return fdatum_make_panic("settings should be a string");
  }
  if (!strcmp(module, "prelude")) {
    if (!strcmp(settings->bytestring_value, "c-prelude-no-macros")) {
      module = "c-prelude";
    } else {
      module = settings->bytestring_value;
    }
  }
  char fname[1024] = {'\0'};
  module_to_filename(fname, module);
  fdatum src = file_source(fname);
  if (fdatum_is_panic(src)) {
    return src;
  }
  datum compdata = compdata_make();
  return prog_compile(&src.ok_value, &compdata, trivial_extension);
}

LOCAL void module_to_filename(char *fname, char *module) {
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
