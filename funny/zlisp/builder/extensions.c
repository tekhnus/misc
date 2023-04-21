#include <assert.h>
#include <extensions.h>
#include <zlisp/common.h>

struct lisp_extension_state {
  vec lisp_extension_sl;
  size_t lisp_extension_prg;
  datum lisp_extension_routine;
  datum lisp_extension_compdata;
  extension lisp_extension_ext;
};

#if INTERFACE
typedef struct lisp_extension_state lisp_extension_state;
#endif

EXPORT extension extension_make() {
  struct lisp_extension_state *exps = malloc(sizeof(lisp_extension_state));
  *exps = lisp_extension_state_make();
  return (extension){call_ext, exps};
}

EXPORT extension *extension_alloc_make() {
  // For Lisp.
  extension *res = malloc(sizeof(extension));
  *res = extension_make();
  return res;
}

LOCAL char *call_ext_trivial(vec *sl, size_t *begin, datum *stmt,
                             datum *compdata, struct extension *ext) {
  datum *op = list_at(stmt, 0);
  if (datum_is_the_symbol(op, "backquote")) {
    if (list_length(stmt) != 2) {
      return "backquote should have a single arg";
    }
    return prog_append_backquoted_statement(sl, begin, list_at(stmt, 1),
                                            compdata, ext);
  }
  return "<not an extension>";
}

LOCAL char *call_ext(vec *sl, size_t *begin, datum *stmt, datum *compdata,
                     struct extension *ext) {
  datum *op = list_at(stmt, 0);
  if (datum_is_the_symbol(op, "backquote")) {
    if (list_length(stmt) != 2) {
      return "backquote should have a single arg";
    }
    return prog_append_backquoted_statement(sl, begin, list_at(stmt, 1),
                                            compdata, ext);
  }
  if (datum_is_the_symbol(op, "switch") || datum_is_the_symbol(op, "fntest")) {
    datum invokation_statement = datum_copy(stmt);
    *list_at(&invokation_statement, 0) = datum_make_list_of(
        datum_make_symbol("hash"),
        datum_make_list_of(datum_make_symbol("polysym"), datum_make_symbol(""),
                           datum_make_symbol("stdmacro"), datum_copy(op)));
    fdatum res = lisp_extension_run(&invokation_statement, ext->state);
    if (fdatum_is_panic(res)) {
      return res.panic_message;
    }
    assert(datum_is_list(&res.ok_value));
    for (int i = 0; i < list_length(&res.ok_value); ++i) {
      char *err = prog_append_statement(sl, begin, list_at(&res.ok_value, i),
                                        compdata, ext);
      if (err) {
        return err;
      }
    }
    return NULL;
  }
  return "<not an extension>";
}

LOCAL char *prog_append_backquoted_statement(vec *sl, size_t *begin,
                                             datum *stmt, datum *compdata,
                                             extension *ext) {
  if (!datum_is_list(stmt)) {
    prog_append_put_const(sl, begin, stmt, compdata);
    return NULL;
  }
  for (int i = 0; i < list_length(stmt); ++i) {
    datum *elem = list_at(stmt, i);
    char *err;
    if (datum_is_list(elem) && list_length(elem) == 2 &&
        datum_is_the_symbol(list_at(elem, 0), "tilde")) {
      err = prog_append_statement(sl, begin, list_at(elem, 1), compdata, ext);
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

LOCAL struct lisp_extension_state lisp_extension_state_make() {
  struct lisp_extension_state e;
  e.lisp_extension_sl = vec_make(16 * 1024);
  e.lisp_extension_prg = vec_append_new(&e.lisp_extension_sl);
  size_t lisp_extension_builder_prg = vec_append_new(&e.lisp_extension_sl);
  e.lisp_extension_routine = routine_make(lisp_extension_builder_prg, NULL);
  e.lisp_extension_compdata = compdata_make();
  datum lisp_extension_builder_compdata = compdata_make();
  prog_build_init(&e.lisp_extension_sl, &e.lisp_extension_prg,
                  &lisp_extension_builder_prg, &e.lisp_extension_compdata,
                  &lisp_extension_builder_compdata);
  e.lisp_extension_ext = extension_make_trivial();
  datum initialization_statements = datum_make_list_of(
      datum_make_list_of(datum_make_symbol("req"),
                         datum_make_list_of(datum_make_symbol("stdmacro"),
                                            datum_make_bytestring("stdmacro")),
                         datum_make_list_of(datum_make_symbol("fntest"),
                                            datum_make_bytestring("stdmacro"),
                                            datum_make_symbol("fntest")),
                         datum_make_list_of(datum_make_symbol("switch"),
                                            datum_make_bytestring("stdmacro"),
                                            datum_make_symbol("switch"))));
  datum set = datum_make_bytestring("c-prelude");
  char *res = prog_build(
      &e.lisp_extension_sl, &e.lisp_extension_prg, &lisp_extension_builder_prg,
      &initialization_statements, &e.lisp_extension_compdata,
      &lisp_extension_builder_compdata, &set, &e.lisp_extension_ext);
  if (res) {
    fprintf(stderr, "while building extensions: %s\n", res);
    exit(EXIT_FAILURE);
  }
  result init_res = routine_run_with_handler(
      e.lisp_extension_sl, &e.lisp_extension_routine, host_ffi);
  if (!datum_is_the_symbol(&init_res.type, "halt")) {
    fprintf(stderr, "while initializing extensions: %s\n",
            datum_repr(&init_res.value));
    exit(EXIT_FAILURE);
  }
  return e;
}

LOCAL fdatum lisp_extension_run(datum *e, struct lisp_extension_state *est) {
  datum mod = datum_make_list_of(datum_copy(e));
  datum set = datum_make_bytestring("c-prelude");
  char *err = prog_build(&est->lisp_extension_sl, &est->lisp_extension_prg,
                         NULL, &mod, &est->lisp_extension_compdata, NULL, &set,
                         &est->lisp_extension_ext);
  if (err != NULL) {
    char err2[256];
    err2[0] = 0;
    strcat(err2, "error while invoking an extension: ");
    strcat(err2, err);
    return fdatum_make_panic(err2);
  }
  result res = routine_run_with_handler(est->lisp_extension_sl,
                                        &est->lisp_extension_routine, host_ffi);
  if (!datum_is_the_symbol(&res.type, "halt")) {
    return fdatum_make_panic(datum_repr(&res.value));
  }
  return fdatum_make_ok(res.value);
}

LOCAL extension extension_make_trivial() {
  return (extension){call_ext_trivial, NULL};
}
