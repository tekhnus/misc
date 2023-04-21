#include <assert.h>
#include <extensions.h>

#if EXPORT_INTERFACE
#include <zlisp/common.h>
struct expander_state {
  vec expander_sl;
  size_t expander_prg;
  datum expander_routine;
  datum expander_compdata;
  extension_fn expander_ext;
};
#endif

EXPORT extension_fn extension_make() {
  struct expander_state *exps = malloc(sizeof(expander_state));
  *exps = expander_state_make();
  return (extension_fn){call_ext, exps};
}

EXPORT extension_fn *extension_alloc_make() {
  // For Lisp.
  extension_fn *res = malloc(sizeof(extension_fn));
  *res = extension_make();
  return res;
}

LOCAL char *call_ext(vec *sl, size_t *begin, datum *stmt, datum *compdata,
                      struct extension_fn *ext) {
  datum *op = list_at(stmt, 0);
  if (datum_is_the_symbol(op, "backquote")) {
    if (list_length(stmt) != 2) {
      return "backquote should have a single arg";
    }
    return prog_append_backquoted_statement(sl, begin, list_at(stmt, 1),
                                            compdata, ext);
  }
  if (datum_is_the_symbol(op, "switch") || datum_is_the_symbol(op, "fntest")) {
    datum macrostmt = datum_copy(stmt);
    *list_at(&macrostmt, 0) = datum_make_list_of(
        datum_make_symbol("hash"),
        datum_make_list_of(datum_make_symbol("polysym"), datum_make_symbol(""),
                           datum_make_symbol("stdmacro"), datum_copy(op)));
    fdatum res = datum_expand(&macrostmt, ext->state);
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
                                             extension_fn *ext) {
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

LOCAL struct expander_state expander_state_make() {
  struct expander_state e;
  e.expander_sl = vec_make(16 * 1024);
  e.expander_prg = vec_append_new(&e.expander_sl);
  size_t expander_builder_prg = vec_append_new(&e.expander_sl);
  e.expander_routine = routine_make(expander_builder_prg, NULL);
  e.expander_compdata = compdata_make();
  datum expander_builder_compdata = compdata_make();
  prog_build_init(&e.expander_sl, &e.expander_prg, &expander_builder_prg,
                  &e.expander_compdata, &expander_builder_compdata);
  e.expander_ext = extension_for_macros_make();
  datum macro_init = datum_make_list_of(
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
  char *res = prog_build(&e.expander_sl, &e.expander_prg, &expander_builder_prg,
                         &macro_init, &e.expander_compdata,
                         &expander_builder_compdata, &set, &e.expander_ext);
  if (res) {
    fprintf(stderr, "while building macros: %s\n", res);
    exit(EXIT_FAILURE);
  }
  result init_res =
      routine_run_with_handler(e.expander_sl, &e.expander_routine, host_ffi);
  if (!datum_is_the_symbol(&init_res.type, "halt")) {
    fprintf(stderr, "while initializing macros: %s\n",
            datum_repr(&init_res.value));
    exit(EXIT_FAILURE);
  }
  return e;
}

LOCAL fdatum datum_expand(datum *e, struct expander_state *est) {
  datum mod = datum_make_list_of(datum_copy(e));
  datum set = datum_make_bytestring("c-prelude");
  char *err =
      prog_build(&est->expander_sl, &est->expander_prg, NULL, &mod,
                 &est->expander_compdata, NULL, &set, &est->expander_ext);
  if (err != NULL) {
    char err2[256];
    err2[0] = 0;
    strcat(err2, "error while compiling a macro: ");
    strcat(err2, err);
    return fdatum_make_panic(err2);
  }
  result res = routine_run_with_handler(est->expander_sl,
                                        &est->expander_routine, host_ffi);
  if (!datum_is_the_symbol(&res.type, "halt")) {
    return fdatum_make_panic(datum_repr(&res.value));
  }
  return fdatum_make_ok(res.value);
}

LOCAL extension_fn extension_for_macros_make() {
  return (extension_fn){call_ext_for_macros, NULL};
}

LOCAL char *call_ext_for_macros(vec *sl, size_t *begin, datum *stmt,
                                 datum *compdata, struct extension_fn *ext) {
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
