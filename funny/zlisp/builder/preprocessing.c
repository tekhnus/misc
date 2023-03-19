#include <preprocessing.h>

#if EXPORT_INTERFACE
struct expander_state {
  vec expander_sl;
  size_t expander_prg;
  size_t expander_builder_prg;
  datum expander_routine;
  datum expander_compdata;
  datum expander_builder_compdata;
};
#endif

EXPORT struct expander_state expander_state_make() {
  struct expander_state e;
  e.expander_sl = vec_make(16 * 1024);
  e.expander_prg = vec_append_new(&e.expander_sl);
  e.expander_builder_prg = vec_append_new(&e.expander_sl);
  e.expander_routine = routine_make(e.expander_builder_prg, NULL);
  e.expander_compdata = compdata_make();
  e.expander_builder_compdata = compdata_make();
  prog_build_init(&e.expander_sl, &e.expander_prg, &e.expander_builder_prg,
                  &e.expander_compdata, &e.expander_builder_compdata);
  return e;
}

EXPORT fdatum file_source(char *fname) {
  FILE *stre = fopen(fname, "r");
  if (stre == NULL) {
    char err[1024];
    err[0] = 0;
    strcat(err, "Module not found: ");
    strcat(err, fname);
    return fdatum_make_panic(err);
  }

  struct expander_state e = expander_state_make();
  read_result rr;
  datum res = datum_make_nil();
  for (; read_result_is_ok(rr = datum_read(stre));) {
    fdatum val = datum_expand(
                              &rr.ok_value, &e);
    if (fdatum_is_panic(val)) {
      char err[1024];
      char *end = err;
      end += sprintf(end, "while expanding %s: %s", datum_repr(&rr.ok_value),
                     val.panic_message);
      return fdatum_make_panic(err);
    }
    // fprintf(stderr, "exp\n  from %s\n  to   %s\n", datum_repr(rr.ok_value),
    // datum_repr(val.ok_value));
    if (datum_is_the_symbol(&val.ok_value, ":void-value")) {
      continue;
    }
    list_append(&res, val.ok_value);
  }
  if (read_result_is_panic(rr)) {
    return fdatum_make_panic(rr.panic_message);
  }
  if (read_result_is_right_paren(rr)) {
    return fdatum_make_panic("unmatched right paren");
  }
  return fdatum_make_ok(res);
}

EXPORT fdatum datum_expand(datum *e, struct expander_state *est) {
  if (!datum_is_list(e) || datum_is_nil(e)) {
    return fdatum_make_ok(*e);
  }
  if (!datum_is_the_symbol(list_at(e, 0), "bang")) {
    datum res = datum_make_nil();

    for (int i = 0; i < list_length(e); ++i) {
      datum *x = list_at(e, i);
      fdatum nxt =
          datum_expand(x, est);
      if (fdatum_is_panic(nxt)) {
        return nxt;
      }
      list_append(&res, nxt.ok_value);
    }
    return fdatum_make_ok(res);
  }
  if (list_length(e) != 2) {
    return fdatum_make_panic("! should be used with a single arg");
  }
  fdatum exp = datum_expand(list_at(e, 1), est);
  if (fdatum_is_panic(exp)) {
    return exp;
  }
  datum mod = datum_make_list_of(exp.ok_value);
  datum set = datum_make_bytestring("c-prelude");
  char *err = prog_build(&est->expander_sl, &est->expander_prg, &est->expander_builder_prg, &mod, &est->expander_compdata,
                         &est->expander_builder_compdata, &set);
  if (err != NULL) {
    char err2[256];
    err2[0] = 0;
    strcat(err2, "error while compiling a macro: ");
    strcat(err2, err);
    return fdatum_make_panic(err2);
  }
  fdatum res = routine_run_in_ffi_host(est->expander_sl, &est->expander_routine);
  if (fdatum_is_panic(res)) {
    return res;
  }
  if (list_length(&res.ok_value) == 0) {
    return fdatum_make_ok(datum_make_symbol(":void-value"));
  }
  if (list_length(&res.ok_value) != 1) {
    return fdatum_make_panic("expected a single result while preprocessing");
  }
  return fdatum_make_ok(*list_at(&res.ok_value, 0));
}
