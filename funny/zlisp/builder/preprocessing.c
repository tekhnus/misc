#include <preprocessing.h>

EXPORT fdatum file_source(char *fname) {
  FILE *stre = fopen(fname, "r");
  if (stre == NULL) {
    char err[1024];
    err[0] = 0;
    strcat(err, "Module not found: ");
    strcat(err, fname);
    return fdatum_make_panic(err);
  }

  vec expander_sl = vec_make(16 * 1024);
  size_t expander_prg = vec_append_new(&expander_sl);
  size_t expander_builder_prg = vec_append_new(&expander_sl);
  datum expander_routine = routine_make(expander_builder_prg, NULL);
  datum expander_compdata = *compdata_make();
  datum *expander_compdata_ptr = &expander_compdata;
  datum expander_builder_compdata = *compdata_make();
  datum *expander_builder_compdata_ptr = &expander_builder_compdata;
  prog_build_init(&expander_sl, &expander_prg, &expander_builder_prg,
                  &expander_compdata_ptr, &expander_builder_compdata_ptr);
  read_result rr;
  datum res = *datum_make_nil();
  for (; read_result_is_ok(rr = datum_read(stre));) {
    fdatum val = datum_expand(
        &rr.ok_value, &expander_sl, &expander_routine, &expander_prg,
        &expander_compdata_ptr, &expander_builder_prg, &expander_builder_compdata_ptr);
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
    list_append(&res, &val.ok_value);
  }
  if (read_result_is_panic(rr)) {
    return fdatum_make_panic(rr.panic_message);
  }
  if (read_result_is_right_paren(rr)) {
    return fdatum_make_panic("unmatched right paren");
  }
  return fdatum_make_ok(res);
}

LOCAL fdatum datum_expand(datum *e, vec *sl, datum *routine, size_t *p,
                          datum **compdata, size_t *bp,
                          datum **builder_compdata) {
  if (!datum_is_list(e) || datum_is_nil(e)) {
    return fdatum_make_ok(*e);
  }
  if (!datum_is_the_symbol(list_at(e, 0), "bang")) {
    datum res = *datum_make_nil();

    for (int i = 0; i < list_length(e); ++i) {
      datum *x = list_at(e, i);
      fdatum nxt =
          datum_expand(x, sl, routine, p, compdata, bp, builder_compdata);
      if (fdatum_is_panic(nxt)) {
        return nxt;
      }
      list_append(&res, &nxt.ok_value);
    }
    return fdatum_make_ok(res);
  }
  if (list_length(e) != 2) {
    return fdatum_make_panic("! should be used with a single arg");
  }
  fdatum exp = datum_expand(list_at(e, 1), sl, routine, p, compdata,
                            bp, builder_compdata);
  if (fdatum_is_panic(exp)) {
    return exp;
  }
  datum mod = *datum_make_list_of(exp.ok_value);
  char *err = prog_build(sl, p, bp, &mod, compdata,
                         builder_compdata, datum_make_bytestring("c-prelude"));
  if (err != NULL) {
    char err2[256];
    err2[0] = 0;
    strcat(err2, "error while compiling a macro: ");
    strcat(err2, err);
    return fdatum_make_panic(err2);
  }
  fdatum res = routine_run_in_ffi_host(*sl, routine);
  if (fdatum_is_panic(res)) {
    return res;
  }
  if (list_length(&res.ok_value) == 0) {
    return fdatum_make_ok(*datum_make_symbol(":void-value"));
  }
  if (list_length(&res.ok_value) != 1) {
    return fdatum_make_panic("expected a single result while preprocessing");
  }
  return fdatum_make_ok(*list_at(&res.ok_value, 0));
}
