#include <preprocessing.h>

EXPORT fdatum file_source(char *fname) {
  FILE *stre = fopen(fname, "r");
  if (stre == NULL) {
    char *err = malloc(1024);
    err[0] = 0;
    strcat(err, "Module not found: ");
    strcat(err, fname);
    return fdatum_make_panic(err);
  }

  prog_slice expander_sl = prog_slice_make(16 * 1024);
  size_t expander_prg = prog_slice_append_new(&expander_sl);
  size_t expander_builder_prg = prog_slice_append_new(&expander_sl);
  datum *expander_routine = routine_make_new(expander_builder_prg);
  datum *expander_compdata = compdata_make();
  datum *expander_builder_compdata = compdata_make();
  prog_build_init(&expander_sl, &expander_prg, &expander_builder_prg, &expander_compdata, &expander_builder_compdata);
  read_result rr;
  datum *res = datum_make_nil();
  datum **resend = &res;
  for (; read_result_is_ok(rr = datum_read(stre));) {
    fdatum val = datum_expand(rr.ok_value, &expander_sl, &expander_routine, &expander_prg, &expander_compdata, &expander_builder_prg, &expander_builder_compdata);
    if (fdatum_is_panic(val)) {
      char *err = malloc(1024);
      char *end = err;
      end += sprintf(end, "while expanding %s: %s", datum_repr(rr.ok_value), val.panic_message);
      return fdatum_make_panic(err);
    }
    if (datum_is_the_symbol(val.ok_value, ":void-value")) {
      continue;
    }
    *resend = datum_make_list(val.ok_value, datum_make_nil());
    resend = &((*resend)->list_tail);
  }
  if (read_result_is_panic(rr)) {
    return fdatum_make_panic(rr.panic_message);
  }
  if (read_result_is_right_paren(rr)) {
    return fdatum_make_panic("unmatched right paren");
  }
  return fdatum_make_ok(res);
}

LOCAL fdatum datum_expand(datum *e, prog_slice *sl, datum **routine, size_t *p, datum **compdata, size_t *bp, datum **builder_compdata) {
  if (!datum_is_list(e) || datum_is_nil(e)) {
    return fdatum_make_ok(e);
  }
  if (!datum_is_symbol(e->list_head) ||
      strcmp(e->list_head->symbol_value, "bang")) {
    datum *res = datum_make_nil();
    datum **end = &res;

    for (datum *rest = e; !datum_is_nil(rest); rest = rest->list_tail) {
      datum *x = rest->list_head;
      fdatum nxt = datum_expand(x, sl, routine, p, compdata, bp, builder_compdata);
      if (fdatum_is_panic(nxt)) {
        return nxt;
      }
      *end = datum_make_list(nxt.ok_value, datum_make_nil());
      end = &((*end)->list_tail);
    }
    return fdatum_make_ok(res);
  }
  if (datum_is_nil(e->list_tail) || !datum_is_nil(e->list_tail->list_tail)) {
    return fdatum_make_panic("! should be used with a single arg");
  }
  fdatum exp = datum_expand(e->list_tail->list_head, sl, routine, p, compdata, bp, builder_compdata);
  if (fdatum_is_panic(exp)) {
    return exp;
  }
  char *err = prog_build(
                                sl, p, bp, datum_make_list_1(exp.ok_value), compdata, builder_compdata, datum_make_bytestring("c-prelude"));
  if (err != NULL) {
    char *err2 = malloc(256);
    err2[0] = 0;
    strcat(err2, "error while compiling a macro: ");
    strcat(err2, err);
    return fdatum_make_panic(err2);
  }
  fdatum res = routine_run_and_get_value_c_host_new_new(*sl, routine);
  if (fdatum_is_panic(res)) {
    return res;
  }
  if (!compdata_has_value(*compdata)) {
    return fdatum_make_ok(datum_make_symbol(":void-value"));
  }
  return res;
}
