#include <compiling.h>
#if INTERFACE
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <zlisp/common.h>
#endif

char *prog_build_c_host(prog_slice *sl, size_t *p, size_t *bp, datum *source, datum **compdata, datum **builder_compdata) {
  return prog_build_2(sl, p, bp, source, module_routine, compdata, builder_compdata);
}

char *prog_build_one_c_host(prog_slice *sl, size_t p, datum *source, datum **compdata) {
  return prog_build_one(sl, p, source, module_routine, compdata);
}

char *prog_build_one_c_host_2(prog_slice *sl, size_t *p, size_t *bp, datum *source, datum **compdata, datum **builder_compdata) {
  return prog_build_one_2(sl, p, bp, source, module_routine, compdata, builder_compdata);
}

char *module_routine(prog_slice *sl, size_t *p, char *module) {
  fdatum src = module_source(module);
  if (fdatum_is_panic(src)) {
    return src.panic_message;
  }
  datum *compdata = compdata_make();
  return prog_init_submodule(sl, p, src.ok_value, &compdata, datum_make_list_1(datum_make_symbol(module)));
}

fdatum module_source(char *module) {
  if (!strcmp(module, "prelude")) {
    module = "c-prelude";
  }
  char *fname = module_to_filename(module);
  return file_source(fname);
}

char *module_to_filename(char *module) {
  char *fname = malloc(1024);
  char *zlisp_home = getenv("ZLISP");
  if (zlisp_home == NULL) {
    fprintf(stderr, "ZLISP variable not defined");
    exit(EXIT_FAILURE);
  }
  strcat(fname, zlisp_home);
  strcat(fname, "/");
  strcat(fname, module);
  strcat(fname, "/main.lisp");
  return fname;
}

fdatum file_source(char *fname) {
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
  datum *expander_routine = routine_2_make(expander_builder_prg);
  datum *expander_compdata = compdata_make();
  datum *expander_builder_compdata = compdata_make();
  prog_build_init(&expander_sl, &expander_prg, &expander_builder_prg, &expander_compdata, &expander_builder_compdata);
  // routine_run_and_get_value_c_host_new(expander_sl, &expander_routine);
  read_result rr;
  datum *res = datum_make_nil();
  datum **resend = &res;
  // printf("start expanding %s\n", fname);
  for (; read_result_is_ok(rr = datum_read(stre));) {
    // printf("preparing to expand a statement\n");
    fdatum val = datum_expand(rr.ok_value, &expander_sl, &expander_routine, &expander_prg, &expander_compdata, &expander_builder_prg, &expander_builder_compdata);
    // printf("expanded a statement\n");
    if (fdatum_is_panic(val)) {
      char *err = malloc(1024);
      char *end = err;
      end += sprintf(end, "while expanding %s: %s", datum_repr(rr.ok_value), val.panic_message);
      return fdatum_make_panic(err);
    }
    // printf("%s\n", datum_repr(val.ok_value));
    if (datum_is_the_symbol(val.ok_value, ":void-value")) {
      // to support things like !(def x 42)
      continue;
    }
    *resend = datum_make_list(val.ok_value, datum_make_nil());
    resend = &((*resend)->list_tail);
  }
  // printf("expanded all statements in %s\n", fname);
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
  char *err = prog_build_one_c_host_2(
                                      sl, p, bp, exp.ok_value, compdata, builder_compdata);
  if (err != NULL) {
    char *err2 = malloc(256);
    err2[0] = 0;
    strcat(err2, "error while compiling a macro: ");
    strcat(err2, err);
    return fdatum_make_panic(err2);
  }
  fdatum res = routine_run_and_get_value_c_host_new(*sl, routine);
  size_t p_alt = routine_2_get_offset(*routine);
  if (p_alt != *p) {
    return fdatum_make_panic("suddenly p_alt != p");
  }
  return res;
}
