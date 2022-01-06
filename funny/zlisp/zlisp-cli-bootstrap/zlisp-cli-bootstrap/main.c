// a basic CLI for zlisp interpreter.
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp-impl/main.h>

static fdatum module_source(char *module);
static fdatum file_source(char *file);

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("usage: %s <script>\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  char filename_copy[1024] = {0};

  if (argv[1][0] != '/') {
    getcwd(filename_copy, sizeof(filename_copy));
    strcat(filename_copy, "/");
  }
  strcat(filename_copy, argv[1]);
  fdatum src = file_source(filename_copy);
  if (fdatum_is_panic(src)) {
    fprintf(stderr, "file error: %s\n", src.panic_message);
    return EXIT_FAILURE;
  }
  prog *p = prog_make();
  char *err = prog_init_module(p, src.ok_value, module_source);
  if (err != NULL) {
    fprintf(stderr, "compilation error: %s\n", err);
    return EXIT_FAILURE;
  }
  routine r = routine_make(p, state_make_builtins());
  fstate ns = routine_run(r);
  if (fstate_is_panic(ns)) {
    fprintf(stderr, "runtime error: %s\n", ns.panic_message);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

static fstate datum_expand(datum *e, state *ctxt);

fdatum module_source(char *module) {
  char fname[1024] = {};
  char *zlisp_home = getenv("ZLISP");
  if (zlisp_home == NULL) {
    return fdatum_make_panic("ZLISP variable not defined");
  }
  strcat(fname, zlisp_home);
  strcat(fname, "/");
  strcat(fname, module);
  strcat(fname, "/main.lisp");

  return file_source(fname);
}

static fdatum file_source(char *fname) {
  FILE *stre = fopen(fname, "r");
  if (stre == NULL) {
    char *err = malloc(1024);
    err[0] = 0;
    strcat(err, "Module not found: ");
    strcat(err, fname);
    return fdatum_make_panic(err);
  }

  state *expander_state = state_make_builtins();
  read_result rr;
  datum *res = datum_make_nil();
  datum **resend = &res;
  for (; read_result_is_ok(rr = datum_read(stre));) {

    fstate exp = datum_expand(rr.ok_value, expander_state);
    if (fstate_is_panic(exp)) {
      return fdatum_make_panic(exp.panic_message);
    }
    expander_state = exp.ok_value;
    datum *val = state_value_pop(&expander_state);

    if (datum_is_void(val)) {
      // to support things like !(def x 42)
      continue;
    }
    *resend = datum_make_list(val, datum_make_nil());
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

static fstate datum_expand(datum *e, state *ctxt) {
  if (!datum_is_list(e) || datum_is_nil(e)) {
    state_value_put(&ctxt, e);
    return fstate_make_ok(ctxt);
  }
  if (!datum_is_symbol(e->list_head) ||
      strcmp(e->list_head->symbol_value, "bang")) {
    datum *res = datum_make_nil();
    datum **end = &res;

    for (datum *rest = e; !datum_is_nil(rest); rest=rest->list_tail) {
      datum *x = rest->list_head;
      fstate nxt = datum_expand(x, ctxt);
      if (fstate_is_panic(nxt)) {
        return nxt;
      }
      ctxt = nxt.ok_value;
      datum *y = state_value_pop(&ctxt);
      *end = datum_make_list(y, datum_make_nil());
      end = &((*end)->list_tail);
    }
    state_value_put(&ctxt, res);
    return fstate_make_ok(ctxt);
  }
  if (datum_is_nil(e->list_tail) || !datum_is_nil(e->list_tail->list_tail)) {
    return fstate_make_panic("! should be used with a single arg");
  }
  fstate exp = datum_expand(e->list_tail->list_head, ctxt);
  if (fstate_is_panic(exp)) {
    return exp;
  }
  ctxt = exp.ok_value;
  datum *preext = state_value_pop(&ctxt);
  char *err = state_value_eval(&ctxt, preext, module_source);
  if (err != NULL) {
    return fstate_make_panic(err);
  }
  return fstate_make_ok(ctxt);
}
