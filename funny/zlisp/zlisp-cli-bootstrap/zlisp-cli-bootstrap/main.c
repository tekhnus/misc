// a basic CLI for zlisp interpreter.
#include <unistd.h>
#include <zlisp-impl/main.h>

static fdatum_t module_source(char *module);
static fdatum_t file_source(char *file);

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
  prog_t *p = prog_make();
  fdatum_t src = file_source(filename_copy);
  if (fdatum_is_panic(src)) {
    fprintf(stderr, "file error: %s\n", src.panic_message);
    return EXIT_FAILURE;
  }

  char *err = prog_init_from_source(p, src.ok_value, module_source);
  if (err != NULL) {
    fprintf(stderr, "compilation error: %s\n", err);
    return EXIT_FAILURE;
  }
  fstate_t prelude = fstate_make_prelude();
  if (fstate_is_panic(prelude)) {
    fprintf(stderr, "prelude error: %s\n", prelude.panic_message);
    return EXIT_FAILURE;
  }
  state_t *s = prelude.ok_value;
  fstate_t ns = state_eval(routine_make(p, s));
  if (fstate_is_panic(ns)) {
    fprintf(stderr, "runtime error: %s\n", ns.panic_message);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}

static fdatum_t datum_expand(datum_t *e, state_t *ctxt);

fdatum_t module_source(char *module) {
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

static fdatum_t file_source(char *fname) {
  FILE *stre = fopen(fname, "r");
  if (stre == NULL) {
    char *err = malloc(1024);
    err[0] = 0;
    strcat(err, "Module not found: ");
    strcat(err, fname);
    return fdatum_make_panic(err);
  }

  fstate_t prelude = fstate_make_prelude();
  if (fstate_is_panic(prelude)) {
    return fdatum_make_panic(prelude.panic_message);
  }
  read_result_t rr;
  datum_t *res = datum_make_nil();
  datum_t **resend = &res;
  for (; read_result_is_ok(rr = datum_read(stre));) {
    fdatum_t exp = datum_expand(rr.ok_value, prelude.ok_value);
    if (fdatum_is_panic(exp)) {
      return exp;
    }
    *resend = datum_make_list(exp.ok_value, datum_make_nil());
    resend = &((*resend)->list_tail);
  }
  return fdatum_make_ok(res);
}

static fdatum_t datum_expand(datum_t *e, state_t *ctxt) {
  if (!datum_is_list(e) || datum_is_nil(e)) {
    return fdatum_make_ok(e);
  }
  if (!datum_is_symbol(e->list_head) ||
      strcmp(e->list_head->symbol_value, "bang")) {
    return list_map(datum_expand, e, ctxt);
  }
  if (datum_is_nil(e->list_tail) || !datum_is_nil(e->list_tail->list_tail)) {
    return fdatum_make_panic("! should be used with a single arg");
  }
  fdatum_t exp = datum_expand(e->list_tail->list_head, ctxt);
  if (fdatum_is_panic(exp)) {
    return exp;
  }
  fstate_t ev = datum_eval(exp.ok_value, ctxt);
  if (fstate_is_panic(ev)) {
    return fdatum_make_panic(ev.panic_message);
  }
  fdatum_t res = state_stack_peek(ev.ok_value);
  if (fdatum_is_panic(res)) {
    return res;
  }
  return fdatum_make_ok(res.ok_value);
}
