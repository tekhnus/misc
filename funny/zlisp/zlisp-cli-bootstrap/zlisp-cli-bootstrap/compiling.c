#include <zlisp-cli-bootstrap/compiling.h>
#if INTERFACE
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp-impl/zlisp-impl.h>
#endif

char *prog_init_module_c_host(prog *p, datum *source) {
  return prog_init_module(p, source, module_routine);
}

routine module_routine(char *module) {
  routine r = routine_make_null();
  prog *p = module_prog(module);
  if (p == NULL) {
    return routine_make_null();
  }
  return routine_make(p, state_make_builtins());
}

LOCAL prog *module_prog(char *module) {
  fdatum src = module_source(module);
  if (fdatum_is_panic(src)) {
    fprintf(stderr, "syntax error in required module: %s\n", src.panic_message);
    return NULL;
  }
  prog *p = prog_make();
  char *err = prog_init_module_c_host(p, src.ok_value);
  if (err != NULL) {
    fprintf(stderr, "error in required module: %s\n", err);
    return NULL;
  }
  return p;
}

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

fdatum file_source(char *fname) {
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
    fdatum val = datum_expand(rr.ok_value, &expander_state);
    if (fdatum_is_panic(val)) {
      return val;
    }

    if (datum_is_void(val.ok_value)) {
      // to support things like !(def x 42)
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

LOCAL fdatum datum_expand(datum *e, state **ctxt) {
  if (!datum_is_list(e) || datum_is_nil(e)) {
    return fdatum_make_ok(e);
  }
  if (!datum_is_symbol(e->list_head) ||
      strcmp(e->list_head->symbol_value, "bang")) {
    datum *res = datum_make_nil();
    datum **end = &res;

    for (datum *rest = e; !datum_is_nil(rest); rest=rest->list_tail) {
      datum *x = rest->list_head;
      fdatum nxt = datum_expand(x, ctxt);
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
  fdatum exp = datum_expand(e->list_tail->list_head, ctxt);
  if (fdatum_is_panic(exp)) {
    return exp;
  }
  prog *p = prog_make();
  char *err = prog_init_module_c_host(p, datum_make_list(exp.ok_value, datum_make_nil()));
  if (err != NULL) {
    char *err2 = malloc(256);
    err2[0] = 0;
    strcat(err2, "error while compiling a macro: ");
    strcat(err2, err);
    return fdatum_make_panic(err2);
  }
  return routine_run_and_get_value_c_host(ctxt, p);
}
