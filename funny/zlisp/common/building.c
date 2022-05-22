#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <extern.h>

EXPORT char *prog_build(prog_slice *sl, prog *entrypoint, datum *source, fdatum (*module_source)(prog_slice *sl, prog *p, char *)) {
  prog *run_main = prog_slice_append_new(sl);
  fdatum res = prog_init_submodule(sl, run_main, source);
  if (fdatum_is_panic(res)) {
    return res.panic_message;
  }
  prog_append_args(sl, &entrypoint);
  prog_append_put_prog(sl, &entrypoint, run_main, 0);
  char *err = prog_build_deps(sl, &entrypoint, res.ok_value->list_tail->list_head, module_source);
  if (err != NULL) {
    return err;
  }
  prog_append_collect(sl, &entrypoint);
  prog_append_call(sl, &entrypoint, false);
  return NULL;
}

EXPORT char *prog_build_one(prog_slice *sl, prog *s, datum *stmt,
                       fdatum (*module_source)(prog_slice *sl, prog *p,
                                              char *)) {
  if (datum_is_list(stmt) && !datum_is_nil(stmt) && datum_is_the_symbol(stmt->list_head, "req")) {
    fdatum res = prog_read_usages(stmt);
    if (fdatum_is_panic(res)) {
      return res.panic_message;
    }
    char *err = prog_build_deps(sl, &s, res.ok_value->list_tail->list_head, module_source);
    if (err != NULL) {
      return err;
    }
    for (datum *rest_deps=res.ok_value->list_head; !datum_is_nil(rest_deps); rest_deps=rest_deps->list_tail) {
      datum *dep_var = rest_deps->list_head;
      prog_append_pop(sl, &s, dep_var);
    }
    prog_append_put_const(sl, &s, datum_make_void());
    return NULL;
  }
  return prog_append_statement(sl, &s, stmt);
}

LOCAL char *prog_build_deps(prog_slice *sl, prog **p, datum *deps, fdatum (*module_source)(prog_slice *sl, prog *p, char *)) {
  // fprintf(stderr, "!!!!!!!!! building deps %s\n", datum_repr(deps));
  for (datum *rest_deps = deps; !datum_is_nil(rest_deps); rest_deps=rest_deps->list_tail) {
    datum *dep = rest_deps->list_head;
    if (!datum_is_bytestring(dep)) {
      return "req expects bytestrings";
    }
    prog *run_dep = prog_slice_append_new(sl);
    fdatum status = module_source(sl, run_dep, dep->bytestring_value);
    if (fdatum_is_panic(status)) {
      return status.panic_message;
    }
    prog_append_args(sl, p);
    prog_append_put_prog(sl, p, run_dep, 0);
    char *err = prog_build_deps(sl, p, status.ok_value->list_tail->list_head, module_source);
    if (err != NULL) {
      return err;
    }
    prog_append_collect(sl, p);
    prog_append_call(sl, p, false);
  }
  return NULL;
}
