#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <extern.h>

EXPORT char *prog_build(prog_slice *sl, prog *entrypoint, datum *source, fdatum (*module_source)(prog_slice *sl, prog **p, char *)) {
  prog *run_main = prog_slice_append_new(sl);
  prog *run_main_end = run_main;
  fdatum res = prog_init_submodule(sl, &run_main_end, source);
  if (fdatum_is_panic(res)) {
    return res.panic_message;
  }
  char *err = prog_build_deps_isolated(sl, &entrypoint, res.ok_value->list_tail->list_head, module_source);
  if (err != NULL) {
    return err;
  }
  *entrypoint = *run_main;
  return NULL;
}

EXPORT char *prog_build_one(prog_slice *sl, prog *s, datum *stmt,
                       fdatum (*module_source)(prog_slice *sl, prog **p,
                                              char *)) {
  datum *reqspec = datum_make_list_1(datum_make_symbol("req"));
  datum *stmts = datum_make_nil();
  if (datum_is_list(stmt) && !datum_is_nil(stmt) && datum_is_the_symbol(stmt->list_head, "req")) {
    reqspec = stmt; 
  } else {
    stmts = datum_make_list_1(stmt);
  }
  prog *run_stmt = prog_slice_append_new(sl);
  prog *run_stmt_end = run_stmt;
  fdatum res = prog_init_submodule(sl, &run_stmt_end, datum_make_list(reqspec, stmts));
  if (fdatum_is_panic(res)) {
    return res.panic_message;
  }
  char *err = prog_build_deps_isolated(sl, &s, res.ok_value->list_tail->list_head, module_source);
  if (err != NULL) {
    return err;
  }
  *s = *run_stmt;
  return NULL;
}

LOCAL char *prog_build_deps_isolated(prog_slice *sl, prog **p, datum *deps, fdatum (*module_source)(prog_slice *sl, prog **p, char *)) {
  prog *bdr = prog_slice_append_new(sl);
  prog *bdr_end = bdr;
  prog_append_pop(sl, &bdr_end, datum_make_symbol(":void"));
  prog_append_args(sl, &bdr_end);
  char *err = prog_build_deps(sl, &bdr_end, deps, module_source);
  if (err != NULL) {
    return err;
  }
  prog_append_collect(sl, &bdr_end);
  prog_append_return(sl, &bdr_end, false);
  prog_append_args(sl, p);
  prog_append_put_prog(sl, p, bdr, 0);
  prog_append_collect(sl, p);
  prog_append_call(sl, p, false);
  return NULL;
}

LOCAL char *prog_build_deps(prog_slice *sl, prog **p, datum *deps, fdatum (*module_source)(prog_slice *sl, prog **p, char *)) {
  // fprintf(stderr, "!!!!!!!!! building deps %s\n", datum_repr(deps));
  for (datum *rest_deps = deps; !datum_is_nil(rest_deps); rest_deps=rest_deps->list_tail) {
    datum *dep = rest_deps->list_head;
    if (!datum_is_bytestring(dep)) {
      return "req expects bytestrings";
    }
    prog *run_dep = prog_slice_append_new(sl);
    prog *run_dep_end = run_dep;
    fdatum status = module_source(sl, &run_dep_end, dep->bytestring_value);
    if (fdatum_is_panic(status)) {
      return status.panic_message;
    }
    prog_append_yield(sl, &run_dep_end, false);
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
