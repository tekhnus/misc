#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <extern.h>

EXPORT char *prog_build(prog_slice *sl, size_t ep, datum *source, fdatum (*module_source)(prog_slice *sl, prog **p, char *)) {
  prog *entrypoint = prog_slice_at(*sl, ep);
  size_t run_main_off = prog_slice_append_new(sl);
  prog *run_main = prog_slice_at(*sl, run_main_off);
  prog *run_main_end = run_main;
  // fprintf(stderr, "!!!!! %s\n", datum_repr(source));
  fdatum res = prog_init_submodule(sl, &run_main_end, source);
  // fprintf(stderr, "!!!!! %s\n", datum_repr(source));
  if (fdatum_is_panic(res)) {
    // fprintf(stderr, "finita %s %s\n", datum_repr(source), res.panic_message);
    return res.panic_message;
  }
  char *err = prog_build_deps_isolated(sl, &entrypoint, res.ok_value->list_tail->list_head, module_source);
  // fprintf(stderr, "!!!!! %s\n", datum_repr(source));
  if (err != NULL) {
    return err;
  }
  *entrypoint = *run_main;
  return NULL;
}

EXPORT char *prog_build_one(prog_slice *sl, size_t ep, datum *stmt_or_spec,
                       fdatum (*module_source)(prog_slice *sl, prog **p,
                                              char *)) {
  datum *spec = datum_make_list_1(datum_make_symbol("req"));
  datum *stmts = datum_make_nil();
  if (datum_is_list(stmt_or_spec) && !datum_is_nil(stmt_or_spec) && datum_is_the_symbol(stmt_or_spec->list_head, "req")) {
    spec = stmt_or_spec; 
  } else {
    stmts = datum_make_list_1(stmt_or_spec);
  }
  return prog_build(sl, ep, datum_make_list(spec, stmts), module_source);
}

LOCAL char *prog_build_deps_isolated(prog_slice *sl, prog **p, datum *deps, fdatum (*module_source)(prog_slice *sl, prog **p, char *)) {
  // fprintf(stderr, "!!!!! %s\n", datum_repr(deps));
  size_t bdr_off = prog_slice_append_new(sl);
  prog *bdr = prog_slice_at(*sl, bdr_off);
  prog *bdr_end = bdr;
  prog_append_pop(sl, &bdr_end, datum_make_symbol(":void"));
  prog_append_args(sl, &bdr_end);
  datum *state = datum_make_nil();
  char *err = prog_build_deps(&state, sl, &bdr_end, deps, module_source);
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

LOCAL char *prog_build_deps(datum **state, prog_slice *sl, prog **p, datum *deps, fdatum (*module_source)(prog_slice *sl, prog **p, char *)) {
  for (datum *rest_deps = deps; !datum_is_nil(rest_deps); rest_deps=rest_deps->list_tail) {
    datum *dep = rest_deps->list_head;
    if (!datum_is_bytestring(dep)) {
      return "req expects bytestrings";
    }
    char *err = prog_build_dep(state, sl, p, dep, module_source);
    if (err != NULL) {
      return err;
    }
  }
  return NULL;
}

LOCAL char *prog_build_dep(datum **state, prog_slice *sl, prog **p, datum *dep, fdatum (*module_source)(prog_slice *sl, prog **p, char *)) {
  bool already_built = false;
  for (datum *rest_state=*state; !datum_is_nil(rest_state); rest_state=rest_state->list_tail) {
    datum *b = rest_state->list_head;
    if (datum_eq(dep, b)) {
      already_built = true;
      break;
    }
  }
  if (already_built) {
    prog_append_put_var(sl, p, datum_make_symbol(datum_repr(dep)));
    return NULL;
  }
  size_t run_dep_off = prog_slice_append_new(sl);
  prog *run_dep = prog_slice_at(*sl, run_dep_off);
  prog *run_dep_end = run_dep;
  fdatum status = module_source(sl, &run_dep_end, dep->bytestring_value);
  if (fdatum_is_panic(status)) {
    return status.panic_message;
  }
  prog_append_yield(sl, &run_dep_end, false);
  prog_append_args(sl, p);
  prog_append_put_prog(sl, p, run_dep, 0);
  char *err = prog_build_deps(state, sl, p, status.ok_value->list_tail->list_head, module_source);
  if (err != NULL) {
    return err;
  }
  prog_append_collect(sl, p);
  prog_append_call(sl, p, false);
  prog_append_pop(sl, p, datum_make_symbol(datum_repr(dep)));
  prog_append_put_var(sl, p, datum_make_symbol(datum_repr(dep)));
  *state = datum_make_list(dep, *state);
  return NULL;
}
