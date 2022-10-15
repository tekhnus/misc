#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <extern.h>

EXPORT char *prog_build(prog_slice *sl, size_t ep, datum *source, fdatum (*module_source)(prog_slice *sl, size_t *p, char *), datum **compdata) {
  size_t run_main_off = prog_slice_append_new(sl);
  size_t run_main_end = run_main_off;
  datum *dup_compdata = *compdata;
  fdatum res = prog_init_submodule(sl, &run_main_end, source, compdata, datum_make_list_1(datum_make_void()));
  // fprintf(stderr, "!!!!! %s\n", datum_repr(source));
  if (fdatum_is_panic(res)) {
    // fprintf(stderr, "finita %s %s\n", datum_repr(source), res.panic_message);
    return res.panic_message;
  }
  char *err = prog_build_deps_isolated(sl, &ep, res.ok_value->list_head, module_source, &dup_compdata);
  // fprintf(stderr, "!!!!! %s\n", datum_repr(source));
  if (err != NULL) {
    return err;
  }
  *prog_slice_datum_at(*sl, ep) = *prog_slice_datum_at(*sl, run_main_off);
  return NULL;
}

EXPORT char *prog_build_one(prog_slice *sl, size_t ep, datum *stmt_or_spec,
                       fdatum (*module_source)(prog_slice *sl, size_t *p,
                                               char *), datum **compdata) {
  datum *spec = datum_make_list_1(datum_make_symbol("req"));
  datum *stmts = datum_make_list_1(datum_make_symbol(":void-value"));
  if (datum_is_list(stmt_or_spec) && !datum_is_nil(stmt_or_spec) && datum_is_the_symbol(stmt_or_spec->list_head, "req")) {
    spec = stmt_or_spec; 
  } else {
    stmts = datum_make_list_1(stmt_or_spec);
  }
  return prog_build(sl, ep, datum_make_list(spec, stmts), module_source, compdata);
}

LOCAL char *prog_build_deps_isolated(prog_slice *sl, size_t *p, datum *deps, fdatum (*module_source)(prog_slice *sl, size_t *p, char *), datum **compdata) {
  // fprintf(stderr, "!!!!! %s\n", datum_repr(deps));
  size_t bdr_off = prog_slice_append_new(sl);
  size_t bdr_end = bdr_off;
  datum *bdr_compdata = compdata_make();
  prog_append_nop(sl, &bdr_end, datum_make_list_2(datum_make_symbol("info"), datum_make_list_1(datum_make_symbol("build-deps-isolated"))));
  prog_append_recieve(sl, &bdr_end, datum_make_nil(), &bdr_compdata);  // bdr is callable with zero arguments
  datum *state = datum_make_nil();
  char *err = prog_build_deps(&state, sl, &bdr_end, deps, module_source, &bdr_compdata);
  if (err != NULL) {
    return err;
  }
  prog_put_deps(sl, &bdr_end, deps, &bdr_compdata);
  prog_append_yield(sl, &bdr_end, false, list_length(deps), datum_make_nil(), &bdr_compdata);
  prog_append_put_prog(sl, p, bdr_off, 0, compdata);
  prog_append_collect(sl, 1, p, compdata);
  prog_append_call(sl, p, false, list_length(deps), compdata);
  prog_append_pop(sl, p, datum_make_symbol(":void"), compdata);
  return NULL;
}

LOCAL char *prog_build_deps(datum **state, prog_slice *sl, size_t *p, datum *deps, fdatum (*module_source)(prog_slice *sl, size_t *p, char *), datum **compdata) {
  for (datum *rest_deps = deps; !datum_is_nil(rest_deps); rest_deps=rest_deps->list_tail) {
    datum *dep = rest_deps->list_head;
    char *err = prog_build_dep(state, sl, p, dep, module_source, compdata);
    if (err != NULL) {
      return err;
    }
  }
  return NULL;
}

LOCAL void prog_put_deps(prog_slice *sl, size_t *p, datum *deps, datum **compdata) {
  for (datum *rest_deps = deps; !datum_is_nil(rest_deps); rest_deps=rest_deps->list_tail) {
    datum *dep = rest_deps->list_head;
    prog_append_put_var(sl, p, datum_make_symbol(get_varname(dep)), compdata);
  }
}

LOCAL char *get_varname(datum *dep_and_sym) {
  char *dep = dep_and_sym->list_head->bytestring_value;
  char *sym;
  if (!datum_is_nil(dep_and_sym->list_tail)) {
    sym = dep_and_sym->list_tail->list_head->symbol_value;
  } else {
    sym = "";
  }
  char *res = malloc(1024);
  res[0] = 0;
  strcat(res, dep);
  strcat(res, "__");
  strcat(res, sym);
  return res;
}

LOCAL datum *list_append(datum *x, datum *y) {
  if (!datum_is_list(x)) {
    fprintf(stderr, "list_append failed\n");
    exit(EXIT_FAILURE);
  }
  if (datum_is_nil(x)) {
    return datum_make_list_1(y);
  }
  return datum_make_list(x->list_head, list_append(x->list_tail, y));
}

LOCAL char *prog_build_dep(datum **state, prog_slice *sl, size_t *p, datum *dep_and_sym, fdatum (*module_source)(prog_slice *sl, size_t *p, char *), datum **compdata) {
  if (!datum_is_list(dep_and_sym) || datum_is_nil(dep_and_sym) || !datum_is_bytestring(dep_and_sym->list_head)){
    return "req expects bytestrings";
  }
  datum *dep = dep_and_sym->list_head;

  bool already_built = false;
  for (datum *rest_state=*state; !datum_is_nil(rest_state); rest_state=rest_state->list_tail) {
    datum *b = rest_state->list_head;
    if (datum_eq(dep_and_sym, b)) {
      already_built = true;
      break;
    }
  }
  if (already_built) {
    return NULL;
  }
  // fprintf(stderr, "!!!!!! %s\n", datum_repr(dep_and_sym));
  size_t run_dep_off = prog_slice_append_new(sl);
  size_t run_dep_end = run_dep_off;
  prog_append_nop(sl, &run_dep_end, datum_make_list_2(datum_make_symbol("info"), datum_make_list_1(dep)));
  fdatum status = module_source(sl, &run_dep_end, dep->bytestring_value);
  if (fdatum_is_panic(status)) {
    return status.panic_message;
  }
  datum *transitive_deps = status.ok_value->list_head;
  datum *syms = status.ok_value->list_tail->list_head;
  prog_append_yield(sl, &run_dep_end, false, list_length(syms), datum_make_nil(), NULL);
  char *err = prog_build_deps(state, sl, p, transitive_deps, module_source, compdata);
  if (err != NULL) {
    return err;
  }
  prog_append_put_prog(sl, p, run_dep_off, 0, compdata);
  prog_put_deps(sl, p, transitive_deps, compdata);
  prog_append_collect(sl, 1 + list_length(transitive_deps), p, compdata);
  prog_append_call(sl, p, false, 1 + list_length(syms), compdata);
  prog_append_pop(sl, p, datum_make_symbol(":void"), compdata);
  datum *names = datum_make_nil();
  for (datum *rest_syms = syms; !datum_is_nil(rest_syms); rest_syms=rest_syms->list_tail) {
    datum *sym = rest_syms->list_head;
    names = list_append(names, datum_make_symbol(get_varname(datum_make_list_2(dep, sym))));
    *state = datum_make_list(datum_make_list_2(dep, sym), *state);
  }
  prog_append_pop(sl, p, names, compdata);
  return NULL;
}
