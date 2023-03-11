#include <extern.h>
#include <linking.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

EXPORT size_t prog_build_init(vec *sl, size_t *ep, size_t *bdr_p,
                              datum **compdata, datum **builder_compdata) {
  datum nil = *datum_make_nil();
  prog_append_yield(sl, bdr_p, datum_make_symbol("halt"), 0, 0,
                    nil, builder_compdata);
  prog_append_put_prog(sl, bdr_p, *ep, 0, builder_compdata);
  prog_append_call(sl, bdr_p, datum_make_list_of(*compdata_get_top_polyindex(*builder_compdata)),
                   false, datum_make_symbol("plain"), 0, 0,
                   builder_compdata);
  prog_append_yield(sl, ep, datum_make_symbol("plain"), 0, 0, nil,
                    compdata);
  return 42;
}

EXPORT char *prog_link_deps(vec *sl, size_t *bdr_p,
                            datum **builder_compdata, size_t p,
                            fdatum (*module_bytecode)(char *, datum *),
                            datum *settings) {
  datum *input_meta = extract_meta(*sl, p);
  if (input_meta == NULL) {
    return NULL;
  }
  compdata_give_names(datum_make_list_of(*datum_make_symbol("__main__")),
                      builder_compdata);
  char *err = prog_build_deps(sl, bdr_p, input_meta, module_bytecode, settings,
                              builder_compdata);
  if (err != NULL) {
    return err;
  }
  prog_append_put_var(sl, bdr_p, datum_make_symbol("__main__"),
                      builder_compdata);
  datum fn_index = *compdata_get_top_polyindex(*builder_compdata);
  prog_put_deps(sl, bdr_p, input_meta, builder_compdata);
  prog_append_call(sl, bdr_p, datum_make_list_of(fn_index), false,
                   datum_make_symbol("plain"), list_length(input_meta), 0,
                   builder_compdata);
  return NULL;
}

LOCAL char *prog_build_deps(vec *sl, size_t *p, datum *deps,
                            fdatum (*module_bytecode)(char *, datum *),
                            datum *settings, datum **compdata) {
  for (int i = 0; i < list_length(deps); ++i) {
    datum *dep = list_at(deps, i);
    char *err = prog_build_dep(sl, p, dep, module_bytecode, settings, compdata);
    if (err != NULL) {
      return err;
    }
  }
  return NULL;
}

LOCAL void prog_put_deps(vec *sl, size_t *p, datum *deps,
                         datum **compdata) {
  char varname[1024];
  for (int i = 0; i < list_length(deps); ++i) {
    datum *dep = list_at(deps, i);
    get_varname(varname, dep);
    prog_append_put_var(sl, p, datum_make_symbol(varname), compdata);
  }
}

LOCAL void get_varname(char *res, datum *dep_and_sym) {
  char *dep = list_at(dep_and_sym, 0)->bytestring_value;
  char *sym;
  if (list_length(dep_and_sym) > 1) {
    sym = list_at(dep_and_sym, 1)->symbol_value;
  } else {
    sym = "";
  }
  res[0] = 0;
  strcat(res, dep);
  strcat(res, "__");
  strcat(res, sym);
}

LOCAL datum *offset_relocate(datum *ins, size_t delta) {
  if (!datum_is_integer(ins)) {
    fprintf(stderr, "error: offset_relocate");
    exit(EXIT_FAILURE);
  }
  return datum_make_int(ins->integer_value + delta);
}

LOCAL datum instruction_relocate(datum *ins, size_t delta) {
  if (datum_is_the_symbol(list_at(ins, 0), ":end")) {
    return *datum_make_list_of(datum_copy(list_at(ins, 0)));
  }
  if (datum_is_the_symbol(list_at(ins, 0), ":if")) {
    return *datum_make_list_of(datum_copy(list_at(ins, 0)),
                               *offset_relocate(list_at(ins, 1), delta),
                             *offset_relocate(list_at(ins, 2), delta));
  }
  if (datum_is_the_symbol(list_at(ins, 0), ":put-prog")) {
    return *datum_make_list_of( 
        datum_copy(list_at(ins, 0)), *offset_relocate(list_at(ins, 1), delta),
        datum_copy(list_at(ins, 2)), *offset_relocate(list_at(ins, 3), delta));
  }
  if (datum_is_the_symbol(list_at(ins, 0), ":set-closures")) {
    return *datum_make_list_of(datum_copy(list_at(ins, 0)),
                             *offset_relocate(list_at(ins, 1), delta),
                             *offset_relocate(list_at(ins, 2), delta));
  }
  datum res = datum_copy(ins);
  if (list_length(&res) < 2) {
    fprintf(stderr, "malformed instruction: %s\n", datum_repr(&res));
    exit(EXIT_FAILURE);
  }
  datum *nxt = list_at(&res, list_length(&res) - 1);
  list_pop(&res);
  list_append(&res, offset_relocate(nxt, delta));
  return res;
}

EXPORT char *vec_relocate(vec *dst, size_t *p, datum *src) {
  if (*p + 1 != vec_length(dst)) {
    return "relocation can only be done to the slice end";
  }
  size_t delta = *p;
  // the "+ 1" comes because of the final :end
  for (int i = 0; i + 1 < list_length(src); ++i) {
    datum *ins = list_at(src, i);
    *vec_at(dst, *p) = instruction_relocate(ins, delta);
    *p = vec_append_new(dst);
  }
  return NULL;
}

LOCAL char *prog_build_dep(vec *sl, size_t *p, datum *dep_and_sym,
                           fdatum (*module_bytecode)(char *, datum *),
                           datum *settings, datum **compdata) {
  if (!datum_is_list(dep_and_sym) || datum_is_nil(dep_and_sym) ||
      !datum_is_bytestring(list_at(dep_and_sym, 0))) {
    return "req expects bytestrings";
  }
  datum *dep = list_at(dep_and_sym, 0);

  char varname[1024];
  get_varname(varname, dep_and_sym);
  datum idex = compdata_get_polyindex(
                                      *compdata, datum_make_symbol(varname));
  bool already_built = !datum_is_nil(&idex);
  if (already_built) {
    return NULL;
  }
  size_t run_dep_off = vec_append_new(sl);
  size_t run_dep_end = run_dep_off;

  fdatum stts = module_bytecode(dep->bytestring_value, settings);
  if (fdatum_is_panic(stts)) {
    return stts.panic_message;
  }
  char *er = vec_relocate(sl, &run_dep_end, &stts.ok_value);

  if (er != NULL) {
    return er;
  }
  datum *transitive_deps = extract_meta(*sl, run_dep_off);
  if (transitive_deps == NULL) {
    return "error: null extract_meta for reqs";
  }
  if (run_dep_end == 0) {
    return "error: run_dep_end == 0";
  }
  datum *syms = extract_meta(*sl, run_dep_end - 1);
  if (syms == NULL) {
    return "error: null extract_meta for exports";
  }
  char *err = prog_build_deps(sl, p, transitive_deps, module_bytecode, settings,
                              compdata);
  if (err != NULL) {
    return err;
  }
  prog_append_put_prog(sl, p, run_dep_off, 0, compdata);
  datum *fn_index = compdata_get_top_polyindex(*compdata);
  prog_put_deps(sl, p, transitive_deps, compdata);
  prog_append_call(sl, p, datum_make_list_of(*fn_index), false,
                   datum_make_symbol("plain"), list_length(transitive_deps),
                   list_length(syms), compdata);
  datum names = *datum_make_nil();
  get_varname(varname, datum_make_list_of(datum_copy(dep)));
  list_append(&names,
              datum_make_symbol(varname));
  for (int i = 0; i < list_length(syms); ++i) {
    datum *sym = list_at(syms, i);
    get_varname(varname, datum_make_list_of(datum_copy(dep), datum_copy(sym)));
    list_append(
                &names, datum_make_symbol(varname));
  }
  compdata_give_names(&names, compdata);
  return NULL;
}

LOCAL datum *extract_meta(vec sl, size_t run_main_off) {
  datum *first_main_instruction = vec_at(&sl, run_main_off);
  if (!datum_is_list(first_main_instruction) ||
      list_length(first_main_instruction) != 6 ||
      !datum_is_the_symbol(list_at(first_main_instruction, 0), ":yield") ||
      !datum_is_integer(list_at(first_main_instruction, 5))) {
    return NULL;
  }
  return list_at(first_main_instruction, 4);
}
