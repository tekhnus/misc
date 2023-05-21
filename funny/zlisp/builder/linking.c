#include <assert.h>
#include <extern.h>
#include <linking.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

EXPORT size_t prog_build_init(vec *sl, datum *compdata,
                              datum *builder_compdata) {
  size_t e = 0;
  datum nil = datum_make_nil();
  prog_append_yield(sl, &e, datum_make_symbol("halt"), 0, 0, nil,
                    builder_compdata);
  size_t bdr_put_prog = prog_append_something(sl, &e); // filled below
  size_t ep_start = e;
  prog_append_yield(sl, &e, datum_make_symbol("plain"), 0, 0, nil, compdata);
  size_t jm = prog_append_something(sl, &e); // filled below
  assert(bdr_put_prog + 1 == ep_start);
  *vec_at(sl, bdr_put_prog) = prog_get_put_prog(e - bdr_put_prog, 0);
  compdata_put(builder_compdata, datum_make_symbol(":anon"));
  prog_append_call(
      sl, &e, 0,
      datum_make_list_of(compdata_get_top_polyindex(builder_compdata)), false,
      datum_make_symbol("plain"), 0, 0, builder_compdata);
  size_t bdr =
      prog_append_something(sl, &e); // this is first builder instruction.
  // filled by prog_build.
  *vec_at(sl, jm) = prog_get_jmp(e - jm);
  return bdr;
}

EXPORT char *prog_link_deps(vec *sl, datum *builder_compdata, datum *input_meta,
                            fdatum (*module_bytecode)(char *, datum *,
                                                      extension *),
                            datum *settings, extension *ext) {
  size_t bdr_p_val = vec_length(sl) - 1;
  size_t *bdr_p = &bdr_p_val;
  if (input_meta == NULL) {
    return NULL;
  }
  datum s = datum_make_list_of(datum_make_symbol("__main__"));
  store_values_to_variables(sl, bdr_p, &s, builder_compdata);
  char *err = prog_build_deps(sl, bdr_p, input_meta, module_bytecode, settings,
                              builder_compdata, ext);
  if (err != NULL) {
    return err;
  }
  datum v = datum_make_symbol("__main__");
  prog_append_copy(sl, bdr_p, &v, builder_compdata);
  datum fn_index = compdata_get_top_polyindex(builder_compdata);
  prog_put_deps(sl, bdr_p, input_meta, builder_compdata);
  prog_append_call(sl, bdr_p, 0, datum_make_list_of(fn_index), false,
                   datum_make_symbol("plain"), list_length(input_meta), 0,
                   builder_compdata);
  *bdr_p = vec_length(sl) - 1;
  return NULL;
}

LOCAL char *prog_build_deps(vec *sl, size_t *p, datum *deps,
                            fdatum (*module_bytecode)(char *, datum *,
                                                      extension *),
                            datum *settings, datum *compdata, extension *ext) {
  for (int i = 0; i < list_length(deps); ++i) {
    datum *dep = list_at(deps, i);
    char *err =
        prog_build_dep(sl, p, dep, module_bytecode, settings, compdata, ext);
    if (err != NULL) {
      return err;
    }
  }
  return NULL;
}

LOCAL void prog_put_deps(vec *sl, size_t *p, datum *deps, datum *compdata) {
  char varname[1024];
  for (int i = 0; i < list_length(deps); ++i) {
    datum *dep = list_at(deps, i);
    get_varname(varname, dep);
    datum vn = datum_make_symbol(varname);
    prog_append_copy(sl, p, &vn, compdata);
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

LOCAL char *prog_build_dep(vec *sl, size_t *p, datum *dep_and_sym,
                           fdatum (*module_bytecode)(char *, datum *,
                                                     extension *),
                           datum *settings, datum *compdata, extension *ext) {
  if (!datum_is_list(dep_and_sym) || datum_is_nil(dep_and_sym) ||
      !datum_is_bytestring(list_at(dep_and_sym, 0))) {
    return "req expects bytestrings";
  }
  datum *dep = list_at(dep_and_sym, 0);

  char varname[1024];
  get_varname(varname, dep_and_sym);
  datum vn = datum_make_symbol(varname);
  datum idex = compdata_get_polyindex(compdata, &vn);
  bool already_built = !datum_is_nil(&idex);
  if (already_built) {
    return NULL;
  }
  fdatum stts = module_bytecode(dep->bytestring_value, settings, ext);
  if (fdatum_is_panic(stts)) {
    return stts.panic_message;
  }
  vec *bc = list_to_vec(&stts.ok_value);
  datum *transitive_deps = extract_meta(*bc, 0);
  if (transitive_deps == NULL) {
    return "error: null extract_meta for reqs";
  }
  assert(vec_length(bc) >= 2);
  size_t last_instruction = vec_length(bc) - 2;
  datum *syms = extract_meta(*bc, last_instruction);
  if (syms == NULL) {
    return "error: null extract_meta for exports";
  }
  char *err = prog_build_deps(sl, p, transitive_deps, module_bytecode, settings,
                              compdata, ext);
  if (err != NULL) {
    return err;
  }
  size_t put_prog_off = prog_append_something(sl, p); // filled below
  size_t prog_off = *p;
  prog_append_bytecode(sl, p, bc);
  assert(put_prog_off + 1 == prog_off);
  *p = vec_length(sl) - 1;
  *vec_at(sl, put_prog_off) = prog_get_put_prog(*p - put_prog_off, 0);
  compdata_put(compdata, datum_make_symbol(":anon"));
  datum fn_index = compdata_get_top_polyindex(compdata);
  prog_put_deps(sl, p, transitive_deps, compdata);
  prog_append_call(sl, p, 0, datum_make_list_of(fn_index), false,
                   datum_make_symbol("plain"), list_length(transitive_deps),
                   list_length(syms), compdata);
  datum names = datum_make_nil();
  datum dep_singleton = datum_make_list_of(datum_copy(dep));
  get_varname(varname, &dep_singleton);
  vn = datum_make_symbol(varname);
  list_append(&names, vn);
  for (int i = 0; i < list_length(syms); ++i) {
    datum *sym = list_at(syms, i);
    datum depsym = datum_make_list_of(datum_copy(dep), datum_copy(sym));
    get_varname(varname, &depsym);
    vn = datum_make_symbol(varname);
    list_append(&names, vn);
  }
  store_values_to_variables(sl, p, &names, compdata);
  return NULL;
}

EXPORT datum *extract_meta(vec sl, size_t run_main_off) {
  datum *first_main_instruction = vec_at(&sl, run_main_off);
  if (!datum_is_list(first_main_instruction) ||
      list_length(first_main_instruction) == 0 ||
      !datum_is_the_symbol(list_at(first_main_instruction, 0), ":yield")) {
    return NULL;
  }
  return list_at(first_main_instruction, 4);
}
