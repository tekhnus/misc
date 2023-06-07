#include <assert.h>
#include <extern.h>
#include <linking.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

EXPORT size_t prog_build_init(vec *sl, datum *compdata,
                              datum *builder_compdata) {
  extension ext = null_extension_make();
  datum nil = datum_make_nil();
  prog_append_yield(sl, datum_make_symbol("halt"),
                    compdata_get_next_polyindex(builder_compdata), 0, 0, nil,
                    builder_compdata);
  datum stmt0 = datum_make_list_of(datum_make_symbol("__main__"), datum_make_symbol(":="), datum_make_int(42));
  char *res = prog_compile_and_relocate(sl, &stmt0, builder_compdata, &ext);
  if (res != NULL) {
    fprintf(stderr, "%s\n", res);
    exit(EXIT_FAILURE);
  }
  size_t bdr_put_prog = prog_get_next_index(sl);
  ptrdiff_t *bdr_put_prog_ = prog_append_put_prog(sl, 0, builder_compdata);
  prog_append_yield(sl, datum_make_symbol("plain"),
                    compdata_get_next_polyindex(compdata), 0, 0, nil, compdata);
  size_t jm = prog_get_next_index(sl);
  ptrdiff_t *jm_ = prog_append_jmp(sl); // filled below
  // datum fai = compdata_get_next_polyindex(builder_compdata);
  *bdr_put_prog_ = prog_get_next_index(sl) - bdr_put_prog;
  datum xx = datum_make_list_of(datum_make_symbol("__start__"));
  compdata_give_names(builder_compdata, &xx);

  vec call_sexp = vec_make_of(datum_make_list_of(
      datum_make_symbol("polysym"), datum_make_symbol("empty-symbol"),
      datum_make_symbol("__start__")));
  vec_append(&call_sexp, datum_make_symbol("at"));
  vec_append(&call_sexp, datum_make_list_of(datum_make_symbol("mut")));
  vec_append(&call_sexp, datum_make_symbol("at"));
  vec_append(&call_sexp, datum_make_list_of(datum_make_int(0)));
  datum call_stmt = datum_make_list_of(datum_make_list_of(
      datum_make_symbol("call"), datum_make_list(call_sexp)));
  res = prog_compile_and_relocate(sl, &call_stmt, builder_compdata, &ext);
  if (res != NULL) {
    fprintf(stderr, "%s\n", res);
    exit(EXIT_FAILURE);
  }

  size_t bdr = prog_get_next_index(sl);
  prog_append_jmp(sl); // this is first builder instruction.
  // filled by prog_build.
  *jm_ = prog_get_next_index(sl) - jm;
  return bdr;
}

EXPORT char *prog_link_deps(vec *sl, datum *builder_compdata, datum *input_meta,
                            fdatum (*module_bytecode)(char *, datum *,
                                                      extension *),
                            datum *settings, extension *ext) {
  if (input_meta == NULL) {
    return NULL;
  }
  datum s = datum_make_list_of(datum_make_symbol("__main__"));
  move_values_to_variables(sl, &s, builder_compdata);
  char *err = prog_build_deps(sl, input_meta, module_bytecode, settings,
                              builder_compdata, ext);
  if (err != NULL) {
    return err;
  }

  vec call_sexp = vec_make_of(datum_make_list_of(
      datum_make_symbol("polysym"), datum_make_symbol("empty-symbol"),
      datum_make_symbol("__main__")));
  char varname[1024];
  for (int i = 0; i < list_length(input_meta); ++i) {
    datum *dep = list_at(input_meta, i);
    get_varname(varname, dep);
    datum vn = datum_make_symbol(varname);
    vec_append(&call_sexp, vn);
  }
  datum call_stmt = datum_make_list_of(datum_make_list_of(
      datum_make_symbol("call"), datum_make_list(call_sexp)));
  char *res = prog_compile_and_relocate(sl, &call_stmt, builder_compdata, ext);
  return res;
}

LOCAL char *prog_build_deps(vec *sl, datum *deps,
                            fdatum (*module_bytecode)(char *, datum *,
                                                      extension *),
                            datum *settings, datum *compdata, extension *ext) {
  for (int i = 0; i < list_length(deps); ++i) {
    datum *dep = list_at(deps, i);
    char *err =
        prog_build_dep(sl, dep, module_bytecode, settings, compdata, ext);
    if (err != NULL) {
      return err;
    }
  }
  return NULL;
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

LOCAL char *prog_build_dep(vec *sl, datum *dep_and_sym,
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
  vec module_sl = list_to_vec(&stts.ok_value);
  datum *transitive_deps = extract_meta(module_sl, 0);
  if (transitive_deps == NULL) {
    return "error: null extract_meta for reqs";
  }
  assert(prog_get_next_index(&module_sl) >= 1);
  size_t last_instruction = prog_get_next_index(&module_sl) - 1;
  datum *syms = extract_meta(module_sl, last_instruction);
  if (syms == NULL) {
    return "error: null extract_meta for exports";
  }
  char *err = prog_build_deps(sl, transitive_deps, module_bytecode, settings,
                              compdata, ext);
  if (err != NULL) {
    return err;
  }
  size_t ppo = prog_get_next_index(sl);
  ptrdiff_t *put_prog_off_ = prog_append_put_prog(sl, 0, compdata);
  prog_append_bytecode(sl, &module_sl);
  *put_prog_off_ = prog_get_next_index(sl) - ppo;
  datum dep_singleton = datum_make_list_of(datum_copy(dep));
  get_varname(varname, &dep_singleton);
  vn = datum_make_symbol(varname);
  datum xxx = datum_make_list_of(vn);
  compdata_give_names(compdata, &xxx);

  vec call_sexp = vec_make_of(datum_make_list_of(
      datum_make_symbol("polysym"), datum_make_symbol("empty-symbol"), vn));
  vec_append(&call_sexp, datum_make_symbol("at"));
  vec_append(&call_sexp, datum_make_list_of(datum_make_symbol("mut")));
  vec_append(&call_sexp, datum_make_symbol("at"));
  vec_append(&call_sexp, datum_make_list_of(datum_make_int(list_length(syms))));
  for (int i = 0; i < list_length(transitive_deps); ++i) {
    datum *dep = list_at(transitive_deps, i);
    get_varname(varname, dep);
    datum vn = datum_make_symbol(varname);
    vec_append(&call_sexp, vn);
  }
  datum call_stmt = datum_make_list_of(datum_make_list_of(
      datum_make_symbol("call"), datum_make_list(call_sexp)));
  char *res = prog_compile_and_relocate(sl, &call_stmt, compdata, ext);
  if (res != NULL) {
    return res;
  }

  vec names = vec_make(0);
  for (int i = 0; i < list_length(syms); ++i) {
    datum *sym = list_at(syms, i);
    datum depsym = datum_make_list_of(datum_copy(dep), datum_copy(sym));
    get_varname(varname, &depsym);
    vn = datum_make_symbol(varname);
    vec_append(&names, vn);
  }
  datum names_ = datum_make_list(names);
  compdata_give_names(compdata, &names_);
  return NULL;
}

EXPORT datum *extract_meta(vec sl, size_t run_main_off) {
  datum *first_main_instruction = vec_at(&sl, run_main_off);
  if (!datum_is_list(first_main_instruction) ||
      list_length(first_main_instruction) == 0 ||
      !datum_is_the_symbol(list_at(first_main_instruction, 0), ":yield")) {
    return NULL;
  }
  return list_at(first_main_instruction, 5);
}
