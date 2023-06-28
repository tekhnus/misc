#include <assert.h>
#include <zlisp/common.h>
#include <linking.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

EXPORT size_t prog_build_init(vec *sl, datum *compdata,
                              datum *builder_compdata, context *ctxt) {
  extension ext = null_extension_make();
  vec return_expr = vec_make_of(
      datum_make_nil(), datum_make_symbol(":="), datum_make_symbol("return"),
      datum_make_symbol("at"), datum_make_list_of(datum_make_symbol("halt")),
      datum_make_symbol("at"), datum_make_list_of(datum_make_int(0)),
      datum_make_symbol("flat"), datum_make_list_of(datum_make_nil()));
  datum ret_exp = datum_make_list(return_expr);
  prog_compile(sl, &ret_exp, builder_compdata, &ext, ctxt);
  if (ctxt->aborted) {
    return 0;
  }
  size_t bdr_put_prog = prog_get_next_index(sl);
  ptrdiff_t *bdr_put_prog_ =
      prog_define_routine(sl, datum_make_symbol("__main__"), builder_compdata, ctxt);
  if (ctxt->aborted) {
    return 0;
  }
  return_expr = vec_make_of(
      datum_make_nil(), datum_make_symbol(":="), datum_make_symbol("return"),
      datum_make_symbol("at"), datum_make_list_of(datum_make_int(0)),
      datum_make_symbol("flat"), datum_make_list_of(datum_make_nil()));
  ret_exp = datum_make_list(return_expr);
  prog_compile(sl, &ret_exp, compdata, &ext, ctxt);
  if (ctxt->aborted) {
    return 0;
  }
  size_t jm = prog_get_next_index(sl);
  ptrdiff_t *jm_ = prog_append_jmp(sl); // filled below
  *bdr_put_prog_ = prog_get_next_index(sl) - bdr_put_prog;

  vec call_sexp = vec_make_of(datum_make_list_of(
      datum_make_symbol("polysym"), datum_make_symbol("empty-symbol"),
      datum_make_symbol("__main__")));
  vec_append(&call_sexp, datum_make_symbol("at"));
  vec_append(&call_sexp, datum_make_list_of(datum_make_symbol("mut")));
  vec_append(&call_sexp, datum_make_symbol("at"));
  vec_append(&call_sexp, datum_make_list_of(datum_make_int(0)));
  datum call_stmt = datum_make_list_of(datum_make_list_of(
      datum_make_symbol("call"), datum_make_list(call_sexp)));
  prog_compile(sl, &call_stmt, builder_compdata, &ext, ctxt);
  if (ctxt->aborted) {
    return 0;
  }

  size_t bdr = prog_get_next_index(sl);
  prog_append_jmp(sl); // this is first builder instruction.
  // filled by prog__build.
  *jm_ = prog_get_next_index(sl) - jm;
  return bdr;
}

EXPORT void prog_link_deps(vec *sl, datum *builder_compdata, datum *input_meta,
                            datum (*module_bytecode)(char *, datum *,
                                                      extension *, context *ctxt),
                            datum *settings, extension *ext, context *ctxt) {
  if (input_meta == NULL) {
    return;
  }
  prog_build_deps(sl, input_meta, module_bytecode, settings,
                              builder_compdata, ext, ctxt);
  if (ctxt->aborted) {
    return;
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
  prog_compile(sl, &call_stmt, builder_compdata, ext, ctxt);
  if (ctxt->aborted) {
    return;
  }
  return;
}

LOCAL void prog_build_deps(vec *sl, datum *deps,
                            datum (*module_bytecode)(char *, datum *,
                                                      extension *, context *ctxt),
                            datum *settings, datum *compdata, extension *ext, context *ctxt) {
  for (int i = 0; i < list_length(deps); ++i) {
    datum *dep = list_at(deps, i);
    prog_build_dep(sl, dep, module_bytecode, settings, compdata, ext, ctxt);
    if (ctxt->aborted) {
      return;
    }
  }
  return;
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

LOCAL void prog_build_dep(vec *sl, datum *dep_and_sym,
                           datum (*module_bytecode)(char *, datum *,
                                                     extension *, context *ctxt),
                           datum *settings, datum *compdata, extension *ext, context *ctxt) {
  if (!datum_is_list(dep_and_sym) || datum_is_nil(dep_and_sym) ||
      !datum_is_bytestring(list_at(dep_and_sym, 0))) {
    abortf(ctxt, "req expects bytestrings");
    return;
  }
  datum *dep = list_at(dep_and_sym, 0);

  char varname[1024];
  get_varname(varname, dep_and_sym);
  datum vn = datum_make_symbol(varname);
  datum idex = compdata_get_polyindex(compdata, &vn);
  bool already_built = !datum_is_nil(&idex);
  if (already_built) {
    return;
  }
  datum stts = module_bytecode(dep->bytestring_value, settings, ext, ctxt);
  if (ctxt->aborted) {
    return;
  }
  vec module_sl = list_to_vec(&stts);
  datum *transitive_deps = extract_meta(module_sl, 0);
  if (transitive_deps == NULL) {
    abortf(ctxt, "error: null extract_meta for reqs");
    return;
  }
  assert(prog_get_next_index(&module_sl) >= 1);
  size_t last_instruction = prog_get_next_index(&module_sl) - 1;
  datum *syms = extract_meta(module_sl, last_instruction);
  if (syms == NULL) {
    abortf(ctxt, "error: null extract_meta for exports");
    return;
  }
  prog_build_deps(sl, transitive_deps, module_bytecode, settings,
                              compdata, ext, ctxt);
  if (ctxt->aborted) {
    abortf(ctxt, "aborted");
    return;
  } 
  size_t ppo = prog_get_next_index(sl);
  datum dep_singleton = datum_make_list_of(datum_copy(dep));
  get_varname(varname, &dep_singleton);
  vn = datum_make_symbol(varname);
  ptrdiff_t *put_prog_off_ = prog_define_routine(sl, vn, compdata, ctxt);
  if (ctxt->aborted) {
    return;
  }

  prog_append_bytecode(sl, &module_sl);
  *put_prog_off_ = prog_get_next_index(sl) - ppo;

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
  vec names = vec_make(0);
  for (int i = 0; i < list_length(syms); ++i) {
    datum *sym = list_at(syms, i);
    datum depsym = datum_make_list_of(datum_copy(dep), datum_copy(sym));
    get_varname(varname, &depsym);
    vn = datum_make_symbol(varname);
    vec_append(&names, vn);
  }
  datum names_ = datum_make_list(names);
  datum call_stmt =
      datum_make_list_of(names_, datum_make_symbol(":="),
                         datum_make_list_of(datum_make_symbol("call"),
                                            datum_make_list(call_sexp)));
  prog_compile(sl, &call_stmt, compdata, ext, ctxt);
  if (ctxt->aborted) {
    abortf(ctxt, "failure!");
    return;
  }
  return;
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
