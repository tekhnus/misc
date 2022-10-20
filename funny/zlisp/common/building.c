#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <extern.h>

LOCAL datum *extract_meta(prog_slice sl, size_t run_main_off) {
  datum *first_main_instruction = prog_slice_datum_at(sl, run_main_off);
  if (!datum_is_list(first_main_instruction)
      || list_length(first_main_instruction) != 6
      || !datum_is_the_symbol(list_at(first_main_instruction, 0), ":yield")
      || !datum_is_integer(list_at(first_main_instruction, 5))) {
    return NULL;
  }
  return list_at(first_main_instruction, 4);
}

EXPORT size_t prog_build_init(prog_slice *sl, size_t *ep, size_t *bdr_p, datum **compdata, datum **builder_compdata) {
  prog_append_put_prog(sl, bdr_p, *ep, 0, builder_compdata);
  prog_append_call(sl, bdr_p, false, 0, 1, builder_compdata);
  prog_append_yield(sl, ep, false, 0, 0, datum_make_nil(), compdata);
  return 42;
}

EXPORT char *prog_build_2(prog_slice *sl, size_t *ep, size_t *bdr_p, datum *source, fdatum (*module_source)(char *), datum **compdata, datum **builder_compdata) {
  size_t original_ep = *ep;
  char *res = prog_append_statements(sl, ep, source, compdata, datum_make_list_1(datum_make_symbol("main")));
  if (res != NULL) {
    return res;
  }
  datum *input_meta = extract_meta(*sl, original_ep);
  if (input_meta != NULL) {
    prog_append_pop(sl, bdr_p, datum_make_list_1(datum_make_symbol("__main__")), builder_compdata);
    char *err = prog_build_deps(sl, bdr_p, input_meta, module_source, builder_compdata);
    if (err != NULL) {
      return err;
    }
    prog_append_put_var(sl, bdr_p, datum_make_symbol("__main__"), builder_compdata);
    prog_put_deps(sl, bdr_p, input_meta, builder_compdata);
    prog_append_call(sl, bdr_p, false, list_length(input_meta), 1, builder_compdata);
  }
  return NULL;
}

LOCAL char *prog_build_deps(prog_slice *sl, size_t *p, datum *deps, fdatum (*module_source)(char *), datum **compdata) {
  for (datum *rest_deps = deps; !datum_is_nil(rest_deps); rest_deps=rest_deps->list_tail) {
    datum *dep = rest_deps->list_head;
    char *err = prog_build_dep(sl, p, dep, module_source, compdata);
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

LOCAL datum *offset_relocate(datum *ins, size_t delta) {
  if (!datum_is_integer(ins)) {
    fprintf(stderr, "error: offset_relocate");
    exit(EXIT_FAILURE);
  }
  return datum_make_int(ins->integer_value + delta);
}

LOCAL datum *instruction_relocate(datum *ins, size_t delta) {
  if (datum_is_the_symbol(list_at(ins, 0), ":end")) {
    return datum_make_list_1(list_at(ins, 0));
  }
  if (datum_is_the_symbol(list_at(ins, 0), ":if")) {
    return datum_make_list_3(list_at(ins, 0), offset_relocate(list_at(ins, 1), delta), offset_relocate(list_at(ins, 2), delta));
  }
  if (datum_is_the_symbol(list_at(ins, 0), ":put-prog")) {
    return datum_make_list_4(list_at(ins, 0), offset_relocate(list_at(ins, 1), delta), list_at(ins, 2), offset_relocate(list_at(ins, 3), delta));
  }
  if (datum_is_the_symbol(list_at(ins, 0), ":set-closures")) {
    return datum_make_list_4(list_at(ins, 0), offset_relocate(list_at(ins, 1), delta), list_at(ins, 2), offset_relocate(list_at(ins, 3), delta));
  }
  datum *res = ins;
  if (list_length(res) < 2) {
    fprintf(stderr, "malformed instruction: %s\n", datum_repr(res));
    exit(EXIT_FAILURE);
  }
  datum *nxt = list_at(res, list_length(res) - 1);
  res = list_append(list_chop_last(res), offset_relocate(nxt, delta));
  return res;
}

LOCAL char *prog_slice_relocate(prog_slice *dst, size_t *p, datum *src) {
  size_t delta = *p;
  // the ">1" comes because of the final :end
  for (datum *rest = src; list_length(rest) > 1; rest = rest->list_tail) {
    datum *ins = rest->list_head;
    *prog_slice_datum_at(*dst, *p) = *instruction_relocate(ins, delta);
    *p = prog_slice_append_new(dst);
  }
  return NULL;
}

LOCAL char *prog_build_dep(prog_slice *sl, size_t *p, datum *dep_and_sym, fdatum (*module_source)(char *), datum **compdata) {
  if (!datum_is_list(dep_and_sym) || datum_is_nil(dep_and_sym) || !datum_is_bytestring(dep_and_sym->list_head)){
    return "req expects bytestrings";
  }
  datum *dep = dep_and_sym->list_head;

  bool already_built = list_index_of(*compdata, datum_make_symbol(get_varname(dep_and_sym))) >= 0;
  if (already_built) {
    return NULL;
  }
  size_t run_dep_off = prog_slice_append_new(sl);
  size_t run_dep_end = run_dep_off;

  fdatum stts = module_source(dep->bytestring_value);
  if (fdatum_is_panic(stts)) {
    return stts.panic_message;
  }
  char *er = prog_slice_relocate(sl, &run_dep_end, stts.ok_value);

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
  char *err = prog_build_deps(sl, p, transitive_deps, module_source, compdata);
  if (err != NULL) {
    return err;
  }
  prog_append_put_prog(sl, p, run_dep_off, 0, compdata);
  prog_put_deps(sl, p, transitive_deps, compdata);
  prog_append_call(sl, p, false, list_length(transitive_deps), 1 + list_length(syms), compdata);
  prog_append_pop(sl, p, datum_make_symbol(":void"), compdata);
  datum *names = datum_make_nil();
  for (datum *rest_syms = syms; !datum_is_nil(rest_syms); rest_syms=rest_syms->list_tail) {
    datum *sym = rest_syms->list_head;
    names = list_append(names, datum_make_symbol(get_varname(datum_make_list_2(dep, sym))));
  }
  prog_append_pop(sl, p, names, compdata);
  return NULL;
}
