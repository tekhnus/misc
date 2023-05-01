#include <assert.h>
#include <extern.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#if INTERFACE
typedef struct extension extension;
struct extension {
  char *(*call)(extension *self, vec *sl, size_t *begin, datum *stmt,
                datum *compdata);
};
#endif

EXPORT char *prog_compile_and_relocate(vec *sl, size_t *p, datum *source,
                                       datum *compdata, extension *ext) {
  fdatum bytecode = prog_compile(source, compdata, ext);
  if (fdatum_is_panic(bytecode)) {
    return bytecode.panic_message;
  }
  char *res = vec_relocate(sl, p, &bytecode.ok_value);
  if (res != NULL) {
    return res;
  }
  return NULL;
}

EXPORT fdatum prog_compile(datum *source, datum *compdata, extension *ext) {
  vec sl = vec_make(16 * 1024);
  size_t p = vec_append_new(&sl);
  char *err = prog_append_statements(&sl, &p, source, compdata, ext, true);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  return fdatum_make_ok(vec_to_datum(&sl));
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

LOCAL char *prog_append_statements(vec *sl, size_t *off, datum *source,
                                   datum *compdata, extension *ext,
                                   bool skip_first_debug) {
  for (int i = 0; i < list_length(source); ++i) {
    datum *stmt = list_at(source, i);
    if (!(skip_first_debug && i == 0)) {
      prog_append_yield(sl, off,
                        datum_make_list_of(datum_make_symbol("debugger"),
                                           datum_make_symbol("statement"),
                                           datum_copy(stmt)),
                        0, 0, datum_make_nil(), compdata);
    }
    char *err = prog_append_statement(sl, off, stmt, compdata, ext);
    if (err != NULL && strcmp(err, "<not a statement>")) {
      return err;
    }
    if (err != NULL && !strcmp(err, "<not a statement>")) {
      return "expected a statement";
    }
    if (err != NULL) {
      return err;
    }
    if (compdata_validate(compdata) && compdata_has_value(compdata)) {
      fprintf(stderr, "warning: stack leak: %s\n", datum_repr(stmt));
      fprintf(stderr, "compdata: %s\n", datum_repr(compdata));
      return "stack leak";
    }
  }
  return NULL;
}

LOCAL char *prog_append_statement(vec *sl, size_t *begin, datum *stmt,
                                  datum *compdata, extension *ext) {
  datum *op = list_at(stmt, 0);
  datum op2 = datum_make_nil();
  if (list_length(stmt) > 1) {
    op2 = datum_copy(list_at(stmt, 1));
  }

  if (datum_is_the_symbol(op, "req")) {
    return prog_append_usages(sl, begin, stmt, compdata);
  }
  if (datum_is_the_symbol(op, "export")) {
    return prog_append_exports(sl, begin, stmt, compdata, ext);
  }
  if (datum_is_the_symbol(op, "if")) {
    if (list_length(stmt) != 4) {
      return "if should have three args";
    }
    char *err;
    err = prog_append_expression(sl, begin, list_at(stmt, 1), compdata, ext);
    if (err != NULL) {
      return err;
    }

    size_t true_end = vec_append_new(sl), false_end = vec_append_new(sl);

    *vec_at(sl, *begin) =
        datum_make_list_of(datum_make_symbol(":if"), datum_make_int(true_end),
                           datum_make_int(false_end));
    *begin = vec_append_new(sl);

    compdata_del(compdata);
    datum false_compdata_val = datum_copy(compdata);
    datum *false_compdata = &false_compdata_val;
    err =
        prog_append_expression(sl, &true_end, list_at(stmt, 2), compdata, ext);
    if (err != NULL) {
      return err;
    }
    err = prog_append_expression(sl, &false_end, list_at(stmt, 3),
                                 false_compdata, ext);
    if (err != NULL) {
      return err;
    }
    if (!datum_eq(compdata, false_compdata)) {
      compdata_put(compdata, datum_make_symbol("__different_if_branches"));
    }

    prog_join(sl, true_end, false_end, *begin);
    return NULL;
  }
  if (datum_is_the_symbol(op, "while")) {
    if (list_length(stmt) != 3) {
      return "while should have two args";
    }
    char *err;
    size_t pre_condition_check = *begin;
    datum pre_condition_check_compdata = datum_copy(compdata);
    err = prog_append_expression(sl, begin, list_at(stmt, 1), compdata, ext);
    if (err != NULL) {
      return err;
    }
    size_t condition_check = *begin;
    compdata_del(compdata);
    *begin = vec_append_new(sl);
    size_t loop_start = *begin;
    err = prog_append_expression(sl, begin, list_at(stmt, 2), compdata, ext);
    assert(datum_eq(&pre_condition_check_compdata, compdata));
    *vec_at(sl, *begin) = datum_make_list_of(
        datum_make_symbol(":nop"), datum_make_int(pre_condition_check));
    *begin = vec_append_new(sl);
    size_t loop_end = *begin;
    *vec_at(sl, condition_check) =
        datum_make_list_of(datum_make_symbol(":if"), datum_make_int(loop_start),
                           datum_make_int(loop_end));
    return NULL;
  }
  if (datum_is_the_symbol(op, "progn")) {
    datum parts = list_get_tail(stmt);
    char *err = prog_append_statements(sl, begin, &parts, compdata, ext, false);
    if (err != NULL) {
      return err;
    }
    return NULL;
  }
  if (datum_is_the_symbol(op, "list")) {
    for (int i = 1; i < list_length(stmt); ++i) {
      char *err =
          prog_append_expression(sl, begin, list_at(stmt, i), compdata, ext);
      if (err != NULL) {
        return err;
      }
    }
    prog_append_collect(sl, list_length(stmt) - 1, begin, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "quote")) {
    if (list_length(stmt) != 2) {
      return "quote should have a single arg";
    }
    prog_append_put_const(sl, begin, list_at(stmt, 1), compdata);
    return NULL;
  }
  if (datum_is_the_symbol(&op2, "=")) {
    if (list_length(stmt) != 3) {
      return "def should have two args";
    }
    datum *dst;
    datum *expr;
    dst = list_at(stmt, 0);
    expr = list_at(stmt, 2);
    char *err = prog_append_expression(sl, begin, expr, compdata, ext);
    if (err != NULL) {
      return err;
    }
    datum names;
    if (datum_is_list(dst)) {
      names = datum_copy(dst);
    } else {
      names = datum_make_list_of(datum_copy(dst));
    }
    store_values_to_variables(sl, begin, &names, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "defn")) {
    datum *name = list_at(stmt, 1);
    datum *args;
    datum *body;
    if (list_length(stmt) != 4) {
      return "wrong defn";
    }
    args = list_at(stmt, 2);
    body = list_at(stmt, 3);
    size_t s_off = vec_append_new(sl);
    datum routine_compdata = datum_copy(compdata);
    compdata_put(&routine_compdata, datum_copy(name));
    compdata_start_new_section(&routine_compdata);

    // this yield is for pre-call.
    datum target = datum_make_symbol("plain");
    datum met = datum_make_nil();
    prog_append_yield(sl, &s_off, target, 0, 0, met, &routine_compdata);

    char *err =
        prog_init_routine(sl, s_off, args, body, &routine_compdata, ext);
    if (err != NULL) {
      return err;
    }
    prog_append_put_prog(sl, begin, s_off, 2, compdata);
    datum name_singleton = datum_make_list_of(datum_copy(name));
    store_values_to_variables(sl, begin, &name_singleton, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "return")) {
    datum target = datum_make_symbol("plain");
    bool target_defined = false;
    size_t recieve_count = 0;
    int index = 1;
    while (index < list_length(stmt)) {
      datum *tag = list_at(stmt, index);
      if (!datum_is_list(tag) || list_length(tag) != 2 ||
          !datum_is_the_symbol(list_at(tag, 0), "at")) {
        break;
      }
      datum *content = list_at(tag, 1);
      if (datum_is_integer(content)) {
        recieve_count = content->integer_value;
        ++index;
      } else if (!target_defined) {
        target = datum_copy(content);
        target_defined = true;
        ++index;
      } else {
        return "unknown return tag";
      }
    }
    size_t argcnt = list_length(stmt) - index;
    for (; index < list_length(stmt); ++index) {
      datum *component = list_at(stmt, index);
      char *err = prog_append_expression(sl, begin, component, compdata, ext);
      if (err != NULL) {
        return err;
      }
    }
    prog_append_yield(sl, begin, target, argcnt, recieve_count,
                      datum_make_nil(), compdata);
    return NULL;
  }
  char *res = ext->call(ext, sl, begin, stmt, compdata);
  if (res == NULL) {
    return NULL;
  }
  if (strcmp(res, "<not an extension>")) {
    return res;
  }
  return "<not a statement>";
}

EXPORT char *prog_append_expression(vec *sl, size_t *begin, datum *stmt,
                                    datum *compdata, extension *ext) {
  if (datum_is_list(stmt) && !datum_is_nil(stmt)) {
    char *res = prog_append_statement(sl, begin, stmt, compdata, ext);
    if (res == NULL) {
      return NULL;
    }
    if (strcmp(res, "<not a statement>")) {
      return res;
    }
  }
  if (datum_is_constant(stmt)) {
    prog_append_put_const(sl, begin, stmt, compdata);
    return NULL;
  }
  if (datum_is_symbol(stmt)) {
    datum debug_compdata = datum_copy(compdata);
    prog_append_yield(sl, begin,
                      datum_make_list_of(datum_make_symbol("debugger"),
                                         datum_make_symbol("compdata"),
                                         debug_compdata),
                      0, 0, datum_make_nil(), compdata);
    prog_append_put_var(sl, begin, stmt, compdata);
    return NULL;
  }
  datum *fn = list_at(stmt, 0);
  bool hash = false;
  datum target = datum_make_symbol("plain");
  bool target_is_set = false;
  bool mut = false;
  bool pre = false;
  size_t ret_count = 1;
  for (; datum_is_list(fn) && list_length(fn) == 2 &&
         datum_is_symbol(list_at(fn, 0));
       fn = list_at(fn, 1)) {
    char *tag = list_at(fn, 0)->symbol_value;
    if (!strcmp(tag, "hash")) {
      hash = true;
    } else {
      break;
    }
  }
  datum fnsv;
  if (datum_is_list(fn) && !datum_is_nil(fn) &&
      datum_is_the_symbol(list_at(fn, 0), "polysym")) {
    fnsv = list_get_tail(fn);
  } else {
    fnsv = datum_make_list_of(datum_copy(fn));
  }
  datum *fns = &fnsv;
  int index = 1;
  while (index < list_length(stmt)) {
    datum *tag = list_at(stmt, index);
    if (!datum_is_list(tag) || list_length(tag) != 2 ||
        !datum_is_the_symbol(list_at(tag, 0), "at")) {
      break;
    }
    datum *content = list_at(tag, 1);
    if (datum_is_integer(content)) {
      ret_count = content->integer_value;
      ++index;
    } else if (datum_is_the_symbol(content, "mut")) {
      mut = true;
      ++index;
    } else if (datum_is_the_symbol(content, "pre")) {
      pre = true;
      ++index;
    } else if (!target_is_set) {
      target = datum_copy(content);
      target_is_set = true;
      ++index;
    } else {
      return "unknown tag";
    }
  }
  datum indices = datum_make_nil();
  int fn_index = 0;
  int chop = 0;
  if (fn_index < list_length(fns) &&
      datum_is_the_symbol(list_at(fns, fn_index), "..")) {
    ++fn_index;
    chop = 1;
  }
  datum shape = compdata_get_shape(compdata);
  if (fn_index < list_length(fns) &&
      datum_is_the_symbol(list_at(fns, fn_index), "empty-symbol")) {
    ++fn_index;
    chop = list_length(&shape);
  }
  size_t capture_size = 0;
  for (int j = 0; j + chop < list_length(&shape); ++j) {
    ++capture_size;
  }
  for (; fn_index < list_length(fns); ++fn_index) {
    datum *component = list_at(fns, fn_index);
    bool borrow = fn_index + 1 < list_length(fns) || mut;
    if (borrow) {
      if (!datum_is_symbol(component)) {
        return "expected an lvalue";
      }
      datum idx = compdata_get_polyindex(compdata, component);
      if (datum_is_nil(&idx)) {
        if (datum_is_nil(&idx)) {
          fprintf(stderr, "function not found: %s\n", datum_repr(component));
          return "function not found";
        }
      }
      list_append(&indices, idx);
    } else {
      char *err = prog_append_expression(sl, begin, component, compdata, ext);
      if (err != NULL) {
        return err;
      }
      datum idx = compdata_get_top_polyindex(compdata);
      list_append(&indices, idx);
    }
  }
  size_t arg_count = list_length(stmt) - index;
  for (; index < list_length(stmt); ++index) {
    datum *arg = list_at(stmt, index);
    if (hash) {
      prog_append_put_const(sl, begin, arg, compdata);
    } else {
      char *err = prog_append_expression(sl, begin, arg, compdata, ext);
      if (err != NULL) {
        return err;
      }
    }
  }
  prog_append_call(sl, begin, capture_size, indices, !mut, pre, target,
                   arg_count, ret_count, compdata);
  return NULL;
}

LOCAL char *prog_append_usages(vec *sl, size_t *begin, datum *spec,
                               datum *compdata) {
  fdatum res = prog_read_usages(spec);
  if (fdatum_is_panic(res)) {
    return res.panic_message;
  }
  datum re = res.ok_value;
  if (!datum_is_list(&re) || list_length(&re) != 2) {
    return "not gonna happen";
  }
  datum *vars = list_at(&re, 0);
  prog_append_recieve(sl, begin, vars, *list_at(&re, 1), compdata);
  return NULL;
}

LOCAL fdatum prog_read_usages(datum *spec) {
  if (!datum_is_list(spec) || list_length(spec) == 0 ||
      !datum_is_the_symbol(list_at(spec, 0), "req")) {
    return fdatum_make_panic("wrong usage spec");
  }
  int index = 1;
  datum vars = datum_make_nil();
  datum specs = datum_make_nil();
  for (; index < list_length(spec); ++index) {
    datum *item = list_at(spec, index);
    if (!datum_is_list(item) || list_length(item) < 2 ||
        list_length(item) > 3) {
      return fdatum_make_panic("wrong usage spec");
    }
    datum *item_var = list_at(item, 0);
    if (!datum_is_symbol(item_var)) {
      return fdatum_make_panic("wrong usage spec");
    }

    datum item_spec;
    if (list_length(item) == 2) {
      item_spec = datum_make_list_of(datum_copy(list_at(item, 1)));
    } else if (list_length(item) == 3) {
      item_spec = datum_make_list_of(datum_copy(list_at(item, 1)),
                                     datum_copy(list_at(item, 2)));
    } else {
      return fdatum_make_panic("wrong usage spec");
    }
    list_append(&vars, datum_copy(item_var));
    list_append(&specs, item_spec);
  }
  return fdatum_make_ok(datum_make_list_of(vars, specs));
}

LOCAL char *prog_append_exports(vec *sl, size_t *begin, datum *spec,
                                datum *compdata, extension *ext) {
  fdatum res = prog_read_exports(spec);
  if (fdatum_is_panic(res)) {
    return res.panic_message;
  }
  datum re = res.ok_value;
  if (!datum_is_list(&re) || list_length(&re) != 2) {
    return "not gonna happen";
  }
  datum *exprs = list_at(&re, 1);
  for (int i = 0; i < list_length(exprs); ++i) {
    datum *expr = list_at(exprs, i);
    prog_append_expression(sl, begin, expr, compdata, ext);
  }
  /* This nop is appended as a hack so that the yield becomes the last statement
   * on the slice. */
  prog_append_nop(sl, begin);
  prog_append_yield(sl, begin, datum_make_symbol("plain"),
                    list_length(list_at(&re, 0)), 0, *list_at(&re, 0),
                    compdata);
  return NULL;
}

EXPORT void prog_append_call(vec *sl, size_t *begin, size_t capture_size,
                             datum indices, bool pop_one, bool pre, datum type,
                             int arg_count, int return_count, datum *compdata) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) = datum_make_list_of(
      datum_make_symbol(":call"), datum_make_int(capture_size), indices,
      datum_make_int(pop_one), datum_make_int(pre), type,
      datum_make_int(arg_count), datum_make_int(return_count),
      datum_make_int(next));
  for (int i = 0; i < arg_count; ++i) {
    compdata_del(compdata);
  }
  if (pop_one) {
    compdata_del(compdata);
  }
  for (int i = 0; i < return_count; ++i) {
    compdata_put(compdata, datum_make_symbol(":anon"));
  }
  *begin = next;
}

EXPORT void prog_append_put_var(vec *sl, size_t *begin, datum *val,
                                datum *compdata) {
  size_t next = vec_append_new(sl);
  if (!datum_is_symbol(val)) {
    fprintf(stderr, "expected a symbol in put-var\n");
    exit(1);
  }
  datum polyindex = compdata_get_polyindex(compdata, val);
  if (datum_is_nil(&polyindex)) {
    fprintf(stderr, "undefined variable: %s\n", val->symbol_value);
    exit(1);
  }
  compdata_put(compdata, datum_make_symbol(":anon"));
  datum target_polyindex = compdata_get_top_polyindex(compdata);
  *vec_at(sl, *begin) =
      datum_make_list_of(datum_make_symbol(":put-var"), target_polyindex,
                         polyindex, datum_make_int(next));
  *begin = next;
}

LOCAL void prog_append_move(vec *sl, size_t *begin, datum *target,
                            datum *source, datum *compdata) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) =
      datum_make_list_of(datum_make_symbol(":move"), datum_copy(target),
                         datum_copy(source), datum_make_int(next));
  compdata_del(compdata);
  *begin = next;
}

EXPORT void prog_append_put_prog(vec *sl, size_t *begin, size_t val,
                                 int capture, datum *compdata) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) =
      datum_make_list_of(datum_make_symbol(":put-prog"), datum_make_int(val),
                         datum_make_int(capture), datum_make_int(next));
  *begin = next;
  compdata_put(compdata, datum_make_symbol(":anon"));
}

EXPORT void prog_append_yield(vec *sl, size_t *begin, datum type, size_t count,
                              size_t recieve_count, datum meta,
                              datum *compdata) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) = datum_make_list_of(
      datum_make_symbol(":yield"), type, datum_make_int(count),
      datum_make_int(recieve_count), meta, datum_make_int(next));
  *begin = next;
  for (size_t i = 0; i < count; ++i) {
    compdata_del(compdata);
  }
  for (size_t i = 0; i < recieve_count; ++i) {
    compdata_put(compdata, datum_make_symbol(":anon"));
  }
}

LOCAL void prog_append_recieve(vec *sl, size_t *begin, datum *args, datum meta,
                               datum *compdata) {
  prog_append_yield(sl, begin, datum_make_symbol("plain"), 0, list_length(args),
                    meta, compdata);
  store_values_to_variables(sl, begin, args, compdata);
}

LOCAL fdatum prog_read_exports(datum *spec) {
  if (!datum_is_list(spec) || list_length(spec) == 0 ||
      !datum_is_the_symbol(list_at(spec, 0), "export")) {
    return fdatum_make_panic("wrong export spec");
  }
  int index = 1;
  datum names = datum_make_nil();
  datum expressions = datum_make_nil();
  for (; index < list_length(spec); ++index) {
    datum *item = list_at(spec, index);
    if (!datum_is_list(item) || list_length(item) != 2) {
      return fdatum_make_panic("wrong export spec");
    }
    datum *item_name = list_at(item, 0);
    if (!datum_is_symbol(item_name)) {
      return fdatum_make_panic("wrong export spec");
    }
    datum *item_expression = list_at(item, 1);
    list_append(&names, datum_copy(item_name));
    list_append(&expressions, datum_copy(item_expression));
  }
  return fdatum_make_ok(datum_make_list_of(names, expressions));
}

LOCAL char *prog_init_routine(vec *sl, size_t s, datum *args, datum *stmt,
                              datum *routine_compdata, extension *ext) {
  if (args == NULL) {
    return "args can't be null";
  } else {
    prog_append_recieve(sl, &s, args, datum_make_nil(), routine_compdata);
  }
  return prog_append_expression(sl, &s, stmt, routine_compdata, ext);
}

EXPORT void prog_append_put_const(vec *sl, size_t *begin, datum *val,
                                  datum *compdata) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) = datum_make_list_of(
      datum_make_symbol(":put-const"), datum_copy(val), datum_make_int(next));
  *begin = next;
  compdata_put(compdata, datum_make_symbol(":anon"));
}

EXPORT void prog_append_nop(vec *sl, size_t *begin) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) =
      datum_make_list_of(datum_make_symbol(":nop"), datum_make_int(next));
  *begin = next;
}

LOCAL void prog_append_collect(vec *sl, size_t count, size_t *begin,
                               datum *compdata) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) =
      datum_make_list_of(datum_make_symbol(":collect"), datum_make_int(count),
                         datum_make_int(next));
  *begin = next;
  for (size_t i = 0; i < count; ++i) {
    compdata_del(compdata);
  }
  compdata_put(compdata, datum_make_symbol(":anon"));
}

LOCAL void prog_join(vec *sl, size_t a, size_t b, size_t e) {
  *vec_at(sl, a) =
      datum_make_list_of(datum_make_symbol(":nop"), datum_make_int(e));
  *vec_at(sl, b) =
      datum_make_list_of(datum_make_symbol(":nop"), datum_make_int(e));
}

EXPORT datum compdata_make() {
  datum nil = datum_make_nil();
  return datum_make_list_of(nil);
}

EXPORT datum *compdata_alloc_make() {
  // This one is for using from lisp.
  datum *res = malloc(sizeof(datum));
  *res = compdata_make();
  return res;
}

EXPORT bool compdata_has_value(datum *compdata) {
  assert(compdata_validate(compdata));
  datum *outer_frame = list_get_last(compdata);
  return !datum_is_nil(outer_frame) &&
         datum_is_the_symbol(list_get_last(outer_frame), ":anon");
}

LOCAL bool compdata_validate(datum *compdata) {
  datum *outer_frame = list_get_last(compdata);
  if (!datum_is_nil(outer_frame) &&
      datum_is_the_symbol(list_get_last(outer_frame),
                          "__different_if_branches")) {
    // fprintf(stderr, "compdata_del: if branches had different compdata\n");
    // fprintf(stderr, "%s\n", datum_repr(compdata));
    return false;
  }
  return true;
}

LOCAL void compdata_put(datum *compdata, datum var) {
  datum *last_frame = list_get_last(compdata);
  list_append(last_frame, var);
}

LOCAL void compdata_del(datum *compdata) {
  assert(compdata_validate(compdata));
  datum *last_frame = list_get_last(compdata);
  list_pop(last_frame);
}

EXPORT datum compdata_get_polyindex(datum *compdata, datum *var) {
  int frames = list_length(compdata);
  assert(frames > 0);
  for (int frame = frames - 1; frame >= 0; --frame) {
    datum *comp = list_at(compdata, frame);
    int idx = list_index_of(comp, var);
    if (idx != -1) {
      return datum_make_list_of(datum_make_int(frame), datum_make_int(idx));
    }
  }
  return datum_make_nil();
}

LOCAL void compdata_start_new_section(datum *compdata) {
  datum nil = datum_make_nil();
  list_append(compdata, nil);
}

EXPORT datum compdata_get_top_polyindex(datum *compdata) {
  size_t frames = list_length(compdata);
  size_t indices = list_length(list_get_last(compdata));
  assert(frames > 0 && indices > 0);
  return datum_make_list_of(datum_make_int(frames - 1),
                            datum_make_int(indices - 1));
}

EXPORT datum compdata_get_shape(datum *compdata) {
  datum res = datum_make_nil();
  for (int i = 0; i < list_length(compdata); ++i) {
    datum ii = datum_make_int(list_length(list_at(compdata, i)));
    list_append(&res, ii);
  }
  return res;
}

EXPORT void store_values_to_variables(vec *sl, size_t *begin, datum *var,
                                      datum *compdata) {
  if (!datum_is_list(var)) {
    fprintf(stderr, "error: compdata_give_names\n");
    exit(EXIT_FAILURE);
  }
  assert(sl);
  assert(begin);
  bool put = false;
  bool set = false;
  for (int i = 0; i < list_length(var); ++i) {
    datum polyindex = compdata_get_polyindex(compdata, list_at(var, i));
    if (datum_is_nil(&polyindex)) {
      put = true;
    } else {
      set = true;
    }
  }
  if (put && set) {
    fprintf(stderr, "mixed assignment: %s\n", datum_repr(var));
    exit(EXIT_FAILURE);
  }
  if (put) {
    for (int i = 0; i < list_length(var); ++i) {
      compdata_del(compdata);
    }
    for (int i = 0; i < list_length(var); ++i) {
      compdata_put(compdata, datum_copy(list_at(var, i)));
    }
  }
  if (set) {
    for (int i = 0; i < list_length(var); ++i) {
      int idx = list_length(var) - i - 1;
      datum target = compdata_get_polyindex(compdata, list_at(var, idx));
      assert(!datum_is_nil(&target));
      datum source = compdata_get_top_polyindex(compdata);
      prog_append_move(sl, begin, &target, &source, compdata);
    }
  }
}

LOCAL datum instruction_relocate(datum *ins, size_t delta) {
  if (datum_is_the_symbol(list_at(ins, 0), ":end")) {
    return datum_make_list_of(datum_copy(list_at(ins, 0)));
  }
  if (datum_is_the_symbol(list_at(ins, 0), ":if")) {
    return datum_make_list_of(datum_copy(list_at(ins, 0)),
                              offset_relocate(list_at(ins, 1), delta),
                              offset_relocate(list_at(ins, 2), delta));
  }
  if (datum_is_the_symbol(list_at(ins, 0), ":put-prog")) {
    return datum_make_list_of(
        datum_copy(list_at(ins, 0)), offset_relocate(list_at(ins, 1), delta),
        datum_copy(list_at(ins, 2)), offset_relocate(list_at(ins, 3), delta));
  }
  if (datum_is_the_symbol(list_at(ins, 0), ":set-closures")) {
    return datum_make_list_of(datum_copy(list_at(ins, 0)),
                              offset_relocate(list_at(ins, 1), delta),
                              offset_relocate(list_at(ins, 2), delta));
  }
  datum res = datum_copy(ins);
  if (list_length(&res) < 2) {
    fprintf(stderr, "malformed instruction: %s\n", datum_repr(&res));
    exit(EXIT_FAILURE);
  }
  datum *nxt = list_at(&res, list_length(&res) - 1);
  list_pop(&res);
  datum dd = offset_relocate(nxt, delta);
  list_append(&res, dd);
  return res;
}

LOCAL datum offset_relocate(datum *ins, size_t delta) {
  if (!datum_is_integer(ins)) {
    fprintf(stderr, "error: offset_relocate");
    exit(EXIT_FAILURE);
  }
  return datum_make_int(ins->integer_value + delta);
}
