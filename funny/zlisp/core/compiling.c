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
  char *err = prog_append_expressions(&sl, &p, source, compdata, ext, true);
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

EXPORT char *prog_append_expressions(vec *sl, size_t *off, datum *source_,
                                     datum *compdata, extension *ext,
                                     bool skip_first_debug) {
  skip_first_debug = true; // a temporary hack to support req inside {}
  datum source = prog_unflatten(source_);
  for (int i = 0; i < list_length(&source); ++i) {
    datum *stmt = list_at(&source, i);
    if (!(skip_first_debug && i == 0)) {
      prog_append_yield(sl, off,
                        datum_make_list_of(datum_make_symbol("debugger"),
                                           datum_make_symbol("statement"),
                                           datum_copy(stmt)),
                        0, 0, datum_make_nil(), compdata);
    }
    char *err = prog_append_expression(sl, off, stmt, compdata, ext);
    if (err != NULL) {
      return err;
    }
    if (false && compdata_validate(compdata) && compdata_has_value(compdata)) {
      // stack leak check disabled for extensions
      fprintf(stderr, "warning: stack leak: %s\n", datum_repr(stmt));
      fprintf(stderr, "compdata: %s\n", datum_repr(compdata));
      return "stack leak";
    }
  }
  return NULL;
}

LOCAL datum prog_unflatten(datum *source) {
  datum res = datum_make_nil();
  assert(datum_is_list(source));
  int i = 0;
  for (;;) {
    if (i >= list_length(source)) {
      break;
    }
    datum *cur = list_at(source, i);
    if (datum_is_the_symbol(cur, "if")) {
      i += 4;
      list_append(&res, list_copy(source, i - 4, i));
      continue;
    }
    if (datum_is_the_symbol(cur, "while")) {
      i += 3;
      list_append(&res, list_copy(source, i - 3, i));
      continue;
    }
    if (i + 1 < list_length(source) &&
        datum_is_the_symbol(list_at(source, i + 1), "=")) {
      i += 3;
      list_append(&res, list_copy(source, i - 3, i));
      continue;
    }
    if (datum_is_the_symbol(cur, "defn")) {
      i += 4;
      list_append(&res, list_copy(source, i - 4, i));
      continue;
    }
    if (datum_is_the_symbol(cur, "req")) {
      i += 2;
      list_append(&res, list_copy(source, i - 2, i));
      continue;
    }
    if (datum_is_the_symbol(cur, "export")) {
      i += 2;
      list_append(&res, list_copy(source, i - 2, i));
      continue;
    }
    if (datum_is_the_symbol(cur, "defn2")) {
      i += 4;
      list_append(&res, list_copy(source, i - 4, i));
      continue;
    }
    if (datum_is_the_symbol(cur, "switch")) {
      i += 3;
      list_append(&res, list_copy(source, i - 3, i));
      continue;
    }
    if (datum_is_the_symbol(cur, "fntest")) {
      i += 3;
      list_append(&res, list_copy(source, i - 3, i));
      continue;
    }
    if (datum_is_the_symbol(cur, "return")) {
      datum stmt = datum_make_nil();
      list_append(&stmt, datum_copy(list_at(source, i++)));
      for (; i < list_length(source);) {
        datum *item = list_at(source, i);
        list_append(&stmt, datum_copy(item));
        ++i;
        if (!datum_is_list(item) || datum_is_nil(item) ||
            !datum_is_the_symbol(list_at(item, 0), "at")) {
          break;
        }

      }
      list_append(&res, stmt);
      continue;
    }
    if (datum_is_list(cur) && list_length(cur) > 0 &&
        datum_is_the_symbol(list_at(cur, 0), "brackets")) {
      i += 1;
      list_append(&res, datum_copy(cur));
      continue;
    }
    if (datum_is_list(cur) && list_length(cur) > 0 &&
        datum_is_the_symbol(list_at(cur, 0), "return")) {
      fprintf(stderr, "warning: non-flat return: %s\n", datum_repr(cur));
      assert(false);
    }
    // fprintf(stderr, "warning: non-flat statement: %s\n", datum_repr(cur));
    // assert(false);
    i += 1;
    list_append(&res, datum_copy(cur));
    continue;
  }
  return res;
}

LOCAL char *prog_append_expressions_2(vec *sl, size_t *begin, datum *stmt,
                                   datum *compdata, extension *ext) {
  datum exprs = datum_make_list_of(*stmt);
  return prog_append_expressions(sl, begin, &exprs, compdata, ext, true);
}

LOCAL char *prog_append_expression(vec *sl, size_t *begin, datum *stmt,
                                   datum *compdata, extension *ext) {
  datum n = datum_make_nil();
  datum *op = &n;
  if (datum_is_list(stmt) && list_length(stmt) > 0) {
    op = list_at(stmt, 0);
  }
  datum op2 = datum_make_nil();
  if (datum_is_list(stmt) && list_length(stmt) > 1) {
    op2 = datum_copy(list_at(stmt, 1));
  }
  if (datum_is_the_symbol(op, "if")) {
    if (list_length(stmt) != 4) {
      return "if should have three args";
    }
    char *err;
    err = prog_append_expressions_2(sl, begin, list_at(stmt, 1), compdata, ext);
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
        prog_append_expressions_2(sl, &true_end, list_at(stmt, 2), compdata, ext);
    if (err != NULL) {
      return err;
    }
    err = prog_append_expressions_2(sl, &false_end, list_at(stmt, 3),
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
    err = prog_append_expressions_2(sl, begin, list_at(stmt, 1), compdata, ext);
    if (err != NULL) {
      return err;
    }
    size_t condition_check = *begin;
    compdata_del(compdata);
    *begin = vec_append_new(sl);
    size_t loop_start = *begin;
    err = prog_append_expressions_2(sl, begin, list_at(stmt, 2), compdata, ext);
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
  if (datum_is_the_symbol(op, "brackets")) {
    datum parts = list_get_tail(stmt);
    char *err =
        prog_append_expressions(sl, begin, &parts, compdata, ext, false);
    if (err != NULL) {
      return err;
    }
    return NULL;
  }
  if (datum_is_the_symbol(op, "list")) {
    for (int i = 1; i < list_length(stmt); ++i) {
      char *err =
          prog_append_expressions_2(sl, begin, list_at(stmt, i), compdata, ext);
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
    char *err = prog_append_expressions_2(sl, begin, expr, compdata, ext);
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
    datum meta = datum_make_nil();
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
      } else if (datum_is_list(content) && list_length(content) == 2 &&
                 datum_is_the_symbol(list_at(content, 0), "meta")) {
        meta = datum_copy(list_at(content, 1));
        ++index;
      } else if (!target_defined) {
        target = datum_copy(content);
        target_defined = true;
        ++index;
      } else {
        return "unknown return tag";
      }
    }
    size_t argcnt = 0;
    assert(index + 1 == list_length(stmt));
      datum *component = list_at(stmt, index);
      if (datum_is_list(component) && list_length(component) > 0 && datum_is_the_symbol(list_at(component, 0), "brackets")) {
        for (int i = 1; i < list_length(component); ++i) {
          char *err = prog_append_expression(sl, begin, list_at(component, i), compdata, ext);
        if (err != NULL) {
          return err;
        }
        ++argcnt;
        }
      } else {
        char *err = prog_append_expression(sl, begin, component, compdata, ext);
        if (err != NULL) {
          return err;
        }
        ++argcnt;
      }
    prog_append_yield(sl, begin, target, argcnt, recieve_count, meta, compdata);
    return NULL;
  }
  if (!datum_is_nil(op)) {
    char *res = ext->call(ext, sl, begin, stmt, compdata);
    if (res == NULL) {
      return NULL;
    }
    if (strcmp(res, "<not an extension>")) {
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
      char *err = prog_append_expressions_2(sl, begin, arg, compdata, ext);
      if (err != NULL) {
        return err;
      }
    }
  }
  prog_append_call(sl, begin, capture_size, indices, !mut, pre, target,
                   arg_count, ret_count, compdata);
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

LOCAL char *prog_init_routine(vec *sl, size_t s, datum *args, datum *stmt,
                              datum *routine_compdata, extension *ext) {
  if (args == NULL) {
    return "args can't be null";
  } else {
    prog_append_recieve(sl, &s, args, datum_make_nil(), routine_compdata);
  }
  return prog_append_expressions_2(sl, &s, stmt, routine_compdata, ext);
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
