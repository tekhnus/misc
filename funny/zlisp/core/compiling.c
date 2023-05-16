#include <assert.h>
#include <extern.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#if INTERFACE
typedef struct extension extension;
struct extension {
  char *(*call)(extension *self, vec *sl, size_t *begin, datum *stmt, int *i,
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
  char *err = prog_append_expressions(&sl, &p, source, compdata, ext);
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

EXPORT char *prog_append_expressions(vec *sl, size_t *off, datum *source,
                                     datum *compdata, extension *ext) {
  assert(datum_is_list(source));
  int i = 0;
  for (;;) {
    if (i >= list_length(source)) {
      break;
    }
    char *err =
        prog_append_consume_expression(sl, off, source, &i, compdata, ext);
    if (err != NULL) {
      return err;
    }
  }
  return NULL;
}

LOCAL char *prog_append_expression(vec *sl, size_t *begin, datum *stmt,
                                   datum *compdata, extension *ext) {
  datum exprs = datum_make_list_of(*stmt);
  return prog_append_expressions(sl, begin, &exprs, compdata, ext);
}

LOCAL char *prog_append_consume_expression(vec *sl, size_t *off, datum *source,
                                           int *i, datum *compdata,
                                           extension *ext) {
  assert(*i >= 0);
  assert(*i < list_length(source));
  datum *head = list_at(source, *i);
  if (datum_is_the_symbol(head, "if")) {
    (*i)++;
    datum *cond = list_at(source, (*i)++);
    datum *true_branch = list_at(source, (*i)++);
    datum *false_branch = list_at(source, (*i)++);
    char *err;
    err = prog_append_expression(sl, off, cond, compdata, ext);
    if (err != NULL) {
      return err;
    }

    size_t true_end = vec_append_new(sl), false_end = vec_append_new(sl);

    *vec_at(sl, *off) =
        datum_make_list_of(datum_make_symbol(":if"), datum_make_int(true_end),
                           datum_make_int(false_end));
    *off = vec_append_new(sl);

    compdata_del(compdata);
    datum false_compdata_val = datum_copy(compdata);
    datum *false_compdata = &false_compdata_val;
    err = prog_append_expression(sl, &true_end, true_branch, compdata, ext);
    if (err != NULL) {
      return err;
    }
    err = prog_append_expression(sl, &false_end, false_branch, false_compdata,
                                 ext);
    if (err != NULL) {
      return err;
    }
    if (!datum_eq(compdata, false_compdata)) {
      compdata_put(compdata, datum_make_symbol("__different_if_branches"));
    }

    prog_join(sl, true_end, false_end, *off);
    return NULL;
  }
  if (datum_is_the_symbol(head, "while")) {
    (*i)++;
    datum *cond = list_at(source, (*i)++);
    datum *body = list_at(source, (*i)++);
    char *err;
    size_t pre_condition_check = *off;
    datum pre_condition_check_compdata = datum_copy(compdata);
    err = prog_append_expression(sl, off, cond, compdata, ext);
    if (err != NULL) {
      return err;
    }
    size_t condition_check = *off;
    compdata_del(compdata);
    *off = vec_append_new(sl);
    size_t loop_start = *off;
    err = prog_append_expression(sl, off, body, compdata, ext);
    assert(datum_eq(&pre_condition_check_compdata, compdata));
    *vec_at(sl, *off) = datum_make_list_of(datum_make_symbol(":nop"),
                                           datum_make_int(pre_condition_check));
    *off = vec_append_new(sl);
    size_t loop_end = *off;
    *vec_at(sl, condition_check) =
        datum_make_list_of(datum_make_symbol(":if"), datum_make_int(loop_start),
                           datum_make_int(loop_end));
    return NULL;
  }
  if (*i + 2 < list_length(source) &&
      datum_is_the_symbol(list_at(source, *i + 1), "=")) {
    datum *dst = list_at(source, (*i)++);
    (*i)++;
    datum *expr = list_at(source, (*i)++);
    char *err = prog_append_expression(sl, off, expr, compdata, ext);
    if (err != NULL) {
      return err;
    }
    datum names;
    if (datum_is_list(dst)) {
      names = datum_copy(dst);
    } else {
      names = datum_make_list_of(datum_copy(dst));
    }
    store_values_to_variables(sl, off, &names, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(head, "defn")) {
    (*i)++;
    datum *name = list_at(source, (*i)++);
    datum *args = list_at(source, (*i)++);
    datum *body = list_at(source, (*i)++);
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
    prog_append_put_prog(sl, off, s_off, 2, compdata);
    datum name_singleton = datum_make_list_of(datum_copy(name));
    store_values_to_variables(sl, off, &name_singleton, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(head, "return")) {
    (*i)++;
    datum target = datum_make_symbol("plain");
    bool target_defined = false;
    size_t recieve_count = 0;
    datum meta = datum_make_nil();
    while (*i < list_length(source)) {
      datum *tag = list_at(source, *i);
      if (!datum_is_list(tag) || list_length(tag) != 2 ||
          !datum_is_the_symbol(list_at(tag, 0), "at")) {
        break;
      }
      datum *content = list_at(tag, 1);
      if (datum_is_integer(content)) {
        recieve_count = content->integer_value;
        ++(*i);
      } else if (datum_is_list(content) && list_length(content) == 2 &&
                 datum_is_the_symbol(list_at(content, 0), "meta")) {
        meta = datum_copy(list_at(content, 1));
        ++(*i);
      } else if (!target_defined) {
        target = datum_copy(content);
        target_defined = true;
        ++(*i);
      } else {
        return "unknown return tag";
      }
    }
    datum *component = list_at(source, (*i)++);
    size_t argcnt;
    size_t before = compdata_get_length(compdata);
    char *err = prog_append_expression(sl, off, component, compdata, ext);
    if (err != NULL) {
      return err;
    }
    size_t after = compdata_get_length(compdata);
    argcnt = after - before;
    prog_append_yield(sl, off, target, argcnt, recieve_count, meta, compdata);
    return NULL;
  }
  if (datum_is_list(head) && list_length(head) > 0 &&
      datum_is_the_symbol(list_at(head, 0), "brackets")) {
    *i += 1;
    datum parts = list_get_tail(head);
    return prog_append_expressions(sl, off, &parts, compdata, ext);
  }
  char *err = ext->call(ext, sl, off, source, i, compdata);
  if (err == NULL || strcmp(err, "<not an extension>")) {
    return err;
  }

  *i += 1;
  if (datum_is_constant(head)) {
    prog_append_put_const(sl, off, head, compdata);
    return NULL;
  }
  if (datum_is_symbol(head)) {
    datum debug_compdata = datum_copy(compdata);
    prog_append_yield(sl, off,
                      datum_make_list_of(datum_make_symbol("debugger"),
                                         datum_make_symbol("compdata"),
                                         debug_compdata),
                      0, 0, datum_make_nil(), compdata);
    prog_append_put_var(sl, off, head, compdata);
    return NULL;
  }
  if (!datum_is_list(head) || datum_is_nil(head)) {
    return "expected an s-expression";
  }
  datum *fn = list_at(head, 0);
  if (datum_is_the_symbol(fn, "list")) {
    for (int i = 1; i < list_length(head); ++i) {
      char *err =
          prog_append_expression(sl, off, list_at(head, i), compdata, ext);
      if (err != NULL) {
        return err;
      }
    }
    prog_append_collect(sl, list_length(head) - 1, off, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(fn, "quote")) {
    if (list_length(head) != 2) {
      return "quote should have a single arg";
    }
    prog_append_put_const(sl, off, list_at(head, 1), compdata);
    return NULL;
  }
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
  while (index < list_length(head)) {
    datum *tag = list_at(head, index);
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
      char *err = prog_append_expression(sl, off, component, compdata, ext);
      if (err != NULL) {
        return err;
      }
      datum idx = compdata_get_top_polyindex(compdata);
      list_append(&indices, idx);
    }
  }
  // TODO(): this is wrong argcount if there are braces.
  size_t arg_count = list_length(head) - index;
  for (; index < list_length(head); ++index) {
    datum *arg = list_at(head, index);
    if (hash) {
      prog_append_put_const(sl, off, arg, compdata);
    } else {
      char *err = prog_append_expression(sl, off, arg, compdata, ext);
      if (err != NULL) {
        return err;
      }
    }
  }
  prog_append_call(sl, off, capture_size, indices, !mut, pre, target, arg_count,
                   ret_count, compdata);
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

LOCAL size_t compdata_get_length(datum *compdata) {
  return list_length(list_get_last(compdata));
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
