#include <assert.h>
#include <extern.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#if INTERFACE
typedef struct extension extension;
struct extension {
  char *(*call)(extension *self, vec *sl, datum *stmt, int *i, datum *compdata);
};
#endif

EXPORT char *prog_compile_and_relocate(vec *sl, datum *source, datum *compdata,
                                       extension *ext) {
  fdatum bytecode = prog_compile(source, compdata, ext);
  if (fdatum_is_panic(bytecode)) {
    return bytecode.panic_message;
  }
  vec *bc = list_to_vec(&bytecode.ok_value);
  prog_append_bytecode(sl, bc);
  return NULL;
}

EXPORT fdatum prog_compile(datum *source, datum *compdata, extension *ext) {
  vec sl = vec_create_slice();
  char *err = prog_append_expressions(&sl, source, compdata, ext);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  return fdatum_make_ok(vec_to_datum(&sl));
}

EXPORT char *prog_append_expressions(vec *sl, datum *source, datum *compdata,
                                     extension *ext) {
  assert(datum_is_list(source));
  int i = 0;
  for (;;) {
    if (i >= list_length(source)) {
      break;
    }
    int i_before = i;
    char *err = prog_append_consume_expression(sl, source, &i, compdata, ext);
    if (err != NULL) {
      return err;
    }
    if (i < list_length(source)) {
      prog_append_yield(sl,
                        datum_make_list_of(datum_make_symbol("debugger"),
                                           datum_make_symbol("statement"),
                                           list_copy(source, i_before, i)),
                        0, 0, datum_make_nil(), compdata);
    }
  }
  return NULL;
}

LOCAL char *prog_append_expression(vec *sl, datum *stmt, datum *compdata,
                                   extension *ext) {
  datum exprs = datum_make_list_of(*stmt);
  return prog_append_expressions(sl, &exprs, compdata, ext);
}

LOCAL char *prog_append_consume_expression(vec *sl, datum *source, int *i,
                                           datum *compdata, extension *ext) {
  int i_val = *i;
  char *err = ext->call(ext, sl, source, i, compdata);
  if (err != NULL) {
    return err;
  }
  if (i_val != *i) {
    return NULL;
  }
  datum *head = list_at(source, (*i)++);
  if (datum_is_the_symbol(head, "quote")) {
    datum *val = list_at(source, (*i)++);
    prog_append_put_const(sl, val, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(head, "list")) {
    datum *vals = list_at(source, (*i)++);
    int before = compdata_get_length(compdata);
    prog_append_expression(sl, vals, compdata, ext);
    int after = compdata_get_length(compdata);
    prog_append_collect(sl, after - before, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(head, "call")) {
    datum *exp = list_at(source, (*i)++);
    assert(datum_is_list(exp) && list_length(exp) > 0 &&
           datum_is_the_symbol(list_at(exp, 0), "brackets"));
    datum expr = list_get_tail(exp);
    return prog_append_apply(sl, &expr, compdata, ext);
  }
  if (datum_is_the_symbol(head, "if")) {
    datum *cond = list_at(source, (*i)++);
    datum *true_branch = list_at(source, (*i)++);
    datum *false_branch = list_at(source, (*i)++);
    char *err = prog_append_expression(sl, cond, compdata, ext);
    if (err != NULL) {
      return err;
    }
    size_t if_instruction = prog_append_something(sl); // filled below.
    compdata_del(compdata);
    datum false_compdata_val = datum_copy(compdata);
    datum *false_compdata = &false_compdata_val;
    err = prog_append_expression(sl, true_branch, compdata, ext);
    if (err != NULL) {
      return err;
    }
    size_t true_end = prog_append_something(sl); // filled below.
    *vec_at(sl, if_instruction) =
        prog_get_if(prog_get_next_index(sl) - if_instruction);
    err = prog_append_merge_compdata(sl, false_compdata, compdata);
    if (err != NULL) {
      return err;
    }
    err = prog_append_expression(sl, false_branch, false_compdata, ext);
    if (err != NULL) {
      return err;
    }
    size_t false_end = prog_append_something(sl);
    *vec_at(sl, true_end) = prog_get_jmp(prog_get_next_index(sl) - true_end);
    err = prog_append_merge_compdata(sl, compdata, false_compdata);
    if (err != NULL) {
      return err;
    }
    *vec_at(sl, false_end) = prog_get_jmp(prog_get_next_index(sl) - false_end);
    return NULL;
  }
  if (datum_is_the_symbol(head, "while")) {
    datum *cond = list_at(source, (*i)++);
    datum *body = list_at(source, (*i)++);
    char *err;
    size_t pre_condition_check = prog_get_next_index(sl);
    datum pre_condition_check_compdata = datum_copy(compdata);
    err = prog_append_expression(sl, cond, compdata, ext);
    if (err != NULL) {
      return err;
    }
    size_t condition_check = prog_append_something(sl); // filled below.
    compdata_del(compdata);
    err = prog_append_expression(sl, body, compdata, ext);
    assert(datum_eq(&pre_condition_check_compdata, compdata));
    size_t jump_back = prog_append_something(sl); // filled immediately.
    *vec_at(sl, jump_back) = prog_get_jmp(pre_condition_check - jump_back);
    size_t loop_end = prog_get_next_index(sl);
    *vec_at(sl, condition_check) = prog_get_if(loop_end - condition_check);
    return NULL;
  }
  if (*i < list_length(source) &&
      datum_is_the_symbol(list_at(source, *i), "=")) {
    (*i)++;
    datum *expr = list_at(source, (*i)++);
    char *err = prog_append_expression(sl, expr, compdata, ext);
    if (err != NULL) {
      return err;
    }
    datum names;
    if (datum_is_list(head)) {
      assert(!datum_is_nil(head));
      if (!datum_is_the_symbol(list_at(head, 0), "brackets")) {
        fprintf(stderr, "!!! %s\n", datum_repr(head));
        assert(datum_is_the_symbol(list_at(head, 0), "brackets"));
      }
      names = list_get_tail(head);
    } else {
      names = datum_make_list_of(datum_copy(head));
    }
    store_values_to_variables(sl, &names, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(head, "defn")) {
    datum *name = list_at(source, (*i)++);
    datum *args_ = list_at(source, (*i)++);
    // fprintf(stderr, "%s\n", datum_repr(name));
    assert(datum_is_the_symbol(list_at(args_, 0), "brackets"));
    datum argsval = list_get_tail(args_);
    datum *args = &argsval;
    datum *body = list_at(source, (*i)++);
    size_t put_prog_off = prog_append_something(sl); // filled below.
    datum routine_compdata = datum_copy(compdata);
    compdata_put(&routine_compdata, datum_copy(name));
    compdata_start_new_section(&routine_compdata);

    size_t prog_off = prog_get_next_index(sl);
    if (datum_is_the_symbol(name, "__magically_called__")) {
      datum target = datum_make_symbol("plain");
      datum met = datum_make_nil();
      prog_append_yield(sl, target, 0, 0, met, &routine_compdata);
    }
    prog_append_yield(sl, datum_make_symbol("plain"), 0, list_length(args),
                      datum_make_nil(), &routine_compdata);
    store_values_to_variables(sl, args, &routine_compdata);
    char *err = prog_append_expression(sl, body, &routine_compdata, ext);
    if (err != NULL) {
      return err;
    }
    datum errm = datum_make_bytestring("routine guard reached");
    prog_append_put_const(sl, &errm, &routine_compdata);
    prog_append_yield(sl, datum_make_symbol("panic"), 1, 0, datum_make_nil(),
                      &routine_compdata);
    assert(put_prog_off + 1 == prog_off);
    *vec_at(sl, put_prog_off) =
        prog_get_put_prog(prog_get_next_index(sl) - put_prog_off, 2);
    compdata_put(compdata, datum_make_symbol(":anon"));
    datum name_singleton = datum_make_list_of(datum_copy(name));
    store_values_to_variables(sl, &name_singleton, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(head, "return")) {
    datum target = datum_make_symbol("plain");
    bool target_defined = false;
    size_t recieve_count = 0;
    datum meta = datum_make_nil();
    while (*i < list_length(source)) {
      datum *tag = list_at(source, *i);
      if (!datum_is_list(tag) || list_length(tag) != 3 ||
          !datum_is_the_symbol(list_at(tag, 0), "brackets") ||
          !datum_is_the_symbol(list_at(tag, 1), "at")) {
        break;
      }
      datum content = datum_copy(list_at(tag, 2));
      if (datum_is_list(&content)) {
        if (datum_is_the_symbol(list_at(&content, 0), "brackets")) {
          content = list_get_tail(&content);
        }
      }
      if (datum_is_integer(&content)) {
        recieve_count = content.integer_value;
        ++(*i);
      } else if (datum_is_list(&content) && list_length(&content) == 2 &&
                 datum_is_the_symbol(list_at(&content, 0), "meta")) {
        meta = datum_copy(list_at(&content, 1));
        ++(*i);
      } else if (!target_defined) {
        target = datum_copy(&content);
        target_defined = true;
        ++(*i);
      } else {
        return "unknown return tag";
      }
    }
    datum *component = list_at(source, (*i)++);
    size_t argcnt;
    size_t before = compdata_get_length(compdata);
    char *err = prog_append_expression(sl, component, compdata, ext);
    if (err != NULL) {
      return err;
    }
    size_t after = compdata_get_length(compdata);
    argcnt = after - before;
    prog_append_yield(sl, target, argcnt, recieve_count, meta, compdata);
    return NULL;
  }
  if (datum_is_list(head) && list_length(head) > 0 &&
      datum_is_the_symbol(list_at(head, 0), "brackets")) {
    datum parts = list_get_tail(head);
    return prog_append_expressions(sl, &parts, compdata, ext);
  }

  if (datum_is_constant(head)) {
    prog_append_put_const(sl, head, compdata);
    return NULL;
  }
  if (datum_is_symbol(head)) {
    datum debug_compdata = datum_copy(compdata);
    prog_append_yield(sl,
                      datum_make_list_of(datum_make_symbol("debugger"),
                                         datum_make_symbol("compdata"),
                                         debug_compdata),
                      0, 0, datum_make_nil(), compdata);
    prog_append_copy(sl, head, compdata);
    return NULL;
  }
  if (!datum_is_list(head) || datum_is_nil(head)) {
    return "expected an s-expression";
  }
  fprintf(stderr, "old-style call: %s\n", datum_repr(head));
  return prog_append_apply(sl, head, compdata, ext);
}

LOCAL char *prog_append_apply(vec *sl, datum *s_expr, datum *compdata,
                              extension *ext) {
  datum *fn = list_at(s_expr, 0);
  datum target = datum_make_symbol("plain");
  bool target_is_set = false;
  bool mut = false;
  size_t ret_count = 1;
  datum fnsv;
  if (datum_is_list(fn) && list_length(fn) >= 2 &&
      datum_is_the_symbol(list_at(fn, 0), "brackets") &&
      datum_is_the_symbol(list_at(fn, 1), "polysym")) {
    fnsv = list_get_tail(fn);
    fnsv = list_get_tail(&fnsv);
  } else {
    fnsv = datum_make_list_of(datum_copy(fn));
  }
  datum *fns = &fnsv;
  int index = 1;
  while (index < list_length(s_expr)) {
    datum *tag = list_at(s_expr, index);
    if (!datum_is_list(tag) || list_length(tag) != 3 ||
        !datum_is_the_symbol(list_at(tag, 0), "brackets") ||
        !datum_is_the_symbol(list_at(tag, 1), "at")) {
      break;
    }
    datum content = datum_copy(list_at(tag, 2));
    if (datum_is_list(&content)) {
      if (datum_is_the_symbol(list_at(&content, 0), "brackets")) {
        content = list_get_tail(&content);
      }
    }
    if (datum_is_integer(&content)) {
      ret_count = content.integer_value;
      ++index;
    } else if (datum_is_the_symbol(&content, "mut")) {
      mut = true;
      ++index;
    } else if (!target_is_set) {
      target = datum_copy(&content);
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
      char *err = prog_append_expression(sl, component, compdata, ext);
      if (err != NULL) {
        return err;
      }
      datum idx = compdata_get_top_polyindex(compdata);
      list_append(&indices, idx);
    }
  }
  int before = compdata_get_length(compdata);
  for (; index < list_length(s_expr); ++index) {
    datum *arg = list_at(s_expr, index);
    char *err = prog_append_expression(sl, arg, compdata, ext);
    if (err != NULL) {
      return err;
    }
  }
  int after = compdata_get_length(compdata);
  size_t arg_count = after - before;
  prog_append_call(sl, capture_size, indices, !mut, target, arg_count,
                   ret_count, compdata);
  return NULL;
}

EXPORT void prog_append_bytecode(vec *sl, vec *src_sl) {
  for (size_t i = 0; i < prog_get_next_index(src_sl); ++i) {
    datum *ins = vec_at(src_sl, i);
    vec_append(sl, datum_copy(ins));
  }
}

EXPORT void prog_append_call(vec *sl, size_t capture_size, datum indices,
                             bool pop_one, datum type, int arg_count,
                             int return_count, datum *compdata) {
  assert(datum_is_list(&indices));
  indices = list_to_brackets(&indices);
  vec_append(sl, datum_make_list_of(
                     datum_make_symbol(":call"), datum_make_int(capture_size),
                     indices, datum_make_int(pop_one), type,
                     datum_make_int(arg_count), datum_make_int(return_count)));
  for (int i = 0; i < arg_count; ++i) {
    compdata_del(compdata);
  }
  if (pop_one) {
    compdata_del(compdata);
  }
  for (int i = 0; i < return_count; ++i) {
    compdata_put(compdata, datum_make_symbol(":anon"));
  }
}

EXPORT void prog_append_copy(vec *sl, datum *val, datum *compdata) {
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
  vec_append(sl, datum_make_list_of(datum_make_symbol(":copy"),
                                    list_to_brackets(&target_polyindex),
                                    list_to_brackets(&polyindex)));
}

LOCAL void prog_append_move(vec *sl, datum *target, datum *source,
                            datum *compdata) {
  vec_append(sl, datum_make_list_of(datum_make_symbol(":move"),
                                    list_to_brackets((target)),
                                    list_to_brackets((source))));
  compdata_del(compdata);
}

EXPORT void prog_append_yield(vec *sl, datum type, size_t count,
                              size_t recieve_count, datum meta,
                              datum *compdata) {
  vec_append(sl, datum_make_list_of(
                     datum_make_symbol(":yield"), list_to_brackets(&type),
                     datum_make_int(count), datum_make_int(recieve_count),
                     list_to_brackets(&meta)));
  for (size_t i = 0; i < count; ++i) {
    compdata_del(compdata);
  }
  for (size_t i = 0; i < recieve_count; ++i) {
    compdata_put(compdata, datum_make_symbol(":anon"));
  }
}

EXPORT size_t prog_append_something(vec *sl) {
  size_t cur = prog_get_next_index(sl);
  vec_append(sl, datum_make_nil());
  return cur;
}

LOCAL void prog_append_put_const(vec *sl, datum *val, datum *compdata) {
  vec_append(
      sl, datum_make_list_of(datum_make_symbol(":put-const"), datum_copy(val)));
  compdata_put(compdata, datum_make_symbol(":anon"));
}

LOCAL void prog_append_collect(vec *sl, size_t count, datum *compdata) {
  vec_append(sl, datum_make_list_of(datum_make_symbol(":collect"),
                                    datum_make_int(count)));
  for (size_t i = 0; i < count; ++i) {
    compdata_del(compdata);
  }
  compdata_put(compdata, datum_make_symbol(":anon"));
}

EXPORT datum prog_get_put_prog(ptrdiff_t delta, int capture) {
  return datum_make_list_of(datum_make_symbol(":put-prog"),
                            datum_make_int(capture), datum_make_int(delta));
}

EXPORT datum prog_get_jmp(ptrdiff_t delta) {
  return datum_make_list_of(datum_make_symbol(":jmp"), datum_make_int(delta));
}

LOCAL datum prog_get_if(ptrdiff_t delta) {
  return datum_make_list_of(datum_make_symbol(":if"), datum_make_int(delta));
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
  datum *outer_frame = list_get_last(compdata);
  return !datum_is_nil(outer_frame) &&
         datum_is_the_symbol(list_get_last(outer_frame), ":anon");
}

EXPORT void compdata_put(datum *compdata, datum var) {
  datum *last_frame = list_get_last(compdata);
  list_append(last_frame, var);
}

LOCAL void compdata_del(datum *compdata) {
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

LOCAL datum *compdata_get_top_section(datum *compdata) {
  return list_get_last(compdata);
}

LOCAL datum list_subtract(datum *a, datum *b) {
  if (list_length(a) < list_length(b)) {
    return datum_make_bytestring("length mismatch");
  };
  for (int i = 0; i < list_length(b); ++i) {
    if (!datum_eq(list_at(a, i), list_at(b, i))) {
      return datum_make_bytestring("list_subtract error");
    }
  }
  datum res = datum_make_nil();
  for (int i = list_length(b); i < list_length(a); ++i) {
    list_append(&res, datum_copy(list_at(a, i)));
  }
  return res;
}

EXPORT datum compdata_get_shape(datum *compdata) {
  datum res = datum_make_nil();
  for (int i = 0; i < list_length(compdata); ++i) {
    datum ii = datum_make_int(list_length(list_at(compdata, i)));
    list_append(&res, ii);
  }
  return res;
}

LOCAL char *prog_append_merge_compdata(vec *sl, datum *compdata,
                                       datum *another_compdata) {
  datum nil = datum_make_list_of(datum_make_symbol("brackets"));
  datum vars = list_subtract(compdata_get_top_section(another_compdata),
                             compdata_get_top_section(compdata));
  if (datum_is_bytestring(&vars)) {
    return "bad if branches";
  }
  for (int i = 0; i < list_length(&vars); ++i) {
    prog_append_put_const(sl, &nil, compdata);
  }
  compdata_give_names(compdata, &vars);
  assert(datum_eq(compdata_get_top_section(another_compdata),
                  compdata_get_top_section(compdata)));
  return NULL;
}

EXPORT void store_values_to_variables(vec *sl, datum *var, datum *compdata) {
  if (!datum_is_list(var)) {
    fprintf(stderr, "error: compdata_give_names\n");
    exit(EXIT_FAILURE);
  }
  assert(sl);
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
    compdata_give_names(compdata, var);
  }
  if (set) {
    move_values_to_variables(sl, var, compdata);
  }
}

LOCAL void compdata_give_names(datum *compdata, datum *var) {
  for (int i = 0; i < list_length(var); ++i) {
    compdata_del(compdata);
  }
  for (int i = 0; i < list_length(var); ++i) {
    // datum target = compdata_get_polyindex(compdata, list_at(var, i));
    // fprintf(stderr, "%s\n", datum_repr(list_at(var, i)));
    // assert(datum_is_nil(&target));
    compdata_put(compdata, datum_copy(list_at(var, i)));
  }
}

LOCAL void move_values_to_variables(vec *sl, datum *var, datum *compdata) {
  for (int i = 0; i < list_length(var); ++i) {
    int idx = list_length(var) - i - 1;
    datum target = compdata_get_polyindex(compdata, list_at(var, idx));
    assert(!datum_is_nil(&target));
    datum source = compdata_get_top_polyindex(compdata);
    prog_append_move(sl, &target, &source, compdata);
  }
}

EXPORT vec vec_create_slice() {
  vec sl = vec_make(16 * 1024);
  return sl;
}

EXPORT size_t prog_get_next_index(vec *sl) { return vec_length(sl); }

LOCAL datum list_to_brackets(datum *list) {
  if (!datum_is_list(list)) {
    return datum_copy(list);
  }
  datum res = datum_make_list_of(datum_make_symbol("brackets"));
  for (int k = 0; k < list_length(list); ++k) {
    list_append(&res, list_to_brackets(list_at(list, k)));
  }
  return res;
}
