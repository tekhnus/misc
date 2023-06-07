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
  vec bc = list_to_vec(&bytecode.ok_value);
  prog_append_bytecode(sl, &bc);
  return NULL;
}

EXPORT fdatum prog_compile(datum *source, datum *compdata, extension *ext) {
  vec sl = vec_create_slice();
  char *err = prog_append_expressions(&sl, source, compdata, ext);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  return fdatum_make_ok(datum_make_list(sl));
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
                        compdata_get_next_polyindex(compdata), 0, 0,
                        datum_make_nil(), compdata);
    }
  }
  return NULL;
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
  if (datum_is_the_symbol(head, "if")) {
    char *err = prog_append_consume_expression(sl, source, i, compdata, ext);
    if (err != NULL) {
      return err;
    }
    datum idx = compdata_get_top_polyindex(compdata);
    size_t if_instruction = prog_append_something(sl); // filled below.
    compdata_del(compdata);
    datum false_compdata = datum_copy(compdata);
    err = prog_append_consume_expression(sl, source, i, compdata, ext);
    if (err != NULL) {
      return err;
    }
    size_t true_end = prog_append_something(sl); // filled below.
    *vec_at(sl, if_instruction) =
        prog_get_if(prog_get_next_index(sl) - if_instruction, idx);
    err = prog_append_consume_expression(sl, source, i, &false_compdata, ext);
    if (err != NULL) {
      return err;
    }
    if (!datum_eq(compdata, &false_compdata)) {
      return "different if branches";
    }
    *vec_at(sl, true_end) = prog_get_jmp(prog_get_next_index(sl) - true_end);
    return NULL;
  }
  if (datum_is_the_symbol(head, "while")) {
    char *err;
    size_t pre_condition_check = prog_get_next_index(sl);
    datum pre_condition_check_compdata = datum_copy(compdata);
    err = prog_append_consume_expression(sl, source, i, compdata, ext);
    if (err != NULL) {
      return err;
    }
    datum idx = compdata_get_top_polyindex(compdata);
    size_t condition_check = prog_append_something(sl); // filled below.
    compdata_del(compdata);
    err = prog_append_consume_expression(sl, source, i, compdata, ext);
    if (err != NULL) {
      return err;
    }
    assert(datum_eq(&pre_condition_check_compdata, compdata));
    size_t jump_back = prog_append_something(sl); // filled immediately.
    *vec_at(sl, jump_back) = prog_get_jmp(pre_condition_check - jump_back);
    size_t loop_end = prog_get_next_index(sl);
    *vec_at(sl, condition_check) = prog_get_if(loop_end - condition_check, idx);
    return NULL;
  }
  if (*i < list_length(source) &&
      datum_is_the_symbol(list_at(source, *i), ":=")) {
    (*i)++;
    char *err = prog_append_consume_expression(sl, source, i, compdata, ext);
    if (err != NULL) {
      return err;
    }
    datum names;
    if (datum_is_list(head)) {
      names = datum_copy(head);
    } else {
      names = datum_make_list_of(datum_copy(head));
    }
    compdata_give_names(compdata, &names);
    return NULL;
  }
  if (*i < list_length(source) &&
      datum_is_the_symbol(list_at(source, *i), "=")) {
    (*i)++;
    char *err = prog_append_consume_expression(sl, source, i, compdata, ext);
    if (err != NULL) {
      return err;
    }
    datum names;
    if (datum_is_list(head)) {
      names = datum_copy(head);
    } else {
      names = datum_make_list_of(datum_copy(head));
    }
    move_values_to_variables(sl, &names, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(head, "fn") ||
      datum_is_the_symbol(head, "magically_called_fn")) {
    size_t put_prog_off = prog_append_something(sl); // filled below.
    datum routine_compdata = datum_copy(compdata);
    compdata_start_new_section(&routine_compdata);

    size_t prog_off = prog_get_next_index(sl);
    if (datum_is_the_symbol(head, "magically_called_fn")) {
      datum target = datum_make_symbol("plain");
      datum met = datum_make_nil();
      prog_append_yield(sl, target,
                        compdata_get_next_polyindex(&routine_compdata), 0, 0,
                        met, &routine_compdata);
    }
    datum *args = list_at(source, (*i)++);
    prog_append_yield(sl, datum_make_symbol("plain"),
                      compdata_get_next_polyindex(&routine_compdata), 0,
                      list_length(args), datum_make_nil(), &routine_compdata);
    compdata_give_names(&routine_compdata, args);
    char *err =
        prog_append_consume_expression(sl, source, i, &routine_compdata, ext);
    if (err != NULL) {
      return err;
    }
    datum errm = datum_make_bytestring("routine guard reached");
    prog_append_put_const(sl, &errm, &routine_compdata);
    prog_append_yield(sl, datum_make_symbol("panic"),
                      compdata_get_top_polyindex(&routine_compdata), 1, 0,
                      datum_make_nil(), &routine_compdata);
    assert(put_prog_off + 1 == prog_off);
    compdata_put(compdata, datum_make_symbol(":anon"));
    datum pi = compdata_get_top_polyindex(compdata);
    *vec_at(sl, put_prog_off) =
        prog_get_put_prog(&pi, prog_get_next_index(sl) - put_prog_off, 2);
    return NULL;
  }
  if (datum_is_the_symbol(head, "return")) {
    datum target = datum_make_symbol("plain");
    bool target_defined = false;
    size_t recieve_count = 0;
    datum meta = datum_make_nil();
    while (*i < list_length(source)) {
      datum *tag = list_at(source, *i);
      datum *content_val;
      if (datum_is_the_symbol(tag, "at")) {
        content_val = list_at(list_at(source, ++(*i)), 0);
      } else {
        break;
      }
      if (datum_is_integer(content_val)) {
        recieve_count = content_val->integer_value;
        ++(*i);
      } else if (datum_is_list(content_val) && list_length(content_val) == 2 &&
                 datum_is_the_symbol(list_at(content_val, 0), "meta")) {
        meta = datum_copy(list_at(content_val, 1));
        ++(*i);
      } else if (!target_defined) {
        target = datum_copy(content_val);
        target_defined = true;
        ++(*i);
      } else {
        return "unknown return tag";
      }
    }
    size_t argcnt;
    size_t before = compdata_get_length(compdata);
    datum idx = compdata_get_next_polyindex(compdata);
    char *err = prog_append_consume_expression(sl, source, i, compdata, ext);
    if (err != NULL) {
      return err;
    }
    size_t after = compdata_get_length(compdata);
    argcnt = after - before;
    prog_append_yield(sl, target, idx, argcnt, recieve_count, meta, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(head, "flat")) {
    datum *vals = list_at(list_at(source, (*i)++), 0);
    if (!datum_is_list(vals)) {
      fprintf(stderr, "bad: %s\n", datum_repr(vals));
      return "bad";
    }
    int j = 0;
    while (j < list_length(vals)) {
      prog_append_consume_expression(sl, vals, &j, compdata, ext);
    }
    return NULL;
  }
  if (datum_is_the_symbol(head, "quote")) {
    datum *val = list_at(source, (*i)++);
    assert(datum_is_list(val));
    for (int j = 0; j < list_length(val); ++j) {
      prog_append_put_const(sl, list_at(val, j), compdata);
    }
    return NULL;
  }
  if (datum_is_list(head) && list_length(head) == 2 &&
      datum_is_the_symbol(list_at(head, 0), "call")) {
    datum *exp = list_at(head, 1);
    return prog_append_apply(sl, exp, compdata, ext);
    return NULL;
  }
  if (datum_is_list(head)) {
    int before = compdata_get_length(compdata);
    datum *vals = head;
    int j = 0;
    datum idx = compdata_get_next_polyindex(compdata);
    while (j < list_length(vals)) {
      prog_append_consume_expression(sl, vals, &j, compdata, ext);
    }
    int after = compdata_get_length(compdata);
    prog_append_collect(sl, after - before, idx, compdata);
    return NULL;
  }
  if (datum_is_constant(head)) {
    prog_append_put_const(sl, head, compdata);
    return NULL;
  }
  if (datum_is_symbol(head)) {
    /* datum debug_compdata = datum_copy(compdata); */
    /* prog_append_yield(sl, */
    /*                   datum_make_list_of(datum_make_symbol("debugger"), */
    /*                                      datum_make_symbol("compdata"), */
    /*                                      debug_compdata), */
    /*                   0, 0, datum_make_nil(), compdata); */
    prog_append_copy(sl, head, compdata);
    return NULL;
  }
  return "unexpected datum type in expression";
}

LOCAL char *prog_append_apply(vec *sl, datum *s_expr, datum *compdata,
                              extension *ext) {
  datum *fn = list_at(s_expr, 0);
  datum target = datum_make_symbol("plain");
  bool target_is_set = false;
  bool mut = false;
  size_t ret_count = 1;
  datum fnsv;
  if (datum_is_list(fn) && list_length(fn) >= 1 &&
      datum_is_the_symbol(list_at(fn, 0), "polysym")) {
    fnsv = list_get_tail(fn);
  } else {
    fnsv = datum_make_list_of(datum_copy(fn));
  }
  datum *fns = &fnsv;
  int index = 1;
  while (index < list_length(s_expr)) {
    datum *tag = list_at(s_expr, index);
    datum *content;
    if (datum_is_the_symbol(tag, "at")) {
      content = list_at(list_at(s_expr, ++index), 0);
    } else {
      break;
    }
    if (datum_is_integer(content)) {
      ret_count = content->integer_value;
      ++index;
    } else if (datum_is_the_symbol(content, "mut")) {
      mut = true;
      ++index;
    } else if (!target_is_set) {
      target = datum_copy(content);
      target_is_set = true;
      ++index;
    } else {
      return "unknown tag";
    }
  }
  vec indices = vec_make(0);
  int fn_index = 0;
  int chop = 0;
  if (fn_index < list_length(fns) &&
      datum_is_the_symbol(list_at(fns, fn_index), "..")) {
    ++fn_index;
    chop = 1;
  }
  size_t frames = compdata_get_frame_count(compdata);
  if (fn_index < list_length(fns) &&
      datum_is_the_symbol(list_at(fns, fn_index), "empty-symbol")) {
    ++fn_index;
    chop = frames;
  }
  size_t capture_size = 0;
  for (int j = 0; j + chop < (int)frames; ++j) {
    ++capture_size;
  }
  while (fn_index < list_length(fns)) {
    bool borrow = fn_index + 1 < list_length(fns) || mut;
    if (borrow) {
      datum *component = list_at(fns, fn_index++);
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
      vec_append(&indices, idx);
    } else {
      char *err =
          prog_append_consume_expression(sl, fns, &fn_index, compdata, ext);
      if (err != NULL) {
        return err;
      }
      datum idx = compdata_get_top_polyindex(compdata);
      vec_append(&indices, idx);
    }
  }
  datum top_arg_poly = compdata_get_next_polyindex(compdata);
  int before = compdata_get_length(compdata);
  while (index < list_length(s_expr)) {
    char *err =
        prog_append_consume_expression(sl, s_expr, &index, compdata, ext);
    if (err != NULL) {
      return err;
    }
  }
  int after = compdata_get_length(compdata);
  size_t arg_count = after - before;
  prog_append_call(sl, capture_size, datum_make_list(indices), !mut, target,
                   arg_count, ret_count, top_arg_poly, compdata);
  return NULL;
}

EXPORT void prog_append_bytecode(vec *sl, vec *src_sl) {
  for (size_t i = 0; i < prog_get_next_index(src_sl); ++i) {
    datum *ins = vec_at(src_sl, i);
    vec_append(sl, datum_copy(ins));
  }
}

LOCAL void prog_append_call(vec *sl, size_t capture_size, datum indices,
                            bool pop_one, datum type, int arg_count,
                            int return_count, datum top_arg_polyindex,
                            datum *compdata) {
  assert(datum_is_list(&indices));
  vec_append(sl, datum_make_list_of(
                     datum_make_symbol(":call"), datum_make_int(capture_size),
                     indices, datum_make_int(pop_one), type,
                     datum_make_int(arg_count), datum_make_int(return_count),
                     top_arg_polyindex));
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

LOCAL datum prog_append_copy(vec *sl, datum *val, datum *compdata) {
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
                                    target_polyindex, polyindex));
  return target_polyindex;
}

LOCAL void prog_append_move(vec *sl, datum *target, datum *source,
                            datum *compdata) {
  vec_append(sl, datum_make_list_of(datum_make_symbol(":move"),
                                    datum_copy(target), datum_copy(source)));
  compdata_del(compdata);
}

EXPORT void prog_append_yield(vec *sl, datum type, datum yield_val_index,
                              size_t count, size_t recieve_count, datum meta,
                              datum *compdata) {
  for (size_t i = 0; i < count; ++i) {
    compdata_del(compdata);
  }
  for (size_t i = 0; i < recieve_count; ++i) {
    compdata_put(compdata, datum_make_symbol(":anon"));
  }
  vec_append(sl, datum_make_list_of(datum_make_symbol(":yield"), type,
                                    yield_val_index, datum_make_int(count),
                                    datum_make_int(recieve_count), meta));
}

LOCAL size_t prog_append_something(vec *sl) {
  size_t cur = prog_get_next_index(sl);
  vec_append(sl, datum_make_nil());
  return cur;
}

LOCAL void prog_append_put_const(vec *sl, datum *val, datum *compdata) {
  compdata_put(compdata, datum_make_symbol(":anon"));
  datum target_polyindex = compdata_get_top_polyindex(compdata);
  vec_append(sl, datum_make_list_of(datum_make_symbol(":put-const"),
                                    target_polyindex, datum_copy(val)));
}

LOCAL void prog_append_collect(vec *sl, size_t count, datum top_idx,
                               datum *compdata) {
  vec_append(sl, datum_make_list_of(datum_make_symbol(":collect"),
                                    datum_make_int(count), top_idx));
  for (size_t i = 0; i < count; ++i) {
    compdata_del(compdata);
  }
  compdata_put(compdata, datum_make_symbol(":anon"));
}

EXPORT ptrdiff_t *prog_append_put_prog(vec *sl, int capture, datum *compdata) {
  datum target = compdata_put(compdata, datum_make_symbol(":anon"));
  datum ins = prog_get_put_prog(&target, 100500, capture);
  ptrdiff_t *delt = &list_at(&ins, 3)->integer_value;
  vec_append(sl, ins);
  return delt;
}

EXPORT ptrdiff_t *prog_append_jmp(vec *sl) {
  datum ins = prog_get_jmp(100500);
  size_t offset = prog_get_next_index(sl);
  vec_append(sl, ins);
  return prog_get_jmp_delta(sl, offset);
}

LOCAL datum prog_get_put_prog(datum *target, ptrdiff_t delta, int capture) {
  return datum_make_list_of(datum_make_symbol(":put-prog"), datum_copy(target),
                            datum_make_int(capture), datum_make_int(delta));
}

EXPORT ptrdiff_t *prog_get_jmp_delta(vec *sl, size_t offset) {
  return &list_at(vec_at(sl, offset), 1)->integer_value;
}

LOCAL datum prog_get_jmp(ptrdiff_t delta) {
  return datum_make_list_of(datum_make_symbol(":jmp"), datum_make_int(delta));
}

LOCAL datum prog_get_if(ptrdiff_t delta, datum index) {
  return datum_make_list_of(datum_make_symbol(":if"), index,
                            datum_make_int(delta));
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

LOCAL datum compdata_put(datum *compdata, datum var) {
  datum *last_frame = list_get_last(compdata);
  list_append_slow(last_frame, var);
  return compdata_get_top_polyindex(compdata);
}

LOCAL void compdata_del(datum *compdata) {
  datum *last_frame = list_get_last(compdata);
  *last_frame = list_pop_slow(last_frame);
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
  list_append_slow(compdata, nil);
}

LOCAL datum compdata_get_top_polyindex(datum *compdata) {
  size_t frames = list_length(compdata);
  size_t indices = list_length(list_get_last(compdata));
  assert(frames > 0 && indices > 0);
  return datum_make_list_of(datum_make_int(frames - 1),
                            datum_make_int(indices - 1));
}

EXPORT datum compdata_get_next_polyindex(datum *compdata) {
  size_t frames = list_length(compdata);
  size_t indices = list_length(list_get_last(compdata));
  assert(frames > 0);
  return datum_make_list_of(datum_make_int(frames - 1),
                            datum_make_int(indices));
}

LOCAL size_t compdata_get_length(datum *compdata) {
  return list_length(list_get_last(compdata));
}

LOCAL size_t compdata_get_frame_count(datum *compdata) {
  return list_length(compdata);
}

LOCAL datum *compdata_get_top_section(datum *compdata) {
  return list_get_last(compdata);
}

EXPORT void compdata_give_names(datum *compdata, datum *var) {
  for (int i = 0; i < list_length(var); ++i) {
    compdata_del(compdata);
  }
  for (int i = 0; i < list_length(var); ++i) {
    datum target = compdata_get_polyindex(compdata, list_at(var, i));
    if (!datum_is_nil(&target)) {
      fprintf(stderr, "erorr: redefinition of %s\n",
              datum_repr(list_at(var, i)));
      exit(EXIT_FAILURE);
    }
    compdata_put(compdata, datum_copy(list_at(var, i)));
  }
}

LOCAL void move_values_to_variables(vec *sl, datum *var, datum *compdata) {
  for (int i = 0; i < list_length(var); ++i) {
    int idx = list_length(var) - i - 1;
    datum target = compdata_get_polyindex(compdata, list_at(var, idx));
    if (datum_is_nil(&target)) {
      fprintf(stderr, "error: assignment to undeclared variable %s\n",
              datum_repr(list_at(var, idx)));
      exit(EXIT_FAILURE);
    }
    datum source = compdata_get_top_polyindex(compdata);
    prog_append_move(sl, &target, &source, compdata);
  }
}

EXPORT vec vec_create_slice() {
  vec sl = vec_make(16 * 1024);
  return sl;
}

EXPORT size_t prog_get_next_index(vec *sl) { return vec_length(sl); }

LOCAL void list_append_slow(datum *list, datum value) {
  datum newlist = list_make_copies(list_length(list) + 1, datum_make_nil());
  for (int i = 0; i < list_length(list); ++i) {
    *list_at(&newlist, i) = *list_at(list, i);
  }
  *list_at(&newlist, list_length(list)) = value;
  *list = newlist;
}

LOCAL datum list_pop_slow(datum *list) {
  assert(list_length(list) > 0);
  return list_copy(list, 0, list_length(list) - 1);
}
