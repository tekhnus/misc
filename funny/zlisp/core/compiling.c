#include <assert.h>
#include <compiling.h>
#include <types.h>
#include <zlisp/common.h>
#if INTERFACE
#include <stdbool.h>
#include <stddef.h>
#include <types.h>
#endif
#include <stdlib.h>
#include <string.h>

EXPORT context context_make() {
  return (context){};
}

EXPORT char *context_abort_reason(context *ctxt) {
  if (!ctxt->aborted) {
    return NULL;
  }
  return ctxt->error;
}

EXPORT void abortf(context *ctxt, char *format, ...) {
  va_list args;
  va_start(args, format);
  vsnprintf(ctxt->error, 1024, format, args);
  ctxt->aborted = true;
}

EXPORT datum prog_compile(vec *sl, datum *source, datum *compdata,
                         extension *ext, context *ctxt) {
  if (!datum_is_list(source)) {
    abortf(ctxt, "source should be a list, got %s", datum_repr(source));
    return (datum){};
  }
  vec res = vec_make(0);
  int i = 0;
  for (;;) {
    if (i >= list_length(source)) {
      break;
    }
    int i_before = i;
    datum exp = prog_append_expression(sl, source, &i, compdata, ext, ctxt);
    vec_extend(&res, &exp);
    if (ctxt->aborted) {
      return (datum){};
    }
    if (i < list_length(source)) {
      prog_append_yield(sl,
                        datum_make_list_of(datum_make_symbol("debugger"),
                                           datum_make_symbol("statement"),
                                           list_copy(source, i_before, i)),
                        datum_make_nil(), 0, 0,
                        datum_make_nil(), compdata);
    }
  }
  return datum_make_list_vec(res);
}

LOCAL datum prog_append_expression(vec *sl, datum *source, int *i,
                                  datum *compdata, extension *ext,
                                  context *ctxt) {
  int i_val = *i;
  datum res = ext->call(ext, sl, source, i, compdata, ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  if (i_val != *i) {
    return res;
  }
  datum *head = list_at(source, (*i)++);
  if (datum_is_the_symbol(head, "if")) {
    datum conditions = prog_append_expression(sl, source, i, compdata, ext, ctxt);
    assert(list_length(&conditions) == 1);
    datum *condition = list_at(&conditions, 0);

    if (ctxt->aborted) {
      return (datum){};
    }
    size_t if_instruction = prog_append_something(sl); // filled below.
    compdata_del(compdata);
    datum false_compdata = datum_copy(compdata);
    datum res = prog_append_expression(sl, source, i, compdata, ext, ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
    size_t true_end = prog_append_something(sl); // filled below.
    *vec_at(sl, if_instruction) =
        prog_get_if(prog_get_next_index(sl) - if_instruction, *condition);
    datum res2 = prog_append_expression(sl, source, i, &false_compdata, ext, ctxt);

    if (ctxt->aborted) {
      return (datum){};
    }
    if (!datum_eq(compdata, &false_compdata)) {
      abortf(ctxt, "different if branches");
      return (datum){};
    }
    if (!datum_eq(&res, &res2)) {
      abortf(ctxt, "different if branches (2)");
      return (datum){};
    }
    *vec_at(sl, true_end) = prog_get_jmp(prog_get_next_index(sl) - true_end);
    return res;
  }
  if (datum_is_the_symbol(head, "while")) {
    size_t pre_condition_check = prog_get_next_index(sl);
    datum pre_condition_check_compdata = datum_copy(compdata);
    datum conditions = prog_append_expression(sl, source, i, compdata, ext, ctxt);
    assert(list_length(&conditions) == 1);
    datum *condition = list_at(&conditions, 0);

    if (ctxt->aborted) {
      return (datum){};
    }
    size_t condition_check = prog_append_something(sl); // filled below.
    compdata_del(compdata);
    prog_append_expression(sl, source, i, compdata, ext, ctxt);

    if (ctxt->aborted) {
      return (datum){};
    }
    assert(datum_eq(&pre_condition_check_compdata, compdata));
    size_t jump_back = prog_append_something(sl); // filled immediately.
    *vec_at(sl, jump_back) = prog_get_jmp(pre_condition_check - jump_back);
    size_t loop_end = prog_get_next_index(sl);
    *vec_at(sl, condition_check) = prog_get_if(loop_end - condition_check, *condition);
    // FIXME!!!
    return datum_make_nil();
  }
  if (*i < list_length(source) &&
      datum_is_the_symbol(list_at(source, *i), ":=")) {
    (*i)++;
    datum res = prog_append_expression(sl, source, i, compdata, ext, ctxt);

    if (ctxt->aborted) {
      return (datum){};
    }
    datum names;
    if (datum_is_list(head)) {
      names = datum_copy(head);
    } else {
      names = datum_make_list_of(datum_copy(head));
    }
    compdata_give_names(compdata, &names, ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
    return res;
  }
  if (*i < list_length(source) &&
      datum_is_the_symbol(list_at(source, *i), "=")) {
    (*i)++;
    datum rhs_elements = prog_append_expression(sl, source, i, compdata, ext, ctxt);

    if (ctxt->aborted) {
      return (datum){};
    }
    datum names;
    if (datum_is_list(head)) {
      names = datum_copy(head);
    } else {
      names = datum_make_list_of(datum_copy(head));
    }
    move_values_to_variables(sl, &names, &rhs_elements, compdata, ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
    // FIXME!!!!
    return datum_make_nil();
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
                        datum_make_nil(), 0, 0,
                        met, &routine_compdata);
    }
    datum *args = list_at(source, (*i)++);
    prog_append_yield(sl, datum_make_symbol("plain"),
                      datum_make_nil(), 0,
                      list_length(args), datum_make_nil(), &routine_compdata);
    compdata_give_names(&routine_compdata, args, ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
    prog_append_expression(sl, source, i, &routine_compdata, ext, ctxt);

    if (ctxt->aborted) {
      return (datum){};
    }
    datum errm = datum_make_bytestring("routine guard reached");
    datum polyi = prog_append_put_const(sl, &errm, &routine_compdata);
    prog_append_yield(sl, datum_make_symbol("panic"),
                      polyi, 1, 0,
                      datum_make_nil(), &routine_compdata);
    assert(put_prog_off + 1 == prog_off);
    datum pi = compdata_put(compdata, datum_make_symbol(":anon"));
    *vec_at(sl, put_prog_off) =
        prog_get_put_prog(&pi, prog_get_next_index(sl) - put_prog_off, 2);
    return datum_make_list_of(pi);
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
        recieve_count = datum_get_integer(content_val);
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
        abortf(ctxt, "unknown return tag");
        return (datum){};
      }
    }
    size_t argcnt;
    size_t before = compdata_get_length(compdata);
    datum yielded = prog_append_expression(sl, source, i, compdata, ext, ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
    size_t after = compdata_get_length(compdata);
    argcnt = after - before;
    if(list_length(&yielded) != (int)argcnt) {
      abortf(ctxt, "yield internal error: %s", datum_repr(source));
      return (datum){};
    }
    return prog_append_yield(sl, target, yielded, argcnt, recieve_count, meta, compdata);
  }
  if (datum_is_the_symbol(head, "flat")) {
    datum *arg = list_at(source, (*i)++);
    if (!datum_is_list(arg) || list_length(arg) != 1) {
      abortf(ctxt, "bad: %s\n", datum_repr(arg));
      return (datum){};
    }
    datum *vals = list_at(arg, 0);
    if (!datum_is_list(vals)) {
      abortf(ctxt, "bad: %s\n", datum_repr(vals));
      return (datum){};
    }
    int j = 0;
    vec indices = vec_make(0);
    while (j < list_length(vals)) {
      datum idxs = prog_append_expression(sl, vals, &j, compdata, ext, ctxt);
      vec_extend(&indices, &idxs);

      if (ctxt->aborted) {
        return (datum){};
      }
    }
    return datum_make_list_vec(indices);
  }
  if (datum_is_the_symbol(head, "quote")) {
    datum *val = list_at(source, (*i)++);
    assert(datum_is_list(val));
    vec indices = vec_make(0);
    for (int j = 0; j < list_length(val); ++j) {
      datum idx = prog_append_put_const(sl, list_at(val, j), compdata);
      vec_append(&indices, idx);
    }
    return datum_make_list_vec(indices);
  }
  if (datum_is_list(head) && list_length(head) == 2 &&
      datum_is_the_symbol(list_at(head, 0), "call")) {
    datum *exp = list_at(head, 1);
    return prog_append_apply(sl, exp, compdata, ext, ctxt);
  }
  if (datum_is_list(head)) {
    int before = compdata_get_length(compdata);
    datum *vals = head;
    int j = 0;
    vec indices = vec_make(0);
    while (j < list_length(vals)) {
      datum comp = prog_append_expression(sl, vals, &j, compdata, ext, ctxt);
      vec_extend(&indices, &comp);

      if (ctxt->aborted) {
        return (datum){};
      }
    }
    int after = compdata_get_length(compdata);
    datum index = prog_append_collect(sl, after - before, datum_make_list_vec(indices), compdata);
    return datum_make_list_of(index);
  }
  if (datum_is_constant(head)) {
    datum index = prog_append_put_const(sl, head, compdata);
    return datum_make_list_of(index);
  }
  if (datum_is_symbol(head)) {
    datum index = prog_append_copy(sl, head, compdata, ctxt);
    return datum_make_list_of(index);
  }
  abortf(ctxt, "unexpected datum type in expression");
  return (datum){};
}

LOCAL datum prog_append_apply(vec *sl, datum *s_expr, datum *compdata,
                             extension *ext, context *ctxt) {
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
      ret_count = datum_get_integer(content);
      ++index;
    } else if (datum_is_the_symbol(content, "mut")) {
      mut = true;
      ++index;
    } else if (!target_is_set) {
      target = datum_copy(content);
      target_is_set = true;
      ++index;
    } else {
      abortf(ctxt, "unknown tag");
      return (datum){};
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
        abortf(ctxt, "expected an lvalue");
        return (datum){};
      }
      datum idx = compdata_get_polyindex(compdata, component);
      if (datum_is_nil(&idx)) {
        if (datum_is_nil(&idx)) {
          abortf(ctxt, "function not found: %s\n", datum_repr(component));
          return (datum){};
        }
      }
      vec_append(&indices, idx);
    } else {
      datum xx = prog_append_expression(sl, fns, &fn_index, compdata, ext, ctxt);
      assert(list_length(&xx) == 1);

      if (ctxt->aborted) {
        return (datum){};
      }
      vec_append(&indices, *list_at(&xx, 0));
    }
  }
  int before = compdata_get_length(compdata);
  vec arg_indices = vec_make(0);
  while (index < list_length(s_expr)) {
    datum arg_idx = prog_append_expression(sl, s_expr, &index, compdata, ext, ctxt);
    vec_extend(&arg_indices, &arg_idx);

    if (ctxt->aborted) {
      return (datum){};
    }
  }
  int after = compdata_get_length(compdata);
  size_t arg_count = after - before;
  assert(vec_length(&arg_indices) == arg_count);
  return prog_append_call(sl, capture_size, datum_make_list_vec(indices), !mut, target,
                   arg_count, ret_count, datum_make_list_vec(arg_indices), compdata);
}

EXPORT void prog_append_bytecode(vec *sl, vec *src_sl) {
  for (size_t i = 0; i < prog_get_next_index(src_sl); ++i) {
    datum *ins = vec_at(src_sl, i);
    vec_append(sl, datum_copy(ins));
  }
}

LOCAL datum prog_append_call(vec *sl, size_t capture_size, datum indices,
                            bool pop_one, datum type, int arg_count,
                            int return_count, datum arg_indices,
                            datum *compdata) {
  assert(datum_is_list(&indices));
  for (int i = 0; i < arg_count; ++i) {
    compdata_del(compdata);
  }
  if (pop_one) {
    compdata_del(compdata);
  }
  array res = array_make(return_count);
  for (int i = 0; i < return_count; ++i) {
    datum idx = compdata_put(compdata, datum_make_symbol(":anon"));
    *array_at(&res, i) = idx;
  }
  datum result_indices = datum_make_list(res);
  vec_append(sl, datum_make_list_of(
                     datum_make_symbol(":call"), datum_make_int(capture_size),
                     indices, type,
                     datum_make_int(arg_count), datum_make_int(return_count),
                     arg_indices, result_indices));
  return result_indices;
}

LOCAL datum prog_append_copy(vec *sl, datum *val, datum *compdata,
                             context *ctxt) {
  if (!datum_is_symbol(val)) {
    abortf(ctxt, "expected a symbol in put-var\n");
    return (datum){};
  }
  datum polyindex = compdata_get_polyindex(compdata, val);
  if (datum_is_nil(&polyindex)) {
    abortf(ctxt, "undefined variable: %s\n", datum_get_symbol(val));
    return (datum){};
  }
  datum target_polyindex = compdata_put(compdata, datum_make_symbol(":anon"));
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

LOCAL datum prog_append_yield(vec *sl, datum type, datum yield_indices,
                             size_t count, size_t recieve_count, datum meta,
                             datum *compdata) {
  for (size_t i = 0; i < count; ++i) {
    compdata_del(compdata);
  }
  array res = array_make(recieve_count);
  for (size_t i = 0; i < recieve_count; ++i) {
    datum comp = compdata_put(compdata, datum_make_symbol(":anon"));
    *array_at(&res, i) = comp;
  }
  datum recieve_indices = datum_make_list(res);
  vec_append(sl, datum_make_list_of(datum_make_symbol(":yield"), type,
                                    yield_indices,
                                    recieve_indices, meta));
  return recieve_indices;
}

LOCAL size_t prog_append_something(vec *sl) {
  size_t cur = prog_get_next_index(sl);
  vec_append(sl, datum_make_nil());
  return cur;
}

LOCAL datum prog_append_put_const(vec *sl, datum *val, datum *compdata) {
  datum target_polyindex = compdata_put(compdata, datum_make_symbol(":anon"));
  vec_append(sl, datum_make_list_of(datum_make_symbol(":put-const"),
                                    target_polyindex, datum_copy(val)));
  return target_polyindex;
}

LOCAL datum prog_append_collect(vec *sl, size_t count, datum indices,
                               datum *compdata) {
  for (size_t i = 0; i < count; ++i) {
    compdata_del(compdata);
  }
  datum res = compdata_put(compdata, datum_make_symbol(":anon"));
  vec_append(sl, datum_make_list_of(datum_make_symbol(":collect"),
                                    datum_make_int(count), res, indices));
  return res;
}

EXPORT datum *prog_define_routine(vec *sl, datum name, datum *compdata,
                                      context *ctxt) {
  datum target = compdata_put(compdata, datum_make_symbol(":anon"));
  datum ins = prog_get_put_prog(&target, 100500, 0);
  vec_append(sl, ins);
  datum names = datum_make_list_of(name);
  compdata_give_names(compdata, &names, ctxt);
  if (ctxt->aborted) {
    return NULL;
  }
  return list_at(&ins, 3);
}

EXPORT datum *prog_append_jmp(vec *sl) {
  datum ins = prog_get_jmp(100500);
  size_t offset = prog_get_next_index(sl);
  vec_append(sl, ins);
  return (list_at(vec_at(sl, offset), 1));
}

LOCAL datum prog_get_put_prog(datum *target, ptrdiff_t delta, int capture) {
  return datum_make_list_of(datum_make_symbol(":put-prog"), datum_copy(target),
                            datum_make_int(capture), datum_make_int(delta));
}

EXPORT datum prog_get_jmp(ptrdiff_t delta) {
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

LOCAL datum compdata_put(datum *compdata, datum var) {
  size_t frames = list_length(compdata);
  assert(frames > 0);
  datum *last_frame = list_at(compdata, frames - 1);
  list_append_slow(last_frame, var);
  size_t indices = list_length(last_frame);
  assert(indices > 0);
  return datum_make_list_of(datum_make_int(frames - 1),
                            datum_make_int(indices - 1));
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

LOCAL size_t compdata_get_length(datum *compdata) {
  return list_length(list_get_last(compdata));
}

LOCAL size_t compdata_get_frame_count(datum *compdata) {
  return list_length(compdata);
}

LOCAL void compdata_give_names(datum *compdata, datum *var, context *ctxt) {
  for (int i = 0; i < list_length(var); ++i) {
    compdata_del(compdata);
  }
  for (int i = 0; i < list_length(var); ++i) {
    datum target = compdata_get_polyindex(compdata, list_at(var, i));
    if (!datum_is_nil(&target)) {
      abortf(ctxt, "erorr: redefinition of %s\n", datum_repr(list_at(var, i)));
      return;
    }
    compdata_put(compdata, datum_copy(list_at(var, i)));
  }
}

LOCAL void move_values_to_variables(vec *sl, datum *var, datum *rhs_elements, datum *compdata,
                                    context *ctxt) {
  assert(list_length(var) == list_length(rhs_elements));
  for (int i = 0; i < list_length(var); ++i) {
    int idx = list_length(var) - i - 1;
    datum target = compdata_get_polyindex(compdata, list_at(var, idx));
    if (datum_is_nil(&target)) {
      abortf(ctxt, "error: assignment to undeclared variable %s\n",
             datum_repr(list_at(var, idx)));
      return;
    }
    datum *source = list_at(rhs_elements, i);
    prog_append_move(sl, &target, source, compdata);
  }
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
