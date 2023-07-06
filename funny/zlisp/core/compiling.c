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
                        datum_make_nil(), 0,
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
    compdata_invalidate(compdata, &conditions);
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
    compdata_merge(compdata, &false_compdata, ctxt);
    if (ctxt->aborted) {
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
    compdata_invalidate(compdata, &conditions);
    prog_append_expression(sl, source, i, compdata, ext, ctxt);

    if (ctxt->aborted) {
      return (datum){};
    }
    compdata_merge(compdata, &pre_condition_check_compdata, ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
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
    if(list_length(&res) != list_length(&names)) {
      abortf(ctxt, "wrong assignment count: %s", datum_repr(source));
      return (datum){};
    }
    compdata_give_names(compdata, &res, &names, ctxt);
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
    size_t put_prog_off = prog_get_next_index(sl);
    datum pi = prog_append_put_prog(sl, 2, compdata); // filled below.
    datum routine_compdata = datum_copy(compdata);
    compdata_start_new_section(&routine_compdata);

    size_t prog_off = prog_get_next_index(sl);
    if (datum_is_the_symbol(head, "magically_called_fn")) {
      datum target = datum_make_symbol("plain");
      datum met = datum_make_nil();
      prog_append_yield(sl, target,
                        datum_make_nil(), 0,
                        met, &routine_compdata);
    }
    datum *args = list_at(source, (*i)++);
    datum argindices = prog_append_yield(sl, datum_make_symbol("plain"),
                      datum_make_nil(),
                      list_length(args), datum_make_nil(), &routine_compdata);

    assert(list_length(&argindices) == list_length(args));
    compdata_give_names(&routine_compdata, &argindices, args, ctxt);
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
                      datum_make_list_of(polyi), 0,
                      datum_make_nil(), &routine_compdata);
    assert(put_prog_off + 1 == prog_off);
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
    datum yielded = prog_append_expression(sl, source, i, compdata, ext, ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
    return prog_append_yield(sl, target, yielded, recieve_count, meta, compdata);
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
    datum index = prog_append_collect(sl, datum_make_list_vec(indices), compdata);
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
  vec arg_indices = vec_make(0);
  while (index < list_length(s_expr)) {
    datum arg_idx = prog_append_expression(sl, s_expr, &index, compdata, ext, ctxt);
    vec_extend(&arg_indices, &arg_idx);

    if (ctxt->aborted) {
      return (datum){};
    }
  }
  return prog_append_call(sl, capture_size, datum_make_list_vec(indices), !mut, target,
                   ret_count, datum_make_list_vec(arg_indices), compdata);
}

EXPORT void prog_append_bytecode(vec *sl, vec *src_sl) {
  for (size_t i = 0; i < prog_get_next_index(src_sl); ++i) {
    datum *ins = vec_at(src_sl, i);
    vec_append(sl, datum_copy(ins));
  }
}

LOCAL datum prog_append_call(vec *sl, size_t capture_size, datum indices,
                            bool pop_one, datum type,
                            int return_count, datum arg_indices,
                            datum *compdata) {
  assert(datum_is_list(&indices));
  assert(!datum_is_nil(&indices));
  compdata_invalidate(compdata, &arg_indices);
  if (pop_one) {
    datum last_index = datum_make_list_of(datum_copy(list_get_last(&indices)));
    compdata_invalidate(compdata, &last_index);
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
  datum src = datum_make_list_of(datum_copy(source));
  compdata_invalidate(compdata, &src);
}

LOCAL datum prog_append_yield(vec *sl, datum type, datum yield_indices,
                             size_t recieve_count, datum meta,
                             datum *compdata) {
  compdata_invalidate(compdata, &yield_indices);
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

LOCAL datum prog_append_collect(vec *sl, datum indices,
                               datum *compdata) {
  size_t count = list_length(&indices);
  compdata_invalidate(compdata, &indices);
  datum res = compdata_put(compdata, datum_make_symbol(":anon"));
  assert(count == (size_t)list_length(&indices));
  vec_append(sl, datum_make_list_of(datum_make_symbol(":collect"),
                                    res, indices));
  return res;
}

EXPORT datum *prog_define_routine(vec *sl, datum name, datum *compdata,
                                      context *ctxt) {
  datum target = compdata_put(compdata, datum_make_symbol(":anon"));
  datum ins = prog_get_put_prog(&target, 100500, 0);
  vec_append(sl, ins);
  datum names = datum_make_list_of(name);
  datum targets = datum_make_list_of(target);
  compdata_give_names(compdata, &targets, &names, ctxt);
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

LOCAL datum prog_append_put_prog(vec *sl, int capture, datum *compdata) {
  datum target = compdata_put(compdata, datum_make_symbol(":anon"));
  datum ins = prog_get_put_prog(&target, 100500, capture);
  vec_append(sl, ins);
  return target;
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
  datum compdata = datum_make_nil();
  compdata_start_new_section(&compdata);
  return compdata;
}

LOCAL datum compdata_put(datum *compdata, datum var) {
  size_t frames = list_length(compdata);
  assert(frames > 0);
  datum *last_frame = list_at(compdata, frames - 1);
  size_t indices = list_length(last_frame);
  for (size_t i = 0; i < indices; ++i) {
    datum *item = list_at(last_frame, i);
    if (datum_is_the_symbol(item, ":invalid")) {
      *item = var;
      return datum_make_list_of(datum_make_int(frames - 1),
                              datum_make_int(i));
    }
  }
  list_append_slow(last_frame, var);
  return datum_make_list_of(datum_make_int(frames - 1),
                            datum_make_int(indices));
}

LOCAL void compdata_invalidate(datum *compdata, datum *indices) {
  assert(datum_is_list(indices));
  for (int i = 0; i < list_length(indices); ++i) {
    datum *idx = list_at(indices, i);
    assert(datum_is_list(idx));
    datum *item = compdata_at(compdata, idx);
    if (!datum_is_the_symbol(item, ":anon")) {
      // fprintf(stderr, "invalidating %s\n", datum_repr(item));
      // assert(false);
    }
    *compdata_at(compdata, idx) = datum_make_symbol(":invalid");
  }
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

LOCAL size_t compdata_get_frame_count(datum *compdata) {
  return list_length(compdata);
}

LOCAL datum *compdata_at(datum *compdata, datum *idx) {
  assert(list_length(idx) == 2);
  int64_t i = datum_get_integer(list_at(idx, 0));
  int64_t j = datum_get_integer(list_at(idx, 1));
  datum *frame = list_at(compdata, i);
  return list_at(frame, j);
}

LOCAL void compdata_give_names(datum *compdata, datum *indices, datum *var, context *ctxt) {
  if (ctxt == ctxt + 1) {}
  assert(list_length(indices) == list_length(var));
  for (int i = 0; i < list_length(indices); ++i) {
    datum *vari = list_at(var, i);
    datum *srci = list_at(indices, i);
    *compdata_at(compdata, srci) = datum_copy(vari);
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

LOCAL void compdata_merge(datum *aa, datum *bb, context *ctxt) {
  datum *a = list_get_last(aa);
  datum *b = list_get_last(bb);
  if (list_length(a) > list_length(b)) {
    compdata_merge(bb, aa, ctxt);
    return;
  }
  assert(list_length(a) <= list_length(b));
  for (int i = 0; i < list_length(a); ++i) {
    if (!datum_eq(list_at(a, i), list_at(b, i))) {
      abortf(ctxt, "can't merge compdata, non-equal prefixes: %s %s", datum_repr(a), datum_repr(b));
      return;
    }
  }
  for (int i = list_length(a); i < list_length(b); ++i) {
    if (!datum_is_the_symbol(list_at(b, i), ":invalid")) {
      abortf(ctxt, "can't merge compdata, extra valid items");
      return;
    }
    list_append_slow(a, datum_make_symbol(":invalid"));
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