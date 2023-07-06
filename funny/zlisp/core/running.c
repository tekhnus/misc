typedef struct prog prog;
typedef struct routine routine;

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <types.h>
#include <zlisp/common.h>
#include <running.h>

enum prog_type {
  PROG_IF,
  PROG_JMP,
  PROG_PUT_CONST,
  PROG_COPY,
  PROG_MOVE,
  PROG_CALL,
  PROG_COLLECT,
  PROG_PUT_PROG,
  PROG_YIELD,
};

struct prog {
  enum prog_type type;
  union {
    struct {
      datum *if_index;
      ptrdiff_t if_false;
    };
    ptrdiff_t jmp_next;
    struct {
      datum *put_const_target;
      struct datum *put_const_value;
    };
    struct {
      datum *copy_target;
      datum *copy_offset;
    };
    struct {
      datum *move_target;
      datum *move_offset;
    };
    struct {
      struct datum *call_indices;
      size_t call_capture_count;
      struct datum *call_type;
      struct datum *call_arg_indices;
      struct datum *call_result_indices;
    };
    struct {
      struct datum *collect_indices;
      struct datum *collect_target;
    };
    struct {
      datum *put_prog_target;
      int put_prog_capture;
      ptrdiff_t put_prog_next;
    };
    struct {
      struct datum *yield_type;
      struct datum *yield_val_index;
      struct datum *yield_recieve_indices;
      struct datum *yield_meta;
    };
  };
};

struct frame {
  array *state;
  int type_id;
  int parent_type_id;
};

struct routine {
  struct frame frames[10];
  size_t cnt;
};

EXPORT result routine_run(vec *sl, datum *r, datum args, context *ctxt) {
  routine rt = get_routine_from_datum(r, ctxt);
  if (ctxt->aborted) {
    return (result){};
  }
  return routine_run_impl(sl, &rt, args, ctxt);
}

EXPORT datum *result_get_value(result *r, context *ctxt) {
  datum *type = &r->type;
  if (!datum_is_the_symbol(type, "halt")) {
    abortf(ctxt, "expected halt, got %s", datum_repr(type));
    return NULL;
  }
  return &r->value;
}

LOCAL result routine_run_impl(vec *sl, routine *r, datum args, context *ctxt) {
  ptrdiff_t *ro = routine_offset(r);
  for (;;) {
    prog prg = datum_to_prog(vec_at(sl, *ro), ctxt);
    if (ctxt->aborted) {
      print_frame(sl, r);
      return (result){};
    }
    if (prg.type == PROG_CALL) {
      datum *recieve_type = prg.call_type;
      routine rt = make_routine_from_indices(r, prg.call_capture_count,
                                             prg.call_indices, ctxt);
      if (ctxt->aborted) {
        print_frame(sl, r);
        return (result){};
      }
      bool good = true;
      for (size_t i = 0; i < routine_get_count(&rt); ++i) {
        if ((i == 0 && -1 != (rt.frames[0].parent_type_id)) ||
            (i > 0 &&
             (rt.frames[i].parent_type_id != rt.frames[i - 1].type_id))) {
          good = false;
          break;
        }
      }
      if (!good) {
        abortf(ctxt, "wrong call, frame types are wrong\n");
        print_frame(sl, r);
        return (result){};
      }
      result err = routine_run_impl(sl, &rt, args, ctxt);
      if (ctxt->aborted) {
        print_frame(sl, r);
        return (result){};
      }
      datum *yield_type = &err.type;
      if (!datum_eq(recieve_type, yield_type)) {
        if (datum_is_the_symbol(yield_type, "panic")) {
          print_frame(sl, r);
        }
        return err;
      }
      datum *argz = &err.value;
      size_t ret_cnt = list_length(prg.call_result_indices);
      if (ret_cnt != (long unsigned int)list_length(argz)) {
        abortf(ctxt, "call count and yield count are not equal\n");
        print_frame(sl, r);
        return (result){};
      }

      datum result_indices = datum_copy(prg.call_result_indices);
      state_stack_set_many_2(r, result_indices, *argz, ctxt);
      if (ctxt->aborted) {
        print_frame(sl, r);
        return (result){};
      }
      *ro += 1;
      goto body;
    }
    if (prg.type == PROG_YIELD) {
      if (list_length(&args) != list_length(prg.yield_recieve_indices)) {
        abortf(ctxt,
               "recieved incorrect number of arguments: expected %zu, got %d",
               list_length(prg.yield_recieve_indices), list_length(&args));
        print_frame(sl, r);
        return (result){};
      }
      state_stack_set_many_2(r, datum_copy(prg.yield_recieve_indices), args, ctxt);
      if (ctxt->aborted) {
        print_frame(sl, r);
        return (result){};
      }
      *ro += 1;
      goto body;
    }
  body:
    if (true) {
    }
    for (;;) {
      if (*ro >= (ptrdiff_t)vec_length(sl)) {
        abortf(ctxt, "jumped out of bounds\n");
        print_frame(sl, r);
        return (result){};
      }
      prg = datum_to_prog(vec_at(sl, *ro), ctxt);
      if (ctxt->aborted) {
        print_frame(sl, r);
        return (result){};
      }
      if (prg.type == PROG_YIELD) {
        datum first_index = datum_copy(prg.yield_val_index);
        size_t cnt = list_length(prg.yield_val_index);
        datum res =
            state_stack_invalidate_many_2(r, cnt, first_index, ctxt);
        if (ctxt->aborted) {
          print_frame(sl, r);
          return (result){};
        }
        if (datum_is_the_symbol(prg.yield_type, "panic")) {
          print_frame(sl, r);
        }
        return (result){datum_copy(prg.yield_type), res};
      }
      if (prg.type == PROG_CALL) {
        datum arg_indices = datum_copy(prg.call_arg_indices);
        size_t argcnt = list_length(&arg_indices);
        args =
            state_stack_invalidate_many_2(r, argcnt, arg_indices, ctxt);
        if (ctxt->aborted) {
          print_frame(sl, r);
          return (result){};
        }
        break;
      }
      if (prg.type == PROG_PUT_PROG) {
        size_t put_prog_value = *ro + 1;
        datum prog_ptr =
            routine_make(put_prog_value, prg.put_prog_capture ? r : NULL);
        state_stack_set(r, prg.put_prog_target, prog_ptr, ctxt);
        if (ctxt->aborted) {
          print_frame(sl, r);
          return (result){};
        }
        *ro += prg.put_prog_next;
        continue;
      }
      if (prg.type == PROG_JMP) {
        *ro += prg.jmp_next;
        continue;
      }
      if (prg.type == PROG_IF) {
        datum v = state_stack_invalidate(r, *prg.if_index, ctxt);
        if (ctxt->aborted) {
          print_frame(sl, r);
          return (result){};
        }
        if (!datum_is_nil(&v)) {
          *ro += 1;
        } else {
          *ro += prg.if_false;
        }
        continue;
      }
      if (prg.type == PROG_PUT_CONST) {
        state_stack_set(r, prg.put_const_target,
                        datum_copy(prg.put_const_value), ctxt);
        if (ctxt->aborted) {
          print_frame(sl, r);
          return (result){};
        }
        *ro += 1;
        continue;
      }
      if (prg.type == PROG_COPY) {
        datum *er = state_stack_at(r, prg.copy_offset, ctxt);
        if (ctxt->aborted) {
          print_frame(sl, r);
          return (result){};
        }
        state_stack_set(r, prg.copy_target, datum_copy(er), ctxt);
        if (ctxt->aborted) {
          print_frame(sl, r);
          return (result){};
        }
        *ro += 1;
        continue;
      }
      if (prg.type == PROG_MOVE) {
        datum er = state_stack_invalidate(r, datum_copy(prg.move_offset), ctxt);
        if (ctxt->aborted) {
          print_frame(sl, r);
          return (result){};
        }
        state_stack_set(r, prg.move_target, er, ctxt);
        if (ctxt->aborted) {
          print_frame(sl, r);
          return (result){};
        }
        *ro += 1;
        continue;
      }
      if (prg.type == PROG_COLLECT) {
        datum indices = datum_copy(prg.collect_indices);
        size_t cnt = list_length(&indices);
        datum form = state_stack_invalidate_many_2(r, cnt,
                                                 indices, ctxt);
        if (ctxt->aborted) {
          print_frame(sl, r);
          return (result){};
        }
        state_stack_set(r, prg.collect_target, form, ctxt);
        if (ctxt->aborted) {
          print_frame(sl, r);
          return (result){};
        }
        *ro += 1;
        continue;
      }
      abortf(ctxt, "unhandled instruction type\n");
      print_frame(sl, r);
      return (result){};
    }
  }
  abortf(ctxt, "unreachable");
  print_frame(sl, r);
  return (result){};
}

LOCAL routine make_routine_from_indices(routine *r, size_t capture_count,
                                        datum *call_indices, context *ctxt) {
  routine rt = routine_get_prefix(r, capture_count + 1);
  for (int i = 0; i < list_length(call_indices); ++i) {
    datum *x = state_stack_at(r, list_at(call_indices, i), ctxt);
    if (ctxt->aborted) {
      return (routine){};
    }
    routine nr = get_routine_from_datum(x, ctxt);
    if (ctxt->aborted) {
      return (routine){};
    }
    routine_merge(&rt, &nr);
  }
  return rt;
}

LOCAL routine routine_get_prefix(routine *r, size_t capture_count) {
  routine rt;
  rt.cnt = 0;
  assert(capture_count <= r->cnt);
  for (size_t i = 0; i < capture_count; ++i) {
    rt.frames[i] = r->frames[i];
  }
  rt.cnt = capture_count;
  return rt;
}

LOCAL void routine_merge(routine *r, routine *rt_tail) {
  assert(r->cnt > 0);
  // We chop off the program counter.
  --r->cnt;
  for (size_t j = 0; j < routine_get_count(rt_tail); ++j) {
    r->frames[r->cnt++] = rt_tail->frames[j];
  }
}

LOCAL size_t routine_get_count(routine *r) { return r->cnt; }

LOCAL prog datum_to_prog(datum *d, context *ctxt) {
  prog res;
  if (!datum_is_list(d) || datum_is_nil(d) || !datum_is_symbol(list_at(d, 0))) {
    abortf(ctxt, "datum_to_prog panic\n");
    return (prog){};
  }
  char *opsym = datum_get_symbol(list_at(d, 0));
  if (!strcmp(opsym, ":if")) {
    res.type = PROG_IF;
    res.if_index = list_at(d, 1);
    res.if_false = (datum_get_integer(list_at(d, 2)));
  } else if (!strcmp(opsym, ":jmp")) {
    res.type = PROG_JMP;
    res.jmp_next = (datum_get_integer(list_at(d, 1)));
  } else if (!strcmp(opsym, ":put-const")) {
    res.type = PROG_PUT_CONST;
    res.put_const_target = list_at(d, 1);
    res.put_const_value = list_at(d, 2);
  } else if (!strcmp(opsym, ":copy")) {
    res.type = PROG_COPY;
    res.copy_target = list_at(d, 1);
    res.copy_offset = list_at(d, 2);
  } else if (!strcmp(opsym, ":move")) {
    res.type = PROG_MOVE;
    res.move_target = list_at(d, 1);
    res.move_offset = list_at(d, 2);
  } else if (!strcmp(opsym, ":call")) {
    res.type = PROG_CALL;
    res.call_capture_count = datum_get_integer(list_at(d, 1));
    res.call_indices = list_at(d, 2);
    res.call_type = list_at(d, 3);
    res.call_arg_indices = list_at(d, 4);
    res.call_result_indices = list_at(d, 5);
  } else if (!strcmp(opsym, ":collect")) {
    res.type = PROG_COLLECT;
    res.collect_target = list_at(d, 1);
    res.collect_indices = list_at(d, 2);
  } else if (!strcmp(opsym, ":put-prog")) {
    res.type = PROG_PUT_PROG;
    res.put_prog_target = list_at(d, 1);
    res.put_prog_capture = datum_get_integer(list_at(d, 2));
    res.put_prog_next = (datum_get_integer(list_at(d, 3)));
  } else if (!strcmp(opsym, ":yield")) {
    res.type = PROG_YIELD;
    res.yield_type = list_at(d, 1);
    res.yield_val_index = list_at(d, 2);
    res.yield_recieve_indices = (list_at(d, 3));
    res.yield_meta = list_at(d, 4);
  } else {
    abortf(ctxt, "unknown instruction: %s\n", datum_repr(d));
    return (prog){};
  }
  return res;
}

LOCAL void print_frame(vec *sl, routine *r) {
  ptrdiff_t offset = *routine_offset(r);
  for (ptrdiff_t i = offset - 15; i <= offset + 15; ++i) {
    if (i < 0) {
      continue;
    }
    if (i >= (ptrdiff_t)vec_length(sl)) {
      continue;
    }
    if (i == offset) {
      fprintf(stderr, "> ");
    } else {
      fprintf(stderr, "  ");
    }
    fprintf(stderr, "%ld ", i);
    datum *ins = vec_at(sl, i);
    fprintf(stderr, "%-40s\n", datum_repr(ins));
  }
  fprintf(stderr, "**********\n");
}

LOCAL datum *state_stack_at(routine *r, datum *offset, context *ctxt) {
  assert(datum_is_list(offset) && list_length(offset) > 0);
  datum *frame = list_at(offset, 0);
  assert(datum_is_integer(frame));
  if (datum_get_integer(frame) >= (int)routine_get_count(r)) {
    abortf(ctxt, "wrong frame index");
    return NULL;
  }
  struct frame f = r->frames[datum_get_integer(frame)];
  assert(list_length(offset) == 2);
  datum *idx = list_at(offset, 1);
  assert(datum_is_integer(idx));
  array *vars = f.state;
  if ((size_t)datum_get_integer(idx) >= array_length(vars)) {
    abortf(ctxt, "wrong variable index");
    return NULL;
  }
  return array_at(vars, datum_get_integer(idx));
}

LOCAL datum state_stack_set(routine *r, datum *target, datum value,
                            context *ctxt) {
  datum *loc = state_stack_at(r, target, ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  datum res = *loc;
  *loc = value;
  return res;
}

LOCAL datum state_stack_set_many_2(routine *r, datum indices, datum list,
                                 context *ctxt) {
  datum form =
      datum_make_list_vec(vec_make_copies(list_length(&list), datum_make_nil()));
  assert(datum_is_list(&list));
  assert(list_length(&list) == list_length(&indices));
  for (int i = 0; i < list_length(&list); ++i) {
    datum *idx = list_at(&indices, i);
    *list_at(&form, i) = state_stack_set(r, idx, *list_at(&list, i), ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
  }
  return form;
}

LOCAL datum state_stack_invalidate(routine *r, datum polyindex, context *ctxt) {
  return state_stack_set(r, &polyindex, datum_make_symbol(":invalid"), ctxt);
}

LOCAL datum state_stack_invalidate_many_2(routine *r, size_t count,
                                        datum indices, context *ctxt) {
  return state_stack_set_many_2(
      r, indices,
      datum_make_list_vec(vec_make_copies(count, datum_make_symbol(":invalid"))),
      ctxt);
}

EXPORT datum routine_make_topmost(int64_t prg) {
  return routine_make(prg, NULL);
}

LOCAL datum routine_make(ptrdiff_t prg, struct routine *context) {
  assert(context == NULL || routine_get_count(context) > 1);
  int parent_type_id =
      context != NULL
          ? (context->frames[routine_get_count(context) - 2].type_id)
          : -1;
  datum vars_datum = datum_make_frame(
      vec_make_copies(256, datum_make_symbol(":invalid")), prg, parent_type_id);
  datum pc_frame_datum =
      datum_make_frame(vec_make_of(datum_make_int(prg)), -1, prg);
  datum res = datum_make_list_of(vars_datum, pc_frame_datum);
  return res;
}

LOCAL ptrdiff_t *routine_offset(routine *r) {
  assert(routine_get_count(r) > 0);
  struct frame f = r->frames[routine_get_count(r) - 1];
  assert(array_length(f.state) == 1);
  datum *offset_datum = array_at(f.state, 0);
  assert(datum_is_integer(offset_datum));
  return (ptrdiff_t *)datum_get_integer_ptr(offset_datum);
}

LOCAL routine get_routine_from_datum(datum *e, context *ctxt) {
  if (!datum_is_list(e)) {
    abortf(ctxt, "datum is not callable: %s", datum_repr(e));
    return (routine){};
  }
  routine rt;
  rt.cnt = 0;
  for (int i = 0; i < list_length(e); ++i) {
    rt.frames[rt.cnt++] = get_frame_from_datum(list_at(e, i));
  }
  return rt;
}

LOCAL datum datum_make_frame(vec state, int type_id, int parent_type_id) {
  return datum_make_list_of(datum_make_list_vec(state), datum_make_int(type_id),
                            datum_make_int(parent_type_id));
}

LOCAL struct frame get_frame_from_datum(datum *d) {
  assert(datum_is_list(d));
  assert(list_length(d) == 3);
  assert(datum_is_list(list_at(d, 0)));
  assert(datum_is_integer(list_at(d, 1)));
  assert(datum_is_integer(list_at(d, 2)));
  struct frame v;
  v.state = datum_get_array(list_at(d, 0));
  v.type_id = datum_get_integer(list_at(d, 1));
  v.parent_type_id = datum_get_integer(list_at(d, 2));
  return v;
}
