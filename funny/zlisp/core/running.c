#include <assert.h>
#include <extern.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

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
      bool call_invalidate_function;
      struct datum *call_type;
      size_t call_arg_count;
      size_t call_return_count;
      struct datum *call_arg_index;
    };
    struct {
      size_t collect_count;
      struct datum *collect_top_index;
    };
    struct {
      datum *put_prog_target;
      int put_prog_capture;
      ptrdiff_t put_prog_next;
    };
    struct {
      struct datum *yield_type;
      struct datum *yield_val_index;
      size_t yield_count;
      size_t yield_recieve_count;
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

#if INTERFACE
typedef struct prog prog;
typedef struct routine routine;
#endif

EXPORT result routine_run_with_handler(vec sl, datum *r0d,
                                       datum args,
                                       fdatum (*yield_handler)(datum *,
                                                               datum *)) {
  routine r = get_routine_from_datum(r0d);
  result res;
  datum current_statement = datum_make_nil();
  for (;;) {
    res = routine_run(sl, &r, args);
    datum *sec = &res.value;
    datum *yield_type = &res.type;
    fdatum handler_res = yield_handler(yield_type, sec);
    if (fdatum_is_panic(handler_res) &&
        strcmp(handler_res.panic_message, "<not implemented>")) {
      res = (result){datum_make_symbol("panic"),
                     datum_make_bytestring(handler_res.panic_message)};
      break;
    }
    if (!fdatum_is_panic(handler_res)) {
      args = handler_res.ok_value;
      continue;
    }
    if (datum_is_list(yield_type) && list_length(yield_type) == 3 &&
        datum_is_the_symbol(list_at(yield_type, 0), "debugger")) {
      datum *cmd = list_at(yield_type, 1);
      if (datum_is_the_symbol(cmd, "statement")) {
        current_statement = *list_at(yield_type, 2);
      } else {
        fprintf(stderr, "unknown debugger cmd\n");
        exit(EXIT_FAILURE);
      }
      args = datum_make_nil();
      continue;
    }
    break;
  }
  if (datum_is_the_symbol(&res.type, "panic")) {
    fprintf(stderr, "CURRENT STATEMENT: %s\n", datum_repr(&current_statement));
    print_backtrace(sl, &r);
  }
  if (datum_is_the_symbol(&res.type, "interpreter-panic")) {
    fprintf(stderr, "CURRENT STATEMENT: %s\n", datum_repr(&current_statement));
    print_backtrace(sl, &r);
  }
  return res;
}

LOCAL routine make_routine_from_indices(routine *r, size_t capture_count,
                                        datum *call_indices) {
  routine rt = routine_get_prefix(r, capture_count + 1);
  for (int i = 0; i < list_length(call_indices); ++i) {
    datum *x = state_stack_at(r, list_at(call_indices, i));
    routine nr = get_routine_from_datum(x);
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

datum error_instruction;

LOCAL datum *instruction_at(vec *sl, ptrdiff_t index) {
  if (index < 0) {
    error_instruction = datum_make_list_of(
        datum_make_symbol(":yield"), datum_make_symbol("interpreter-panic"),
        datum_make_nil(), datum_make_int(0), datum_make_int(31415926),
        datum_make_nil());
    return &error_instruction;
  }
  return vec_at(sl, index);
}

LOCAL result routine_run(vec sl, routine *r, datum args) {
  for (;;) {
    prog prg = datum_to_prog(instruction_at(&sl, *routine_offset(r)));
    if (prg.type == PROG_CALL) {
      datum *recieve_type = prg.call_type;
      routine rt = make_routine_from_indices(r, prg.call_capture_count,
                                             prg.call_indices);
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
        char bufbeg[2048];
        char *buf = bufbeg;
        buf[0] = '\0';
        for (size_t j = 0; j < routine_get_count(&rt); ++j) {
          buf += sprintf(buf, "frame %zu parent %d self %d vars %zu\n", j,
                         (rt.frames[j].parent_type_id), (rt.frames[j].type_id),
                         array_length(rt.frames[j].state));
        }
        buf += sprintf(buf, "wrong call, frame types are wrong\n");
        fprintf(stderr, "%s", bufbeg);
        *routine_offset(r) = -*routine_offset(r);
        goto body;
      }
      result err = routine_run(sl, &rt, args);
      datum *yield_type = &err.type;
      if (!datum_eq(recieve_type, yield_type)) {
        return err;
      }
      datum *argz = &err.value;
      if (prg.call_return_count != (long unsigned int)list_length(argz)) {
        fprintf(stderr, "call count and yield count are not equal\n");
        *routine_offset(r) = -*routine_offset(r);
        goto body;
      }

      datum fn_index = datum_copy(prg.call_arg_index);
      if (prg.call_invalidate_function) {
        list_at(&fn_index, 1)->integer_value -= 1;
      }
      state_stack_set_many(r, fn_index, *argz);
      *routine_offset(r) += 1;
      goto body;
    }
    if (prg.type == PROG_YIELD) {
      if (list_length(&args) != (int)prg.yield_recieve_count) {
        fprintf(stderr,
                "recieved incorrect number of arguments: expected %zu, got %d",
                prg.yield_recieve_count, list_length(&args));
        *routine_offset(r) = -*routine_offset(r);
        goto body;
      }
      state_stack_set_many(r, datum_copy(prg.yield_val_index), args);
      *routine_offset(r) += 1;
      goto body;
    }
  body:
    if (true) {
    }
    ptrdiff_t prev_offset = *routine_offset(r);
    for (;;) {
      if (*routine_offset(r) >= (ptrdiff_t)vec_length(&sl)) {
        fprintf(stderr, "jumped out of bounds\n");
        *routine_offset(r) = -prev_offset;
        continue;
      }
      prev_offset = *routine_offset(r);
      prg = datum_to_prog(instruction_at(&sl, *routine_offset(r)));
      if (prg.type == PROG_YIELD) {
        datum first_index = datum_copy(prg.yield_val_index);
        datum res =
            state_stack_invalidate_many(r, prg.yield_count, first_index);
        return (result){datum_copy(prg.yield_type), res};
      }
      if (prg.type == PROG_CALL) {
        datum arg_index = datum_copy(prg.call_arg_index);
        args = state_stack_invalidate_many(r, prg.call_arg_count, arg_index);
        break;
      }
      if (prg.type == PROG_PUT_PROG) {
        size_t put_prog_value = *routine_offset(r) + 1;
        datum prog_ptr =
            routine_make(put_prog_value, prg.put_prog_capture ? r : NULL);
        state_stack_set(r, prg.put_prog_target, prog_ptr);
        *routine_offset(r) += prg.put_prog_next;
        continue;
      }
      if (prg.type == PROG_JMP) {
        *routine_offset(r) += prg.jmp_next;
        continue;
      }
      if (prg.type == PROG_IF) {
        datum v = state_stack_invalidate(r, *prg.if_index);
        if (!datum_is_nil(&v)) {
          *routine_offset(r) += 1;
        } else {
          *routine_offset(r) += prg.if_false;
        }
        continue;
      }
      if (prg.type == PROG_PUT_CONST) {
        state_stack_set(r, prg.put_const_target,
                        datum_copy(prg.put_const_value));
        *routine_offset(r) += 1;
        continue;
      }
      if (prg.type == PROG_COPY) {
        if (!state_stack_has(r, prg.copy_offset)) {
          fprintf(stderr, "wrong copy offset\n");
          *routine_offset(r) = -*routine_offset(r);
          continue;
        }
        datum *er = state_stack_at(r, prg.copy_offset);
        state_stack_set(r, prg.copy_target, datum_copy(er));
        *routine_offset(r) += 1;
        continue;
      }
      if (prg.type == PROG_MOVE) {
        if (!state_stack_has(r, prg.move_offset)) {
          fprintf(stderr, "wrong move offset\n");
          *routine_offset(r) = -*routine_offset(r);
          continue;
        }
        datum er = state_stack_invalidate(r, datum_copy(prg.move_offset));
        state_stack_set(r, prg.move_target, er);
        *routine_offset(r) += 1;
        continue;
      }
      if (prg.type == PROG_COLLECT) {
        datum first_index = datum_copy(prg.collect_top_index);
        datum form =
            state_stack_invalidate_many(r, prg.collect_count, first_index);
        state_stack_set(r, prg.collect_top_index, form);
        *routine_offset(r) += 1;
        continue;
      }
      fprintf(stderr, "unhandled instruction type\n");
      exit(EXIT_FAILURE);
    }
  }
  fprintf(stderr, "unreachable\n");
  exit(EXIT_FAILURE);
}

LOCAL prog datum_to_prog(datum *d) {
  prog res;
  if (!datum_is_list(d) || datum_is_nil(d) || !datum_is_symbol(list_at(d, 0))) {
    fprintf(stderr, "datum_to_prog panic\n");
    exit(EXIT_FAILURE);
  }
  char *opsym = list_at(d, 0)->symbol_value;
  if (!strcmp(opsym, ":if")) {
    res.type = PROG_IF;
    res.if_index = list_at(d, 1);
    res.if_false = (list_at(d, 2)->integer_value);
  } else if (!strcmp(opsym, ":jmp")) {
    res.type = PROG_JMP;
    res.jmp_next = (list_at(d, 1)->integer_value);
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
    res.call_capture_count = list_at(d, 1)->integer_value;
    res.call_indices = list_at(d, 2);
    res.call_invalidate_function = list_at(d, 3)->integer_value;
    res.call_type = list_at(d, 4);
    res.call_arg_count = list_at(d, 5)->integer_value;
    res.call_return_count = list_at(d, 6)->integer_value;
    res.call_arg_index = list_at(d, 7);
  } else if (!strcmp(opsym, ":collect")) {
    res.type = PROG_COLLECT;
    res.collect_count = list_at(d, 1)->integer_value;
    res.collect_top_index = list_at(d, 2);
  } else if (!strcmp(opsym, ":put-prog")) {
    res.type = PROG_PUT_PROG;
    res.put_prog_target = list_at(d, 1);
    res.put_prog_capture = list_at(d, 2)->integer_value;
    res.put_prog_next = (list_at(d, 3)->integer_value);
  } else if (!strcmp(opsym, ":yield")) {
    res.type = PROG_YIELD;
    res.yield_type = list_at(d, 1);
    res.yield_val_index = list_at(d, 2);
    res.yield_count = list_at(d, 3)->integer_value;
    res.yield_recieve_count = list_at(d, 4)->integer_value;
    res.yield_meta = list_at(d, 5);
  } else {
    fprintf(stderr, "unknown instruction: %s\n", datum_repr(d));
    exit(EXIT_FAILURE);
  }
  return res;
}

LOCAL bool get_child(vec sl, routine *r) {
  prog prg = datum_to_prog(instruction_at(&sl, *routine_offset(r)));
  if (prg.type != PROG_CALL) {
    return false;
  }
  routine child =
      make_routine_from_indices(r, prg.call_capture_count, prg.call_indices);
  *r = child;
  return true;
}

LOCAL void print_backtrace(vec sl, routine *r) {
  fprintf(stderr, "=========\n");
  fprintf(stderr, "BACKTRACE\n");
  int i = 0;
  routine z = *r;
  for (; i < 10; ++i) {
    ptrdiff_t offset = *routine_offset(&z);
    if (offset < 0) {
      offset = -offset;
    }
    for (ptrdiff_t i = offset - 15; i <= offset + 3; ++i) {
      if (i < 0) {
        continue;
      }
      if (i >= (ptrdiff_t)vec_length(&sl)) {
        continue;
      }
      if (i == offset) {
        if (*routine_offset(&z) < 0) {
          fprintf(stderr, "! ");
        } else {
          fprintf(stderr, "> ");
        }
      } else {
        fprintf(stderr, "  ");
      }
      fprintf(stderr, "%ld ", i);
      datum *ins = instruction_at(&sl, i);
      fprintf(stderr, "%-40s\n", datum_repr(ins));
    }
    fprintf(stderr, "**********\n");
    if (!get_child(sl, &z)) {
      break;
    }
  }
  fprintf(stderr, "=========\n");
}

LOCAL bool state_stack_has(routine *r, datum *offset) {
  assert(datum_is_list(offset) && list_length(offset) > 0);
  datum *frame = list_at(offset, 0);
  assert(datum_is_integer(frame));
  if (frame->integer_value >= (int)routine_get_count(r)) {
    return false;
  }
  struct frame f = r->frames[frame->integer_value];
  assert(list_length(offset) == 2);
  datum *idx = list_at(offset, 1);
  assert(datum_is_integer(idx));
  array *vars = f.state;
  if ((size_t)idx->integer_value >= array_length(vars)) {
    return false;
  }
  return true;
}

LOCAL datum *state_stack_at(routine *r, datum *offset) {
  assert(datum_is_list(offset) && list_length(offset) > 0);
  datum *frame = list_at(offset, 0);
  assert(datum_is_integer(frame));
  assert(frame->integer_value < (int)routine_get_count(r));
  struct frame f = r->frames[frame->integer_value];
  assert(list_length(offset) == 2);
  datum *idx = list_at(offset, 1);
  assert(datum_is_integer(idx));
  array *vars = f.state;
  assert((size_t)idx->integer_value < array_length(vars));
  return array_at(vars, idx->integer_value);
}

LOCAL void state_stack_set(routine *r, datum *target, datum value) {
  *state_stack_at(r, target) = value;
  return;
}

LOCAL void state_stack_set_many(routine *r, datum idx, datum list) {
  if (!datum_is_list(&list)) {
    fprintf(stderr, "put_all expected a list\n");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < list_length(&list); ++i) {
    state_stack_set(r, &idx, *list_at(&list, i));
    list_at(&idx, 1)->integer_value += 1;
  }
}

LOCAL datum state_stack_invalidate(routine *r, datum polyindex) {
  datum res = *state_stack_at(r, &polyindex);
  *state_stack_at(r, &polyindex) = datum_make_symbol(":invalid");
  return res;
}

LOCAL datum state_stack_invalidate_many(routine *r, size_t count,
                                        datum top_polyindex) {
  datum form = datum_make_list(vec_make_copies(count, datum_make_nil()));
  for (size_t i = 0; i < count; ++i) {
    *list_at(&form, i) = state_stack_invalidate(r, top_polyindex);
    list_at(&top_polyindex, 1)->integer_value += 1;
  }
  return form;
}

LOCAL size_t routine_get_count(routine *r) { return r->cnt; }

LOCAL void routine_merge(routine *r, routine *rt_tail) {
  assert(r->cnt > 0);
  // We chop off the program counter.
  --r->cnt;
  for (size_t j = 0; j < routine_get_count(rt_tail); ++j) {
    r->frames[r->cnt++] = rt_tail->frames[j];
  }
}

EXPORT datum routine_make(ptrdiff_t prg, routine *context) {
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

EXPORT datum *routine_make_alloc(ptrdiff_t prg, routine *context) {
  // This one is for using from lisp.
  datum *res = malloc(sizeof(datum));
  *res = routine_make(prg, context);
  return res;
}

LOCAL ptrdiff_t *routine_offset(routine *r) {
  assert(routine_get_count(r) > 0);
  struct frame f = r->frames[routine_get_count(r) - 1];
  assert(array_length(f.state) == 1);
  datum *offset_datum = array_at(f.state, 0);
  assert(datum_is_integer(offset_datum));
  return (ptrdiff_t *)&offset_datum->integer_value;
}

LOCAL routine get_routine_from_datum(datum *e) {
  if (!datum_is_list(e)) {
    fprintf(stderr, "get_routine_from_datum: not a routine: %s\n",
            datum_repr(e));
    exit(EXIT_FAILURE);
  }
  routine rt;
  rt.cnt = 0;
  for (int i = 0; i < list_length(e); ++i) {
    rt.frames[rt.cnt++] = get_frame_from_datum(list_at(e, i));
  }
  return rt;
}

LOCAL datum datum_make_frame(vec state, int type_id, int parent_type_id) {
  return datum_make_list_of(datum_make_list(state), datum_make_int(type_id),
                            datum_make_int(parent_type_id));
}

LOCAL struct frame get_frame_from_datum(datum *d) {
  assert(datum_is_list(d));
  assert(list_length(d) == 3);
  assert(datum_is_list(list_at(d, 0)));
  assert(datum_is_integer(list_at(d, 1)));
  assert(datum_is_integer(list_at(d, 2)));
  struct frame v;
  v.state = &list_at(d, 0)->list_value;
  v.type_id = list_at(d, 1)->integer_value;
  v.parent_type_id = list_at(d, 2)->integer_value;
  return v;
}
