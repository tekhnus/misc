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
      bool call_pop_one;
      struct datum *call_type;
      size_t call_arg_count;
      size_t call_return_count;
    };
    struct {
      size_t collect_count;
    };
    struct {
      int put_prog_capture;
      ptrdiff_t put_prog_next;
    };
    struct {
      struct datum *yield_type;
      size_t yield_count;
      size_t yield_recieve_count;
      struct datum *yield_meta;
    };
  };
};

struct routine {
  struct frame *frames[10];
  size_t cnt;
};

#if INTERFACE
typedef struct prog prog;
typedef struct routine routine;
#endif

EXPORT result routine_run_with_handler(vec sl, datum *r0d,
                                       fdatum (*yield_handler)(datum *,
                                                               datum *)) {
  routine r = get_routine_from_datum(r0d);
  datum args = datum_make_nil();
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
      if (datum_is_the_symbol(cmd, "compdata")) {
        datum *compdata = list_at(yield_type, 2);
        datum compdata_shape = compdata_get_shape(compdata);
        routine rt = r;
        while (get_child(sl, &rt)) {
        }
        datum state_shape = routine_get_shape(&rt);
        if (list_length(&compdata_shape) != list_length(&state_shape)) {
          fprintf(stderr, "compdata mismatch: %s != %s\n",
                  datum_repr(&compdata_shape), datum_repr(&state_shape));
          exit(EXIT_FAILURE);
        }
        int len = list_length(&compdata_shape);
        if (!datum_eq(list_at(&compdata_shape, len - 1),
                      list_at(&state_shape, len - 1))) {
          fprintf(stderr, "compdata mismatch: %s != %s\n",
                  datum_repr(&compdata_shape), datum_repr(&state_shape));
          exit(EXIT_FAILURE);
        }
      } else if (datum_is_the_symbol(cmd, "statement")) {
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
        datum_make_symbol(":yield"), datum_make_symbol("panic"),
        datum_make_int(1), datum_make_int(31415926), datum_make_nil());
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
        if ((i == 0 && -1 != (rt.frames[0]->parent_type_id)) ||
            (i > 0 &&
             (rt.frames[i]->parent_type_id != rt.frames[i - 1]->type_id))) {
          good = false;
          break;
        }
      }
      if (!good) {
        char bufbeg[2048];
        char *buf = bufbeg;
        buf[0] = '\0';
        for (size_t j = 0; j < routine_get_count(&rt); ++j) {
          buf +=
              sprintf(buf, "frame %zu parent %d self %d vars %zu\n", j,
                      (rt.frames[j]->parent_type_id), (rt.frames[j]->type_id),
                      vec_length(&rt.frames[j]->state));
        }
        buf += sprintf(buf, "wrong call, frame types are wrong\n");
        state_stack_put(r, datum_make_bytestring(bufbeg));
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
        state_stack_put(r, datum_make_bytestring(
                               "call count and yield count are not equal"));
        *routine_offset(r) = -*routine_offset(r);
        goto body;
      }

      if (prg.call_pop_one) {
        state_stack_pop(r);
      }
      state_stack_put_all(r, *argz);
      *routine_offset(r) += 1;
      goto body;
    }
    if (prg.type == PROG_YIELD) {
      if (list_length(&args) != (int)prg.yield_recieve_count) {
        char err[256];
        sprintf(err,
                "recieved incorrect number of arguments: expected %zu, got %d",
                prg.yield_recieve_count, list_length(&args));
        state_stack_put(r, datum_make_bytestring(err));
        *routine_offset(r) = -*routine_offset(r);
        goto body;
      }
      state_stack_put_all(r, args);
      *routine_offset(r) += 1;
      goto body;
    }
  body:
    if (true) {}
    ptrdiff_t prev_offset = *routine_offset(r);
    for (;;) {
      if (*routine_offset(r) >= (ptrdiff_t) vec_length(&sl)) {
        state_stack_put(r, datum_make_bytestring("jumped out of bounds"));
        *routine_offset(r) = -prev_offset;
        continue;
      }
      prev_offset = *routine_offset(r);
      prg = datum_to_prog(instruction_at(&sl, *routine_offset(r)));
      if (prg.type == PROG_YIELD) {
        datum res = state_stack_collect(r, prg.yield_count);
        return (result){datum_copy(prg.yield_type), res};
      }
      if (prg.type == PROG_CALL) {
        args = state_stack_collect(r, prg.call_arg_count);
        break;
      }
      if (prg.type == PROG_PUT_PROG) {
        size_t put_prog_value = *routine_offset(r) + 1;
        datum prog_ptr =
            routine_make(put_prog_value, prg.put_prog_capture ? r : NULL);
        state_stack_put(r, prog_ptr);
        *routine_offset(r) += prg.put_prog_next;
        continue;
      }
      if (prg.type == PROG_JMP) {
        *routine_offset(r) += prg.jmp_next;
        continue;
      }
      if (prg.type == PROG_IF) {
        datum v = state_stack_pop(r);
        if (!datum_is_nil(&v)) {
          *routine_offset(r) += 1;
        } else {
          *routine_offset(r) += prg.if_false;
        }
        continue;
      }
      if (prg.type == PROG_PUT_CONST) {
        state_stack_set(r, prg.put_const_target, datum_copy(prg.put_const_value));
        *routine_offset(r) += 1;
        continue;
      }
      if (prg.type == PROG_COPY) {
        datum *er = state_stack_at(r, prg.copy_offset);
        state_stack_set(r, prg.copy_target, datum_copy(er));
        *routine_offset(r) += 1;
        continue;
      }
      if (prg.type == PROG_MOVE) {
        datum *er = state_stack_at(r, prg.move_offset);
        state_stack_set(r, prg.move_target, *er);
        *er = datum_make_symbol("__guard__");
        datum guard = state_stack_pop(r);
        assert(datum_is_the_symbol(&guard, "__guard__"));
        *routine_offset(r) += 1;
        continue;
      }
      if (prg.type == PROG_COLLECT) {
        datum form = state_stack_collect(r, prg.collect_count);
        state_stack_put(r, form);
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
    res.if_false = (list_at(d, 1)->integer_value);
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
    res.call_pop_one = list_at(d, 3)->integer_value;
    res.call_type = list_at(d, 4);
    res.call_arg_count = list_at(d, 5)->integer_value;
    res.call_return_count = list_at(d, 6)->integer_value;
  } else if (!strcmp(opsym, ":collect")) {
    res.type = PROG_COLLECT;
    res.collect_count = list_at(d, 1)->integer_value;
  } else if (!strcmp(opsym, ":put-prog")) {
    res.type = PROG_PUT_PROG;
    res.put_prog_capture = list_at(d, 1)->integer_value;
    res.put_prog_next = (list_at(d, 2)->integer_value);
  } else if (!strcmp(opsym, ":yield")) {
    res.type = PROG_YIELD;
    res.yield_type = list_at(d, 1);
    res.yield_count = list_at(d, 2)->integer_value;
    res.yield_recieve_count = list_at(d, 3)->integer_value;
    res.yield_meta = list_at(d, 4);
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

LOCAL datum *state_stack_at(routine *r, datum *offset) {
  assert(datum_is_list(offset) && list_length(offset) > 0);
  datum *frame = list_at(offset, 0);
  assert(datum_is_integer(frame));
  assert(frame->integer_value < (int)routine_get_count(r));
  struct frame *f = r->frames[frame->integer_value];
  assert(list_length(offset) == 2);
  datum *idx = list_at(offset, 1);
  assert(datum_is_integer(idx));
  vec vars = f->state;
  assert((size_t)idx->integer_value < vec_length(&vars));
  return vec_at(&vars, idx->integer_value);
}

LOCAL void state_stack_set(routine *r, datum *target, datum value) {
  assert(datum_is_list(target) && list_length(target) == 2);
  datum *frame_index = list_at(target, 0);
  assert(datum_is_integer(frame_index));
  size_t frame_index_ = frame_index->integer_value;
  assert(frame_index_ < routine_get_count(r));
  vec *frame = &r->frames[frame_index_]->state;
  datum *value_index = list_at(target, 1);
  assert(datum_is_integer(value_index));
  size_t value_index_ = value_index->integer_value;
  assert(value_index_ <= vec_length(frame));
  if (value_index_ == vec_length(frame)) {
    vec_append(frame, value);
  } else if (value_index_ < vec_length(frame)) {
    *vec_at(frame, value_index_) = value;
  } else {
    assert(false);
  }
}

LOCAL void state_stack_put(routine *r, datum value) {
  vec_append(&r->frames[routine_get_count(r) - 2]->state, value);
}

LOCAL void state_stack_put_all(routine *r, datum list) {
  if (!datum_is_list(&list)) {
    fprintf(stderr, "put_all expected a list\n");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < list_length(&list); ++i) {
    state_stack_put(r, *list_at(&list, i));
  }
}

LOCAL datum state_stack_pop(routine *r) {
  return vec_pop(&r->frames[routine_get_count(r) - 2]->state);
}

LOCAL datum state_stack_collect(routine *r, size_t count) {
  datum form = datum_make_nil();
  for (size_t i = 0; i < count; ++i) {
    datum arg = state_stack_pop(r);
    list_append(&form, arg);
  }
  datum res = datum_make_nil();
  for (size_t i = 0; i < count; ++i) {
    datum x = list_pop(&form);
    list_append(&res, x);
  }
  return res;
}

LOCAL size_t routine_get_stack_size(routine *r) {
  size_t res = 0;
  for (size_t i = 0; i < routine_get_count(r) - 1; ++i) {
    res += vec_length(&r->frames[i]->state);
  }
  return res;
}

LOCAL size_t routine_get_count(routine *r) { return r->cnt; }

LOCAL datum routine_get_shape(routine *r) {
  datum res = datum_make_nil();
  for (size_t i = 0; i < routine_get_count(r) - 1; ++i) {
    datum ii = datum_make_int(vec_length(&r->frames[i]->state));
    list_append(&res, ii);
  }
  return res;
}

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
  frame vars = {
      .state = vec_make(1024),
      .type_id = prg,
      .parent_type_id =
          context != NULL
              ? (context->frames[routine_get_count(context) - 2]->type_id)
              : -1};
  datum vars_datum = datum_make_frame(vars);
  frame pc_frame = {.state = vec_make_of(1, datum_make_int(prg)),
                    .type_id = -1,
                    .parent_type_id = prg};
  datum pc_frame_datum = datum_make_frame(pc_frame);
  frame exec = {.state = vec_make_of(2, vars_datum, pc_frame_datum),
                .type_id = -1,
                .parent_type_id = prg};
  datum res = datum_make_frame(exec);
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
  frame *f = r->frames[routine_get_count(r) - 1];
  assert(vec_length(&f->state) == 1);
  datum *offset_datum = vec_at(&f->state, 0);
  assert(datum_is_integer(offset_datum));
  return &offset_datum->integer_value;
}

LOCAL routine get_routine_from_datum(datum *e) {
  if (!datum_is_frame(e)) {
    fprintf(stderr, "get_routine_from_datum: not a routine: %s\n",
            datum_repr(e));
    exit(EXIT_FAILURE);
  }
  routine rt;
  rt.cnt = 0;
  frame *cell = &e->frame_value;
  for (;;) {
    if (vec_length(&cell->state) == 1) {
      rt.frames[rt.cnt++] = cell;
      break;
    }
    assert(vec_length(&cell->state) == 2);
    datum *car = vec_at(&cell->state, 0);
    datum *cdr = vec_at(&cell->state, 1);
    assert(datum_is_frame(car));
    rt.frames[rt.cnt++] = &car->frame_value;
    assert(datum_is_frame(cdr));
    cell = &cdr->frame_value;
  }
  return rt;
}
