#include <assert.h>
#include <extern.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum prog_type {
  PROG_END,
  PROG_IF,
  PROG_NOP,
  PROG_PUT_CONST,
  PROG_PUT_VAR,
  PROG_CALL,
  PROG_COLLECT,
  PROG_PUT_PROG,
  PROG_YIELD,
};

struct prog {
  enum prog_type type;
  union {
    struct {
      ptrdiff_t if_true;
      ptrdiff_t if_false;
    };
    ptrdiff_t nop_next;
    struct {
      struct datum *put_const_value;
      ptrdiff_t put_const_next;
    };
    struct {
      datum *put_var_offset;
      ptrdiff_t put_var_next;
    };
    struct {
      struct datum *call_indices;
      bool call_pop_one;
      struct datum *call_type;
      size_t call_arg_count;
      size_t call_return_count;
      ptrdiff_t call_next;
    };
    struct {
      size_t collect_count;
      ptrdiff_t collect_next;
    };
    struct {
      size_t uncollect_count;
      ptrdiff_t uncollect_next;
    };
    struct {
      ptrdiff_t put_prog_value;
      int put_prog_capture;
      ptrdiff_t put_prog_next;
    };
    struct {
      struct datum *yield_type;
      size_t yield_count;
      size_t yield_recieve_count;
      struct datum *yield_meta;
      ptrdiff_t yield_next;
    };
  };
};

struct frame {
  vec state;
  datum type_id;
  datum parent_type_id;
};

struct routine {
  struct frame *frames[10];
  size_t cnt;
};

#if INTERFACE
typedef struct prog prog;
typedef struct routine routine;
typedef struct frame frame;
#endif

EXPORT fdatum routine_run_with_handler(vec sl, datum *r0d,
                              fdatum (*yield_handler)(datum *,
                                                      datum *)) {
  routine *r = get_routine_from_datum(r0d);
  datum args = *datum_make_nil();
  fdatum rerr;
  fdatum res = fdatum_make_ok(*datum_make_nil());
  datum current_statement = *datum_make_nil();
  for (;;) {
    rerr = routine_run(sl, r, args);
    if (fdatum_is_panic(rerr)) {
      res = rerr;
      break;
    }
    datum sec = list_pop(&rerr.ok_value);
    datum yield_type = list_pop(&rerr.ok_value);
    if (datum_is_the_symbol(&yield_type, "halt")) {
      res = fdatum_make_ok(sec);
      break;
    }
    if (datum_is_list(&yield_type) && list_length(&yield_type) == 3 && datum_is_the_symbol(list_at(&yield_type, 0), "debugger")) {
      datum *cmd = list_at(&yield_type, 1);
      if (datum_is_the_symbol(cmd, "compdata")) {
        datum *compdata = list_at(&yield_type, 2);
        datum *compdata_shape = compdata_get_shape(compdata);
        routine *rt = r;
        routine *ch;
        while ((ch = get_child(sl, rt)) != NULL) {
          rt = ch;
        }
        datum *state_shape = routine_get_shape(rt);
        if (list_length(compdata_shape) != list_length(state_shape)) {
          fprintf(stderr, "compdata mismatch: %s != %s\n",
                  datum_repr(compdata_shape), datum_repr(state_shape));
          exit(EXIT_FAILURE);
        }
        int len = list_length(compdata_shape);
        if (!datum_eq(list_at(compdata_shape, len - 1),
                      list_at(state_shape, len - 1))) {
          fprintf(stderr, "compdata mismatch: %s != %s\n",
                  datum_repr(compdata_shape), datum_repr(state_shape));
          exit(EXIT_FAILURE);
        }
      } else if (datum_is_the_symbol(cmd, "statement")) {
        current_statement = *list_at(&yield_type, 2);
      } else {
        fprintf(stderr, "unknown debugger cmd\n");
        exit(EXIT_FAILURE);
      }
      args = *datum_make_nil();
      continue;
    }
    if (!datum_is_list(&yield_type) ||
        !datum_is_the_symbol(list_at(&yield_type, 0), "host")) {
      res = fdatum_make_panic("execution stopped at wrong place");
      break;
    }
    datum *name = list_at(&yield_type, 1);
    datum arg = sec;
    res = yield_handler(name, &arg);
    if (fdatum_is_panic(res)) {
      break;
    }
    args = res.ok_value;
  }
  if (fdatum_is_panic(res)) {
    fprintf(stderr, "CURRENT STATEMENT: %s\n", datum_repr(&current_statement));
    print_backtrace(sl, r);
  }
  return res;
}

LOCAL fdatum routine_run(vec sl, routine *r, datum args) {
  bool pass_args = true;
  for (;;) {
    prog prg = datum_to_prog(vec_at(&sl, *routine_offset(r)));
    if (prg.type == PROG_CALL && pass_args) {
      datum *recieve_type = prg.call_type;
      routine *rt = malloc(sizeof(routine));
      rt->cnt = 0;
      for (int i = 0; i < list_length(prg.call_indices); ++i) {
        routine *nr = get_routine_from_datum(state_stack_at(r, list_at(prg.call_indices, i)));
        rt = routine_merge(rt, nr, false);
        if (rt == NULL) {
          return fdatum_make_panic("bad routine merge");
        }
        // The following is a hack to chop of the program counter
        // from the routines:(
        if (list_length(list_at(prg.call_indices, i)) > 1 && i + 1 < list_length(prg.call_indices)) {
          assert(routine_get_count(rt) > 0);
          --rt->cnt;
        }
      }
      assert(datum_is_nil(&rt->frames[0]->parent_type_id));
      for (size_t i = 1; i < routine_get_count(rt); ++i) {
        if(!datum_eq(&rt->frames[i]->parent_type_id, &rt->frames[i - 1]->type_id)) {
          char *buf = malloc(2048);
          buf[0] = '\0';
          for (size_t j = 0; j < routine_get_count(rt); ++j) {
            sprintf(buf, "frame %zu parent %s self %s vars %zu\n", j,
                    datum_repr(&rt->frames[j]->parent_type_id), datum_repr(&rt->frames[j]->type_id),
                    vec_length(&rt->frames[j]->state));
          }
          sprintf(buf, "wrong call, frame types are wrong\n");
          return fdatum_make_panic(buf);
        }
      }
      fdatum err;
      err = routine_run(sl, rt, args);
      pass_args = false;
      if (fdatum_is_panic(err)) {
        return err;
      }
      datum *yield_type = list_at(&err.ok_value, 0);
      if (!datum_eq(recieve_type, yield_type)) {
        return fdatum_make_ok(err.ok_value);
      }
      datum argz = *list_at(&err.ok_value, 1);
      if (prg.call_return_count != (long unsigned int)list_length(&argz)) {
        return fdatum_make_panic("call count and yield count are not equal");
      }

      if (prg.call_pop_one) {
        state_stack_pop(r);
      }
      state_stack_put_all(r, argz);
      *routine_offset(r) = prg.call_next;
      continue;
    }
    if (prg.type == PROG_YIELD && pass_args) {
      if (list_length(&args) != (int)prg.yield_recieve_count) {
        char *err = malloc(256);
        sprintf(err,
                "recieved incorrect number of arguments: expected %zu, got %d",
                prg.yield_recieve_count, list_length(&args));
        return fdatum_make_panic(err);
      }
      state_stack_put_all(r, args);
      pass_args = false;
      *routine_offset(r) = prg.yield_next;
      continue;
    }
    if (pass_args) {
      return fdatum_make_panic("args passed to a wrong instruction");
    }
    if (prg.type == PROG_YIELD) {
      datum res = state_stack_collect(r, prg.yield_count);
      return fdatum_make_ok(*datum_make_list_of(2, prg.yield_type, datum_copy(&res)));
    }
    if (prg.type == PROG_CALL) {
      args = state_stack_collect(r, prg.call_arg_count);
      pass_args = true;
      continue;
    }
    if (prg.type == PROG_PUT_PROG) {
      routine *rt = routine_make_empty(prg.put_prog_value, prg.put_prog_capture ? r : NULL);
      datum prog_ptr = *datum_make_frame(rt);
      state_stack_put(r, prog_ptr);
      *routine_offset(r) = prg.put_prog_next;
      continue;
    }
    if (prg.type == PROG_NOP) {
      *routine_offset(r) = prg.nop_next;
      continue;
    }
    if (prg.type == PROG_IF) {
      datum v = state_stack_pop(r);
      if (!datum_is_nil(&v)) {
        *routine_offset(r) = prg.if_true;
      } else {
        *routine_offset(r) = prg.if_false;
      }
      continue;
    }
    if (prg.type == PROG_PUT_CONST) {
      state_stack_put(r, *datum_copy(prg.put_const_value));
      *routine_offset(r) = prg.put_const_next;
      continue;
    }
    if (prg.type == PROG_PUT_VAR) {
      datum *er = state_stack_at(r, prg.put_var_offset);
      state_stack_put(r, *datum_copy(er));
      *routine_offset(r) = prg.put_var_next;
      continue;
    }
    if (prg.type == PROG_COLLECT) {
      datum form = state_stack_collect(r, prg.collect_count);
      state_stack_put(r, form);
      *routine_offset(r) = prg.collect_next;
      continue;
    }
    return fdatum_make_panic("unhandled instruction type");
  }
  return fdatum_make_panic("unreachable");
}

LOCAL prog datum_to_prog(datum *d) {
  prog res;
  if (!datum_is_list(d) || datum_is_nil(d) || !datum_is_symbol(list_at(d, 0))) {
    fprintf(stderr, "datum_to_prog panic\n");
    exit(EXIT_FAILURE);
  }
  char *opsym = list_at(d, 0)->symbol_value;
  if (!strcmp(opsym, ":end")) {
    res.type = PROG_END;
  } else if (!strcmp(opsym, ":if")) {
    res.type = PROG_IF;
    res.if_true = (list_at(d, 1)->integer_value);
    res.if_false = (list_at(d, 2)->integer_value);
  } else if (!strcmp(opsym, ":nop")) {
    res.type = PROG_NOP;
    res.nop_next = (list_at(d, 1)->integer_value);
  } else if (!strcmp(opsym, ":put-const")) {
    res.type = PROG_PUT_CONST;
    res.put_const_value = list_at(d, 1);
    res.put_const_next = (list_at(d, 2)->integer_value);
  } else if (!strcmp(opsym, ":put-var")) {
    res.type = PROG_PUT_VAR;
    res.put_var_offset = list_at(d, 1);
    res.put_var_next = (list_at(d, 2)->integer_value);
  } else if (!strcmp(opsym, ":call")) {
    res.type = PROG_CALL;
    res.call_indices = list_at(d, 1);
    res.call_pop_one = list_at(d, 2)->integer_value;
    res.call_type = list_at(d, 3);
    res.call_arg_count = list_at(d, 4)->integer_value;
    res.call_return_count = list_at(d, 5)->integer_value;
    res.call_next = list_at(d, 6)->integer_value;
  } else if (!strcmp(opsym, ":collect")) {
    res.type = PROG_COLLECT;
    res.collect_count = list_at(d, 1)->integer_value;
    res.collect_next = list_at(d, 2)->integer_value;
  } else if (!strcmp(opsym, ":put-prog")) {
    res.type = PROG_PUT_PROG;
    res.put_prog_value = (list_at(d, 1)->integer_value);
    res.put_prog_capture = list_at(d, 2)->integer_value;
    res.put_prog_next = (list_at(d, 3)->integer_value);
  } else if (!strcmp(opsym, ":yield")) {
    res.type = PROG_YIELD;
    res.yield_type = list_at(d, 1);
    res.yield_count = list_at(d, 2)->integer_value;
    res.yield_recieve_count = list_at(d, 3)->integer_value;
    res.yield_meta = list_at(d, 4);
    res.yield_next = (list_at(d, 5)->integer_value);
  } else {
    fprintf(stderr, "datum_to_prog incomplete\n");
    exit(EXIT_FAILURE);
  }
  return res;
}

#define BAD_ROUTINE (routine *)1

LOCAL routine *get_child(vec sl, routine *r) {
  prog prg = datum_to_prog(vec_at(&sl, *routine_offset(r)));
  if (prg.type != PROG_CALL) {
    return NULL;
  }
  routine *child = malloc(sizeof(routine));
  child->cnt = 0;
  for (int i = 0; i < list_length(prg.call_indices); ++i) {
    routine *new_child = routine_merge(
                                       child, get_routine_from_datum(state_stack_at(r, list_at(prg.call_indices, i))), true);
    if (new_child == NULL) {
      return BAD_ROUTINE;  // TODO(me)
    }
    child = new_child;
  }
  return child;
}

LOCAL void print_backtrace(vec sl, routine *r) {
  fprintf(stderr, "=========\n");
  fprintf(stderr, "BACKTRACE\n");
  int i = 0;
  routine *z;
  for (z = r; z != NULL && z != BAD_ROUTINE && i < 10; z = get_child(sl, z), ++i) {
    for (ptrdiff_t i = *routine_offset(z) - 15; i < *routine_offset(z) + 3;
         ++i) {
      if (i < 0) {
        continue;
      }
      if (i >= (ptrdiff_t)vec_length(&sl)) {
        break;
      }
      if (i == *routine_offset(z)) {
        fprintf(stderr, "> ");
      } else {
        fprintf(stderr, "  ");
      }
      fprintf(stderr, "%ld ", i);
      datum *ins = vec_at(&sl, i);
      fprintf(stderr, "%-40s\n", datum_repr(ins));
    }
    fprintf(stderr, "**********\n");
    fprintf(stderr, "%zu vars on stack\n", routine_get_stack_size(z));
    fprintf(stderr, "**********\n");
  }
  if (z == BAD_ROUTINE) {
    fprintf(stderr, "BAD ROUTINE AT THE END\n");
  }
  fprintf(stderr, "=========\n");
}

LOCAL routine *frame_to_routine(frame *f) {
  routine *rt = malloc(sizeof(routine));
  rt->cnt = 1;
  rt->frames[0] = f;
  return rt;
}

EXPORT datum *state_stack_at(routine *r, datum *offset) {
  assert(datum_is_list(offset) && list_length(offset) > 0);
  datum *frame = list_at(offset, 0);
  assert(datum_is_integer(frame));
  assert(frame->integer_value < (int)routine_get_count(r));
  struct frame *f = r->frames[frame->integer_value];
  if (list_length(offset) == 1) {
    return datum_make_frame(frame_to_routine(f));
  }
  assert(list_length(offset) == 2);
  datum *idx = list_at(offset, 1);
  assert(datum_is_integer(idx));
  vec vars = f->state;
  assert((size_t)idx->integer_value < vec_length(&vars));
  return vec_at(&vars, idx->integer_value);
}

EXPORT void state_stack_put(routine *r, datum value) {
  vec_append(&r->frames[routine_get_count(r) - 2]->state, value);
}

EXPORT void state_stack_put_all(routine *r, datum list) {
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
  datum form = *datum_make_nil();
  for (size_t i = 0; i < count; ++i) {
    datum arg = state_stack_pop(r);
    list_append(&form, datum_copy(&arg));
  }
  datum res = *datum_make_nil();
  for (size_t i = 0; i < count; ++i) {
    datum x = list_pop(&form);
    list_append(&res, datum_copy(&x));
  }
  return res;
}

LOCAL void routine_copy(routine *dst, routine *src) {
  dst->cnt = src->cnt;
  for (size_t i = 0; i < dst->cnt; ++i) {
    dst->frames[i] = malloc(sizeof(struct frame));
    frame_copy(dst->frames[i], src->frames[i]);
  }
  *routine_offset(dst) = *routine_offset(src);
}

LOCAL void frame_copy(frame *dst, frame *src) {
  vec_copy(&dst->state, &src->state);
  dst->type_id = src->type_id;
  dst->parent_type_id = src->parent_type_id;
}

LOCAL void vec_copy(vec *dst, vec *src) {
  dst->capacity = src->capacity;
  dst->length = src->length;
  dst->begin = malloc(src->capacity * sizeof(datum));
  for (size_t i = 0; i < dst->length; ++i) {
    dst->begin[i] = *datum_copy(vec_at(src, i));
  }
}

LOCAL size_t routine_get_stack_size(routine *r) {
  size_t res = 0;
  for (size_t i = 0; i < routine_get_count(r) - 1; ++i) {
    res += vec_length(&r->frames[i]->state);
  }
  return res;
}

LOCAL size_t routine_get_count(routine *r) {
  return r->cnt;
}

LOCAL datum *routine_get_shape(routine *r) {
  datum *res = datum_make_nil();
  for (size_t i = 0; i < routine_get_count(r) - 1; ++i) {
    list_append(res, datum_make_int(vec_length(&r->frames[i]->state)));
  }
  return res;
}

LOCAL routine *routine_merge(routine *r, routine *rt_tail, bool allow_inexact) {
  assert(routine_get_count(rt_tail) > 0);
  datum *tail_parent_type = &rt_tail->frames[0]->parent_type_id;
  routine *rt = malloc(sizeof(routine));
  rt->cnt = 0;
  for (size_t height = 0; height < routine_get_count(r); ++height) {
    if (datum_is_nil(tail_parent_type) || (rt->cnt > 0 && datum_eq(tail_parent_type, &rt->frames[rt->cnt - 1]->type_id))) {
      break;
    }
    rt->frames[rt->cnt++] = r->frames[height];
  }
  if (routine_get_count(rt) != routine_get_count(r) && !allow_inexact) {
    fprintf(stderr, "chopped a routine: got %zu from %zu\n", routine_get_count(rt), routine_get_count(r));
    return NULL;
  }
  if (datum_is_nil(tail_parent_type)) {
    assert(rt->cnt == 0);
  } else {
    assert(rt->cnt > 0 && datum_eq(&rt->frames[rt->cnt - 1]->type_id, tail_parent_type));
  }
  for (size_t j = 0; j < routine_get_count(rt_tail); ++j) {
    rt->frames[rt->cnt++] = rt_tail->frames[j];
  }
  return rt;
}

EXPORT datum *routine_make(ptrdiff_t prg) {
  routine *r = routine_make_empty(prg, NULL);
  return datum_make_frame(r);
}

LOCAL routine *routine_make_empty(ptrdiff_t prg, routine *context) {
  routine *r = malloc(sizeof(routine));
  r->cnt = 2;
  r->frames[0] = malloc(sizeof(struct frame));
  r->frames[0]->state = vec_make(1024);
  r->frames[0]->type_id = *datum_make_int(prg);
  assert(context == NULL || routine_get_count(context) > 1);
  r->frames[0]->parent_type_id = context != NULL ? *datum_copy(&context->frames[routine_get_count(context) - 2]->type_id) : *datum_make_nil();
  r->frames[1] = malloc(sizeof(struct frame));
  r->frames[1]->state = vec_make(1);
  vec_append(&r->frames[1]->state, *datum_make_int(prg));
  r->frames[1]->type_id = *datum_make_nil();
  r->frames[1]->parent_type_id = *datum_copy(&r->frames[0]->type_id);
  return r;
}

LOCAL ptrdiff_t *routine_offset(routine *r) {
  assert(routine_get_count(r) > 0);
  frame *f = r->frames[routine_get_count(r) - 1];
  assert(vec_length(&f->state) == 1);
  datum *offset_datum = vec_at(&f->state, 0);
  assert(datum_is_integer(offset_datum));
  return &offset_datum->integer_value;
}

LOCAL datum *datum_make_frame(routine *r) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_FRAME;
  e->frame_value = r;
  return e;
}

LOCAL routine *get_routine_from_datum(datum *d) {
  if (!datum_is_frame(d)) {
    fprintf(stderr, "get_routine_from_datum: not a routine\n");
    exit(EXIT_FAILURE);
  }
  return (routine *)d->frame_value;
}

EXPORT datum *datum_copy(datum *d) {
  if (datum_is_frame(d)) {
    routine *fn_r = get_routine_from_datum(d);
    routine *fn_copy = malloc(sizeof(routine));
    routine_copy(fn_copy, fn_r);
    return datum_make_frame(fn_copy);
  }
  if (datum_is_list(d)) {
    datum *e = datum_make_nil();
    for (int i = 0; i < list_length(d); ++i) {
      list_append(e, datum_copy(list_at(d, i)));
    }
    return e;
  }
  return d;
}
