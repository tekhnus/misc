#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <extern.h>

enum prog_type {
  PROG_END,
  PROG_IF,
  PROG_NOP,
  PROG_PUT_CONST,
  PROG_PUT_VAR,
  PROG_CALL,
  PROG_COLLECT,
  PROG_PUT_PROG,
  PROG_RESOLVE,
  PROG_YIELD,
};

struct prog {
  enum prog_type type;
  union {
    struct {
      ptrdiff_t if_true;
      ptrdiff_t if_false;
    };
    struct {
      struct datum *nop_info;
      ptrdiff_t nop_next;
    };
    struct {
      struct datum *put_const_value;
      ptrdiff_t put_const_next;
    };
    struct {
      datum *put_var_offset;
      ptrdiff_t put_var_next;
    };
    struct {
      struct datum *call_fn_index;
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
    ptrdiff_t resolve_next;
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
  ptrdiff_t offset;
  datum *state;
};

struct routine {
  struct frame frames[10];
  size_t cnt;
  int extvars;
};

#if INTERFACE
typedef struct prog prog;
typedef struct routine routine;
#endif

EXPORT datum *routine_make_new(ptrdiff_t prg) {
  routine *r = routine_make_empty(prg);
  return datum_make_frame(r);
}

EXPORT fdatum routine_run_new(prog_slice sl, datum **r0d,
                                 fdatum (*perform_host_instruction)(datum *,
                                                                    datum *)) {
  routine *r = get_routine_from_datum(*r0d);
  datum *args = datum_make_nil();
  datum *result = datum_make_nil();
  for (;;) {
    fdatum rerr = routine_run(sl, r, args);
    if (fdatum_is_panic(rerr)) {
      print_backtrace_new(sl, r);
      return rerr;
    }
    datum *yield_type = list_at(rerr.ok_value, 0);
    if (datum_is_the_symbol(yield_type, "halt")) {
      result = list_at(rerr.ok_value, 1);
      break;
    }
    if (!datum_is_list(yield_type) || !datum_is_the_symbol(list_at(yield_type, 0), "host")) {
      return fdatum_make_panic("execution stopped at wrong place");
    }
    datum *name = list_at(yield_type, 1);
    datum *arg = list_at(rerr.ok_value, 1);
    fdatum res = perform_host_instruction(name, arg);
    if (fdatum_is_panic(res)) {
      return res;
    }
    args = res.ok_value;
  }
  return fdatum_make_ok(result);
}

LOCAL routine *get_routine_from_datum(datum *d) {
  if (!datum_is_frame(d)) {
    fprintf(stderr, "get_routine_from_datum: not a routine\n");
    exit(EXIT_FAILURE);
  }
  return (routine *)d->frame_value;
}

LOCAL fdatum routine_run(prog_slice sl, routine *r, datum *args) {
  for(;;) {
    prog prg = datum_to_prog(prog_slice_datum_at(sl, *routine_offset(r)));
    if (prg.type == PROG_CALL && args != NULL) {
      datum *recieve_type = prg.call_type;
      datum *dchild = state_stack_at_poly(r, prg.call_fn_index);
      routine *child = get_routine_from_datum(dchild);
      fdatum err = routine_run(sl, child, args);
      args = NULL;
      if (fdatum_is_panic(err)) {
        return err;
      }
      datum *yield_type = list_at(err.ok_value, 0);
      if (!datum_eq(recieve_type, yield_type)) {
        return fdatum_make_ok(err.ok_value);
      }
      datum *args = list_at(err.ok_value, 1);
      if (prg.call_return_count != (long unsigned int)list_length(args)) {
        return fdatum_make_panic("call count and yield count are not equal");
      }

      if (prg.call_pop_one) {
        state_stack_pop(r);
      }
      state_stack_put_all(r, args);
      *routine_offset(r) = prg.call_next;
      continue;
    }
    if (prg.type == PROG_YIELD && args != NULL) {
      if (list_length(args) != (int)prg.yield_recieve_count) {
        return fdatum_make_panic("recieved incorrect number of arguments");
      }
      state_stack_put_all(r, args);
      args = NULL;
      *routine_offset(r) = prg.yield_next;
      continue;
    }
    if (args != NULL) {
      return fdatum_make_panic("args passed to a wrong instruction");
    }
    if (prg.type == PROG_YIELD) {
      datum *res = state_stack_collect(r, prg.yield_count);
      return fdatum_make_ok(datum_make_list_2(prg.yield_type, res));
    }
    if (prg.type == PROG_CALL) {
      args = state_stack_collect(r, prg.call_arg_count);
      continue;
    }
    if (prg.type == PROG_PUT_PROG) {
      size_t capture_size;
      if (prg.put_prog_capture == 0) {
        capture_size = 0;
      } else {
        capture_size = routine_get_stack_size(r) + 1;
      }
      routine *rt = routine_make_empty(prg.put_prog_value);
      rt->extvars = capture_size;
      datum *prog_ptr = datum_make_frame(rt);
      state_stack_put(r, prog_ptr);
      *routine_offset(r) = prg.put_prog_next;
      continue;
    }
    if (prg.type == PROG_RESOLVE) {
      // we don't pop immediately because the pointer might want to reference itself
      // (this happens with lambdas).
      datum *fnptr = state_stack_top(r);
      routine *rt_tail = get_routine_from_datum(fnptr);
      routine *rt = routine_merge(r, rt_tail);
      state_stack_pop(r);
      state_stack_put(r, datum_make_frame(rt));
      *routine_offset(r) = prg.resolve_next;
      continue;
    }
    if (prg.type == PROG_NOP) {
      if (datum_is_list(prg.nop_info) && list_length(prg.nop_info) == 2 &&
          datum_is_symbol(prg.nop_info->list_head)) {
        if (datum_is_the_symbol(prg.nop_info->list_head, "compdata")) {
          datum *compdata = prg.nop_info->list_tail->list_head;
          if (compdata_get_top_index(compdata) + 1 != (int)routine_get_stack_size(r)) {
            return fdatum_make_panic("compdata mismatch");
          }
          datum *compdata_shape = compdata_get_shape(compdata);
          datum *state_shape = routine_get_shape(r);
          if (!datum_eq(compdata_shape, state_shape)) {
            fprintf(stderr, "shape mismatch: %s != %s\n", datum_repr(compdata_shape), datum_repr(state_shape));
            exit(EXIT_FAILURE);
          }
        }
      }
      if (datum_is_the_symbol(prg.nop_info, "recieve")) {
        return fdatum_make_panic("nop-reciever");
      }
      *routine_offset(r) = prg.nop_next;
      continue;
    }
    if (prg.type == PROG_IF) {
      datum *v = state_stack_pop(r);
      if (!datum_is_nil(v)) {
        *routine_offset(r) = prg.if_true;
      } else {
        *routine_offset(r) = prg.if_false;
      }
      continue;
    }
    if (prg.type == PROG_PUT_CONST) {
      state_stack_put(r, prg.put_const_value);
      *routine_offset(r) = prg.put_const_next;
      continue;
    }
    if (prg.type == PROG_PUT_VAR) {
      datum *er = state_stack_at_poly(r, prg.put_var_offset);
      state_stack_put(r, datum_copy(er));
      *routine_offset(r) = prg.put_var_next;
      continue;
    }
    if (prg.type == PROG_COLLECT) {
      datum *form = state_stack_collect(r, prg.collect_count);
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
  if (!datum_is_list(d) || datum_is_nil(d) || !datum_is_symbol(d->list_head)) {
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
    res.nop_info = list_at(d, 1);
    res.nop_next = (list_at(d, 2)->integer_value);
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
    res.call_fn_index = list_at(d, 1);
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
  } else if (!strcmp(opsym, ":resolve")) {
    res.type = PROG_RESOLVE;
    res.resolve_next = list_at(d, 1)->integer_value;
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

LOCAL routine *get_child(prog_slice sl, routine *r) {
  prog prg = datum_to_prog(prog_slice_datum_at(sl, *routine_offset(r)));
  if (prg.type != PROG_CALL) {
    return NULL;
  }
  datum *dchild = state_stack_at_poly(r, prg.call_fn_index);
  if (!datum_is_frame(dchild)) {
    fprintf(stderr, "get_child error: not an integer\n");
    exit(EXIT_FAILURE);
  }
  routine *child = get_routine_from_datum(dchild);
  prog child_prg = datum_to_prog(prog_slice_datum_at(sl, *routine_offset(child)));
  if (child_prg.type == PROG_YIELD) {
    // it's not currently being called
    return NULL;
  }
  if (child_prg.type == PROG_CALL) {
    // TODO: this is not correct, fix it!
    return NULL;
  }
  return child;
}

LOCAL void print_backtrace_new(prog_slice sl, routine *r) {
  fprintf(stderr, "=========\n");
  fprintf(stderr, "BACKTRACE\n");
  int i = 0;
  for (routine *z = r; z != NULL && i < 10; z = get_child(sl, r), ++i) {
    for (ptrdiff_t i = *routine_offset(z) - 15; i < *routine_offset(z) + 3; ++i) {
        if (i < 0) {
          continue;
        }
        if (i >= (ptrdiff_t)prog_slice_length(sl)) {
          break;
        }
        if (i == *routine_offset(z)) {
          fprintf(stderr, "> ");
        } else {
          fprintf(stderr, "  ");
        }
        fprintf(stderr, "%ld ", i);
        datum *ins = prog_slice_datum_at(sl, i);
        char *meta = "";
        if (datum_is_the_symbol(ins->list_head, ":nop")) {
          meta = datum_repr(ins->list_tail->list_head);
          ins = datum_make_list_3(datum_make_symbol(":nop"), datum_make_nil(), ins->list_tail->list_tail->list_head);
        }
        fprintf(stderr, "%-40s%s\n", datum_repr(ins), meta);
      }
      fprintf(stderr, "**********\n");
      fprintf(stderr, "%zu vars on stack\n", routine_get_stack_size(z));
      fprintf(stderr, "**********\n");
  }
 
  fprintf(stderr, "=========\n");
}

EXPORT fdatum state_stack_at(routine *r, int offset) {
  assert(r->cnt > 0);
  for (size_t i = 0; i < r->cnt; ++i) {
    if (offset < list_length(r->frames[i].state)) {
      return fdatum_make_ok(list_at(r->frames[i].state,  list_length(r->frames[i].state) - 1 - offset));
    }
    offset -= list_length(r->frames[i].state);
  }
  return fdatum_make_panic("state_stack_at fail");
}

EXPORT datum *state_stack_at_poly(routine *r, datum *offset) {
  assert(datum_is_list(offset) && list_length(offset) == 2);
  datum *frame = list_at(offset, 0);
  datum *idx = list_at(offset, 1);
  assert(datum_is_integer(frame) && datum_is_integer(idx));
  assert(frame->integer_value < (int)r->cnt);
  datum *vars = r->frames[frame->integer_value].state;
  assert(idx->integer_value < list_length(vars));
  return list_at(vars, list_length(vars) - 1 - idx->integer_value);
}

EXPORT void state_stack_put(routine *r, datum *value) {
  assert(r->cnt > 0);
  r->frames[r->cnt - 1].state = datum_make_list(value, r->frames[r->cnt - 1].state);
}

EXPORT void state_stack_put_all(routine *r, datum *list) {
  if (!datum_is_list(list)) {
    fprintf(stderr, "put_all expected a list\n");
    exit(EXIT_FAILURE);
  }
  for (datum *rest = list; !datum_is_nil(rest); rest = rest->list_tail) {
    state_stack_put(r, rest->list_head);
  }
}

EXPORT datum *state_stack_pop(routine *r) {
  assert(r->cnt > 0);
  datum *res = list_at(r->frames[r->cnt - 1].state, 0);
  r->frames[r->cnt - 1].state = list_tail(r->frames[r->cnt - 1].state);
  return res;
}

EXPORT datum *state_stack_top(routine *r) {
  assert(r->cnt > 0);
  return list_at(r->frames[r->cnt - 1].state, 0);
}

EXPORT datum *state_stack_collect(routine *r, size_t count) {
  datum *form = datum_make_nil();
  for (size_t i = 0; i < count; ++i) {
    datum *arg = state_stack_pop(r);
    form = datum_make_list(arg, form);
  }
  return form;
}

LOCAL void routine_copy(routine *dst, routine *src) {
  dst->cnt = src->cnt;
  for (size_t i = 0; i < dst->cnt; ++i) {
    dst->frames[i] = src->frames[i];
  }
  dst->extvars = src->extvars;
}

LOCAL size_t routine_get_stack_size(routine *r) {
  size_t res = 0;
  for (size_t i = 0; i < r->cnt; ++i) {
    res += list_length(r->frames[i].state);
  }
  return res;
}

LOCAL datum *routine_get_shape(routine *r) {
  datum *res = datum_make_nil();
  for (size_t i = 0; i < r->cnt; ++i) {
    res = list_append(res, datum_make_int(list_length(r->frames[i].state)));
  }
  return res;
}

LOCAL routine *routine_merge(routine *r, routine *rt_tail) {
  size_t rest_vars = rt_tail->extvars;
  routine *rt = malloc(sizeof(routine));
  for (size_t i = 0; i < r->cnt && rest_vars > 0; ++i) {
    rt->frames[rt->cnt++] = r->frames[i];
    if (list_length(r->frames[i].state) > (int)rest_vars) {
      datum *cut_state = list_cut(r->frames[i].state, rest_vars);
      rt->frames[rt->cnt - 1].state = cut_state;
    }
    rest_vars -= list_length(rt->frames[rt->cnt - 1].state);
  }
  for (size_t j = 0; j < rt_tail->cnt; ++j) {
    rt->frames[rt->cnt++] = rt_tail->frames[j];
  }
  return rt;
}

LOCAL routine *routine_make_empty(ptrdiff_t prg) {
  routine *r = malloc(sizeof(routine));
  r->frames[0].offset = prg;
  r->frames[0].state = datum_make_nil();
  r->cnt = 1;
  r->extvars = 0;
  return r;
}

LOCAL ptrdiff_t *routine_offset(routine *r) {
  assert(r->cnt > 0);
  return &r->frames[r->cnt - 1].offset;
}

LOCAL datum *list_cut(datum *xs, size_t rest_length) {
  size_t len = list_length(xs);
  if (len < rest_length) {
    return NULL;
  }
  size_t cut_cnt = len - rest_length;
  for (size_t i = 0; i < cut_cnt; ++i) {
    xs = xs->list_tail;
  }
  return xs;
}

LOCAL datum *datum_make_frame(routine *r) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_FRAME;
  e->frame_value = r;
  return e;
}

LOCAL datum *datum_copy(datum *d) {
  if (datum_is_frame(d)) {
    routine *fn_r = get_routine_from_datum(d);
    routine *fn_copy = malloc(sizeof(routine));
    routine_copy(fn_copy, fn_r);
    return datum_make_frame(fn_copy);
  }
  return d;
}
