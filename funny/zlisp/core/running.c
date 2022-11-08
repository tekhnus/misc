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
  PROG_UNCOLLECT,
  PROG_POP,
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
      int put_var_offset;
      ptrdiff_t put_var_next;
    };
    struct {
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
      ptrdiff_t pop_next;
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

struct routine {
  ptrdiff_t offset;
  datum *state;
  struct routine *child;
};

#if INTERFACE
typedef struct prog prog;
typedef struct routine routine;
#endif

EXPORT datum *routine_make_new(ptrdiff_t prg) {
  routine r = {.offset = prg, .state = datum_make_nil(), .child = NULL};
  return routine_to_datum(&r);
}

EXPORT fdatum routine_run_new(prog_slice sl, datum **r0d,
                                 fdatum (*perform_host_instruction)(datum *,
                                                                    datum *)) {
  routine r;
  char *err = datum_to_routine(*r0d, &r);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  for (;;) {
    err = routine_run(sl, &r);
    if (err != NULL) {
      print_backtrace_new(sl, &r);
      return fdatum_make_panic(err);
    }
    routine *top = topmost_routine(&r);
    prog prg = datum_to_prog(prog_slice_datum_at(sl, top->offset));
    if (prg.type == PROG_END) {
      break;
    }
    if (prg.type != PROG_YIELD || !datum_is_the_symbol(prg.yield_type, "host")) {
      return fdatum_make_panic("execution stopped at wrong place");
    }
    //
    //getc(stdin);
    datum *name = prg.yield_meta;
    datum *arg = state_stack_collect(&top->state, prg.yield_count);
    fdatum res = perform_host_instruction(name, arg);
    if (fdatum_is_panic(res)) {
      return res;
    }
    state_stack_put_all(&top->state, res.ok_value);
    top->offset = prg.yield_next;
  }
  *r0d = routine_to_datum(&r);
  routine *top = topmost_routine(&r);
  if (datum_is_nil(top->state)) {
    return fdatum_make_ok(datum_make_nil());
  }
  return fdatum_make_ok(state_stack_top(&top->state));
}

LOCAL routine *topmost_routine(routine *r) {
  if (r == NULL) {
    fprintf(stderr, "a null routinen\n");
    exit(EXIT_FAILURE);
  }
  if (r->child == NULL) {
    return r;
  }
  return topmost_routine(r->child);
}

LOCAL datum *routine_to_datum(routine *r) {
  if (r == NULL) {
    return datum_make_nil();
  }
  return datum_make_list(
                         datum_make_list_2(datum_make_int(r->offset), r->state),
                         routine_to_datum(r->child));
}

LOCAL char *datum_to_routine(datum *d, routine *r) {
  if (!datum_is_list(d) || datum_is_nil(d)) {
    return "not a routine";
  }
  datum *first_frame = list_at(d, 0);
  if (!datum_is_list(first_frame) || list_length(first_frame) != 2 || !datum_is_integer(list_at(first_frame, 0))) {
    // return datum_repr(first_frame);
    return "invalid frame";
  }
  r->offset = list_at(first_frame, 0)->integer_value;
  r->state = list_at(first_frame, 1);
  if (list_length(d) == 1) {
    r->child = NULL;
    return NULL;
  }
  r->child = malloc(sizeof(routine));
  return datum_to_routine(d->list_tail, r->child);
}

LOCAL char *routine_run(prog_slice sl, routine *r) {
  for(;;) {
    prog prg = datum_to_prog(prog_slice_datum_at(sl, r->offset));
    if (r->child != NULL) {
      if (prg.type != PROG_CALL) {
        return "a routine has child, but the instruction is not 'call'";
      }
      datum *recieve_type = prg.call_type;
      char *err = routine_run(sl, r->child);
      if (err != NULL) {
        return err;
      }
      routine *yielding_routine = topmost_routine(r->child);
      prog yield = datum_to_prog(prog_slice_datum_at(sl, yielding_routine->offset));
      if (yield.type == PROG_END) {
        return NULL;
      }
      if (yield.type != PROG_YIELD) {
        return "a child routine stopped not on a yield instruction";
      }
      datum *yield_type = yield.yield_type;
      if (!datum_eq(recieve_type, yield_type)) {
        return NULL;
      }
      if (prg.call_return_count != yield.yield_count) {
        return "call count and yield count are not equal";
      }
      datum *args = state_stack_collect(&yielding_routine->state, yield.yield_count);
      datum *suspended = routine_to_datum(r->child);
      free(r->child);
      r->child = NULL;
      state_stack_put_all(&r->state, args);
      state_stack_put(&r->state, suspended);
      r->offset = prg.call_next;
      continue;
    }
    if (prg.type == PROG_END) {
      return NULL;
    }
    if (prg.type == PROG_YIELD) {
      return NULL;
    }
    if (prg.type == PROG_CALL) {
      datum *form = state_stack_collect(&r->state, prg.call_arg_count + 1);
      if (!datum_is_list(form) || datum_is_nil(form)) {
        return "a call instruction with a malformed form";
      }
      datum *fn = form->list_head;
      datum *args = form->list_tail;
      routine child;
      char *err = datum_to_routine(fn, &child);
      if (err != NULL) {
        return err;
      }
      routine *child_top = topmost_routine(&child);
      prog recieve = datum_to_prog(prog_slice_datum_at(sl, child_top->offset));
      if (recieve.type != PROG_YIELD) {
        return "the routine beging called is not at yield instruction";
      }
      if (prg.call_arg_count != recieve.yield_recieve_count) {
        return "arg count and recieve count are not equal";
      }
      state_stack_put_all(&child_top->state, args);
      child_top->offset = recieve.yield_next;
      r->child = malloc(sizeof(routine));
      *r->child = child;
      continue;
    }
    if (prg.type == PROG_PUT_PROG) {
      routine rt;
      rt.offset = prg.put_prog_value;
      rt.child = NULL;
      if (prg.put_prog_capture == 1) {
        rt.state = r->state;
      } else if (prg.put_prog_capture == 0) {
        rt.state = datum_make_nil();
      } else {
        size_t stack_size_after_put = list_length(r->state) + 1;
        datum *prog_ptr = datum_make_list_2(datum_make_int(prg.put_prog_value), datum_make_int(stack_size_after_put));
        state_stack_put(&r->state, prog_ptr);
        r->offset = prg.put_prog_next;
        continue;
      }
      datum *prog = routine_to_datum(&rt);
      state_stack_put(&r->state, prog);
      r->offset = prg.put_prog_next;
      continue;
    }
    if (prg.type == PROG_RESOLVE) {
      // we don't pop immediately because the pointer might want to reference itself
      // (this happens with lambdas).
      datum *fnptr = state_stack_top(&r->state);
      if (!datum_is_list(fnptr) || list_length(fnptr) != 2 || !datum_is_integer(list_at(fnptr, 0)) || !datum_is_integer(list_at(fnptr, 1))) {
        return "incorrect fnptr";
      }
      size_t off = list_at(fnptr, 0)->integer_value;
      size_t stack_off = list_at(fnptr, 1)->integer_value;
      datum *cut_state = list_cut(r->state, stack_off);
      if (cut_state == NULL) {
        return "list_cut: list is too short";
      }
      routine rt = {.offset = off, .state = cut_state, .child = NULL};
      state_stack_pop(&r->state);
      state_stack_put(&r->state, routine_to_datum(&rt));
      r->offset = prg.resolve_next;
      continue;
    }
    if (prg.type == PROG_NOP) {
      if (datum_is_list(prg.nop_info) && list_length(prg.nop_info) == 2 &&
          datum_is_symbol(prg.nop_info->list_head)) {
        if (datum_is_the_symbol(prg.nop_info->list_head, "compdata")) {
          datum *compdata = prg.nop_info->list_tail->list_head;
          if (list_length(compdata) != list_length(r->state)) {
            return "compdata mismatch";
          }
        }
      }
      if (datum_is_the_symbol(prg.nop_info, "recieve")) {
        return "nop-reciever";
      }
      r->offset = prg.nop_next;
      continue;
    }
    if (prg.type == PROG_IF) {
      datum *v = state_stack_pop(&r->state);
      if (!datum_is_nil(v)) {
        r->offset = prg.if_true;
      } else {
        r->offset = prg.if_false;
      }
      continue;
    }
    if (prg.type == PROG_PUT_CONST) {
      state_stack_put(&r->state, prg.put_const_value);
      r->offset = prg.put_const_next;
      continue;
    }
    if (prg.type == PROG_PUT_VAR) {
      fdatum er = state_stack_at(r->state, prg.put_var_offset);
      if (fdatum_is_panic(er)) {
        return (er.panic_message);
      }
      state_stack_put(&r->state, er.ok_value);
      r->offset = prg.put_var_next;
      continue;
    }
    if (prg.type == PROG_POP) {
      state_stack_pop(&r->state);
      r->offset = prg.pop_next;
      continue;
    }
    if (prg.type == PROG_COLLECT) {
      datum *form = state_stack_collect(&r->state, prg.collect_count);
      state_stack_put(&r->state, form);
      r->offset = prg.collect_next;
      continue;
    }
    if (prg.type == PROG_UNCOLLECT) {
      datum *xs = state_stack_pop(&r->state);
      state_stack_put_all(&r->state, xs);
      r->offset = prg.uncollect_next;
      continue;
    }
    // return datum_repr(prog_slice_datum_at(sl, r->offset));
    return "unhandled instruction type";
  }
  return "unreachable";
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
    res.put_var_offset = list_at(d, 1)->integer_value;
    res.put_var_next = (list_at(d, 2)->integer_value);
  } else if (!strcmp(opsym, ":call")) {
    res.type = PROG_CALL;
    res.call_type = list_at(d, 1);
    res.call_arg_count = list_at(d, 2)->integer_value;
    res.call_return_count = list_at(d, 3)->integer_value;
    res.call_next = list_at(d, 4)->integer_value;
  } else if (!strcmp(opsym, ":collect")) {
    res.type = PROG_COLLECT;
    res.collect_count = list_at(d, 1)->integer_value;
    res.collect_next = list_at(d, 2)->integer_value;
  } else if (!strcmp(opsym, ":uncollect")) {
    res.type = PROG_UNCOLLECT;
    res.uncollect_count = list_at(d, 1)->integer_value;
    res.uncollect_next = list_at(d, 2)->integer_value;
  } else if (!strcmp(opsym, ":pop")) {
    res.type = PROG_POP;
    res.pop_next = (list_at(d, 1)->integer_value);
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

void print_backtrace_new(prog_slice sl, routine *r) {
  fprintf(stderr, "=========\n");
  fprintf(stderr, "BACKTRACE\n");
  for (routine *z = r; z != NULL; z = z->child) {
      for (ptrdiff_t i = z->offset - 15; i < z->offset + 3; ++i) {
        if (i < 0) {
          continue;
        }
        if (i >= (ptrdiff_t)prog_slice_length(sl)) {
          break;
        }
        if (i == z->offset) {
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
      fprintf(stderr, "%d vars on stack\n", list_length(z->state));
      fprintf(stderr, "**********\n");
  }
 
  fprintf(stderr, "=========\n");
}

EXPORT fdatum state_stack_at(datum *ns, int offset) {
  datum *entry = list_at(ns, offset);
  return fdatum_make_ok(entry);
}

EXPORT void state_stack_put(datum **ns, datum *value) {
  *ns = datum_make_list(value, (*ns));
}

EXPORT void state_stack_put_all(datum **ns, datum *list) {
  if (!datum_is_list(list)) {
    fprintf(stderr, "put_all expected a list\n");
    exit(EXIT_FAILURE);
  }
  for (datum *rest = list; !datum_is_nil(rest); rest = rest->list_tail) {
    state_stack_put(ns, rest->list_head);
  }
}

EXPORT datum *state_stack_pop(datum **s) {
  datum *res = list_at(*s, 0);
  *s = list_tail(*s);
  return res;
}

EXPORT datum *state_stack_top(datum **s) {
  return list_at(*s, 0);
}

EXPORT datum *state_stack_collect(datum **s, size_t count) {
  datum *form = datum_make_nil();
  for (size_t i = 0; i < count; ++i) {
    datum *arg = state_stack_pop(s);
    form = datum_make_list(arg, form);
  }
  return form;
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
