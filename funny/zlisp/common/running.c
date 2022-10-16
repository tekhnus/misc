#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <extern.h>

struct routine_0 {
  ptrdiff_t offset;
  datum *state_;
};

struct routine_1 {
  struct routine_0 cur;
  struct routine_1 *par;
};

struct routine_2 {
  struct routine_1 cur;
  struct routine_2 *par;
};

enum prog_type {
  PROG_END,
  PROG_IF,
  PROG_NOP,
  PROG_PUT_CONST,
  PROG_PUT_VAR,
  PROG_CALL,
  PROG_HOST,
  PROG_COLLECT,
  PROG_POP,
  PROG_SET_CLOSURES,
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
      bool call_hat;
      ptrdiff_t call_next;
    };
    struct {
      struct datum *host_instruction;
      ptrdiff_t host_next;
    };
    struct {
      size_t collect_count;
      ptrdiff_t collect_next;
    };
    struct {
      ptrdiff_t pop_next;
    };
    struct {
      ptrdiff_t put_prog_value;
      int put_prog_capture;
      ptrdiff_t put_prog_next;
    };
    struct {
      ptrdiff_t set_closures_prog;
      bool set_closures_hat;
      ptrdiff_t set_closures_next;
    };
    struct {
      bool yield_hat;
      size_t yield_count;
      size_t yield_recieve_count;
      struct datum *yield_meta;
      ptrdiff_t yield_next;
    };
  };
};

#if INTERFACE
typedef struct routine_0 routine_0;
typedef struct routine_1 routine_1;
typedef struct routine_2 routine_2;
typedef struct prog prog;
#endif

EXPORT datum *state_make_builtins() {
  return datum_make_nil();
}

EXPORT datum *make_routine_0_with_empty_state(ptrdiff_t prg) {
  routine_0 r0 = {.offset = prg, .state_ = state_make_builtins()};
  return routine_0_to_datum(r0);
}

EXPORT ptrdiff_t decode_offset_from_routine_0(datum *r0d) {
  routine_0 r0;
  char *err = datum_to_routine_0(&r0, r0d);
  if (err != NULL) {
    fprintf(stderr, "%s\n", err);
    exit(EXIT_FAILURE);
  }
  return r0.offset;
}

EXPORT fdatum routine_run_and_get_value_new(prog_slice sl, datum **r0d,
                                 fdatum (*perform_host_instruction)(datum *,
                                                                    datum *)) {
  routine_0 r0;
  char *err = datum_to_routine_0(&r0, *r0d);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  routine_1 r1 = {.cur = r0, .par = NULL};
  routine_2 r = {.cur = r1, .par = NULL};
  char *s = routine_2_run(sl, &r, perform_host_instruction);
  if (s != NULL) {
    print_backtrace(sl, &r);
    return fdatum_make_panic(s);
  }
  datum *d = state_stack_top(&r.cur.cur.state_);
  *r0d = routine_0_to_datum(r.cur.cur);
  return fdatum_make_ok(d);
}

EXPORT fdatum routine_run_and_get_value(prog_slice sl, datum **ctxt, ptrdiff_t prg,
                                 fdatum (*perform_host_instruction)(datum *,
                                                                    datum *)) {
  routine_0 r0 = {.offset = prg, .state_ = *ctxt};
  routine_1 r1 = {.cur = r0, .par = NULL};
  routine_2 r = {.cur = r1, .par = NULL};
  char *s = routine_2_run(sl, &r, perform_host_instruction);
  if (s != NULL) {
    print_backtrace(sl, &r);
    return fdatum_make_panic(s);
  }
  datum *d = state_stack_top(&r.cur.cur.state_);
  *ctxt = r.cur.cur.state_;
  return fdatum_make_ok(d);
}

LOCAL char *routine_2_run(prog_slice sl, routine_2 *r,
                          fdatum (*perform_host_instruction)(datum *,
                                                             datum *)) {
  for (; datum_to_prog(prog_slice_datum_at(sl, r->cur.cur.offset)).type != PROG_END;) {
    char *err = routine_2_step(sl, r, perform_host_instruction);
    if (err != NULL) {
      return err;
    }
  }
  return NULL;
}

LOCAL void routine_2_push_frame(routine_2 *r, routine_1 sub) {
  routine_2 *cont = malloc(sizeof(routine_2));
  *cont = *r;
  r->cur = sub;
  r->par = cont;
}

LOCAL routine_1 routine_2_pop_frame(routine_2 *r) {
  if (r->par == NULL) {
    fprintf(stderr, "routine_2 has no more frames\n");
    exit(EXIT_FAILURE);
  }
  routine_1 res = r->cur;
  r->cur = r->par->cur;
  return res;
}

LOCAL void routine_1_push_frame(routine_1 *r, routine_0 sub) {
  routine_1 *cont = malloc(sizeof(routine_1));
  *cont = *r;
  r->cur = sub;
  r->par = cont;
}

LOCAL routine_0 routine_1_pop_frame(routine_1 *r) {
  if (r->par == NULL) {
    fprintf(stderr, "routine_1 has no more frames\n");
    exit(EXIT_FAILURE);
  }
  routine_0 res = r->cur;
  *r = *r->par;
  return res;
}

LOCAL datum *routine_0_to_datum(routine_0 r) {
  return datum_make_list_2(datum_make_int(r.offset),
                           datum_make_list_1(r.state_));
}

LOCAL datum *routine_1_to_datum(prog_slice sl, routine_1 r) {
  if (r.par == NULL) {
    return datum_make_list_1(routine_0_to_datum(r.cur));
  }
  return datum_make_list(routine_0_to_datum(r.cur),
                         routine_1_to_datum(sl, *r.par));
}

LOCAL char *datum_to_routine_0(routine_0 *res, datum *fn) {
  if (!(datum_is_list(fn) && list_length(fn) == 2 &&
        datum_is_integer(fn->list_head) &&
        datum_is_list(fn->list_tail->list_head) &&
        list_length(fn->list_tail->list_head) == 1)) {
    return "cannot convert datum to routine-0";
  }
  res->offset = fn->list_head->integer_value;
  res->state_ = fn->list_tail->list_head->list_head;
  return NULL;
}

LOCAL char *datum_to_routine_1(routine_1 *res, prog_slice sl, datum *fns) {
  if (!datum_is_list(fns) || datum_is_nil(fns)) {
    return "cannot convert datum to routine-1";
  }
  char *err = datum_to_routine_0(&res->cur, fns->list_head);
  if (err != NULL) {
    return err;
  }
  if (datum_is_nil(fns->list_tail)) {
    res->par = NULL;
    return NULL;
  }
  res->par = malloc(sizeof(routine_1));
  return datum_to_routine_1(res->par, sl, fns->list_tail);
}

LOCAL char *routine_2_step(prog_slice sl, routine_2 *r,
                           fdatum (*perform_host_instruction)(datum *,
                                                              datum *)) {
  prog prgx = datum_to_prog(prog_slice_datum_at(sl, r->cur.cur.offset));
  prog *prg = &prgx;
  datum **st = &r->cur.cur.state_;
  switch (prg->type) {
  case PROG_CALL: {
    if (!prg->call_hat) {
      break;
    }
    datum *form = state_stack_pop(st);
    if (!datum_is_list(form) || datum_is_nil(form)) {
      return ("a call instruction with a malformed form");
    }
    datum *fn = form->list_head;
    datum *args = form->list_tail;
    routine_1 callee;
    char *err = datum_to_routine_1(&callee, sl, fn);
    if (err != NULL) {
      return err;
    }
    r->cur.cur.offset = prg->call_next;
    routine_2_push_frame(r, callee);
    state_stack_put_all(&r->cur.cur.state_, args);
    prog xxx = datum_to_prog(prog_slice_datum_at(sl, r->cur.cur.offset));
    if (xxx.type != PROG_YIELD) {
      return "not a yield-reciever";
    }
    r->cur.cur.offset = xxx.yield_next;
    return NULL;
  } break;
  case PROG_SET_CLOSURES: {
    if (!prg->set_closures_hat) {
      break;
    }
    datum *clos = datum_make_nil();
    state_stack_put(st, clos);
    routine_1 callee = r->cur;
    callee.cur.offset = prg->set_closures_prog;
    *clos = *routine_1_to_datum(
        sl, callee); // modifying the datum because there is a self-reference:(
    r->cur.cur.offset = prg->set_closures_next;
    return NULL;
  } break;
  case PROG_PUT_PROG: {
    if (prg->put_prog_capture != 2){
      break;
    }
    return "put_prog capture=2 not implemented yet";
  } break;
  case PROG_YIELD: {
    if (!prg->yield_hat) {
      break;
    }
    datum *vals = state_stack_collect(st, prg->yield_count);
    routine_1 fr = routine_2_pop_frame(r);
    datum *conti = routine_1_to_datum(sl, fr);
    state_stack_put_all(st, vals);
    state_stack_put(st, conti);
    return NULL;
  } break;
  default:
    break;
  }
  char *err = routine_1_step(sl, &r->cur, perform_host_instruction);
  return err;
}

LOCAL char *routine_1_step(prog_slice sl, routine_1 *r,
                           fdatum (*perform_host_instruction)(datum *,
                                                              datum *)) {
  prog prgx = datum_to_prog(prog_slice_datum_at(sl, r->cur.offset));
  prog *prg = &prgx;
  
  datum **st = &r->cur.state_;
  switch (prg->type) {
  case PROG_CALL: {
    if (prg->call_hat) {
      break;
    }
    datum *form = state_stack_pop(st);
    if (!datum_is_list(form) || datum_is_nil(form)) {
      return ("a call instruction with a malformed form");
    }
    datum *fn = form->list_head;
    datum *args = form->list_tail;
    routine_0 callee;
    if (datum_is_list(fn) && list_length(fn) == 2 &&
        datum_is_integer(fn->list_head) &&
        datum_is_list(fn->list_tail->list_head) &&
        list_length(fn->list_tail->list_head) == 1) {
      int64_t offset = fn->list_head->integer_value;
      datum *vars = fn->list_tail->list_head->list_head;
      callee.offset = offset;
      callee.state_ = vars;
    } else {
      return ("tried to plain-call a non-routine-0");
    }
    r->cur.offset = prg->call_next;
    routine_1_push_frame(r, callee);
    state_stack_put_all(st, args);
    prog xxx = datum_to_prog(prog_slice_datum_at(sl, r->cur.offset));
    if (xxx.type != PROG_YIELD) {
      return "not a yield-reciever";
    }
    r->cur.offset = xxx.yield_next;
    return NULL;
  } break;
  case PROG_SET_CLOSURES: {
    if (prg->set_closures_hat) {
      break;
    }
    datum *clos = datum_make_list_2(datum_make_int(prg->set_closures_prog),
                                    datum_make_nil());
    state_stack_put(st, clos);
    clos->list_tail->list_head = datum_make_list_1(
        (*st)); // modifying a datum because we need to
                                    // create a circular reference:(
    r->cur.offset = prg->set_closures_next;
    return NULL;
  } break;
  case PROG_PUT_PROG: {
    if (prg->put_prog_capture != 1 && prg->put_prog_capture != 0){
      break;
    }
    if (prg->put_prog_capture == 1) {
      datum *s = *st;
      routine_0 rt = {.offset = prg->put_prog_value, .state_ = s};
      datum *prog = routine_0_to_datum(rt);
      state_stack_put(st, prog);
      r->cur.offset = prg->put_prog_next;
      return NULL;
    }
    routine_0 rt = {.offset = prg->put_prog_value, .state_ = datum_make_nil()};
    datum *prog = routine_0_to_datum(rt);
    state_stack_put(st, prog);
    r->cur.offset = prg->put_prog_next;
    return NULL;
  } break;
  case PROG_YIELD: {
    if (prg->yield_hat) {
      break;
    }
    datum *vals = state_stack_collect(st, prg->yield_count);
    routine_0 fr = routine_1_pop_frame(r);
    datum *conti =
      datum_make_list_2(datum_make_int(fr.offset),
                          datum_make_list_1(fr.state_));
    state_stack_put_all(st, vals);
    state_stack_put(st, conti);
    return NULL;
  } break;
  default:
    break;
  }
  char *err = routine_0_step(sl, &r->cur, perform_host_instruction);
  return err;
}

void print_backtrace(prog_slice sl, routine_2 *r) {
  fprintf(stderr, "=========\n");
  fprintf(stderr, "BACKTRACE\n");
  for (routine_2 *x = r; x != NULL; x = x->par) {
    for (routine_1 *y = &x->cur; y != NULL; y = y->par) {
      routine_0 *z = &y->cur;
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
    }
  }
 
  fprintf(stderr, "=========\n");
}

LOCAL char *routine_0_step(prog_slice sl, routine_0 *r,
                           fdatum (*perform_host_instruction)(datum *,
                                                              datum *)) {
  prog prgx = datum_to_prog(prog_slice_datum_at(sl, r->offset));
  prog *prg = &prgx;
  datum **st = &r->state_;
  switch (prg->type) {
  case PROG_NOP: {
    if (datum_is_list(prg->nop_info) && list_length(prg->nop_info) == 2 &&
        datum_is_symbol(prg->nop_info->list_head)) {
      if (datum_is_the_symbol(prg->nop_info->list_head, "compdata")) {
        datum *compdata = prg->nop_info->list_tail->list_head;
        if (list_length(compdata) != list_length(r->state_)) {
          return "compdata mismatch";
        }
      }
    }
    if (datum_is_the_symbol(prg->nop_info, "recieve")) {
      return "nop-reciever";
    }
    r->offset = prg->nop_next;
    return NULL;
  } break;
  case PROG_IF: {
    datum *v = state_stack_pop(st);
    if (!datum_is_nil(v)) {
      r->offset = prg->if_true;
    } else {
      r->offset = prg->if_false;
    }
    return NULL;
  } break;
  case PROG_PUT_CONST: {
    state_stack_put(st, prg->put_const_value);
    r->offset = prg->put_const_next;
    return NULL;
  } break;
  case PROG_PUT_VAR: {
    fdatum er = state_stack_at(*st, prg->put_var_offset);
    if (fdatum_is_panic(er)) {
      return (er.panic_message);
    }
    state_stack_put(st, er.ok_value);
    r->offset = prg->put_var_next;
    return NULL;
  } break;
  case PROG_POP: {
    state_stack_pop(st);
    r->offset = prg->pop_next;
    return NULL;
  } break;
  case PROG_HOST: {
    datum *name = prg->host_instruction;
    datum *arg = state_stack_pop(st);
    fdatum res = perform_host_instruction(name, arg);
    if (fdatum_is_panic(res)) {
      return (res.panic_message);
    }
    state_stack_put(st, res.ok_value);
    r->offset = prg->host_next;
    return NULL;
  } break;
  case PROG_COLLECT: {
    datum *form = state_stack_collect(st, prg->collect_count);
    state_stack_put(st, form);
    r->offset = prg->collect_next;
    return NULL;
  } break;
  default:
    break;
  }
  return ("unhandled instruction type");
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
    res.call_hat = list_at(d, 1)->integer_value;
    res.call_next = list_at(d, 2)->integer_value;
  } else if (!strcmp(opsym, ":host")) {
    res.type = PROG_HOST;
    res.host_instruction = list_at(d, 1);
    res.host_next = (list_at(d, 2)->integer_value);
  } else if (!strcmp(opsym, ":collect")) {
    res.type = PROG_COLLECT;
    res.collect_count = list_at(d, 1)->integer_value;
    res.collect_next = list_at(d, 2)->integer_value;
  } else if (!strcmp(opsym, ":pop")) {
    res.type = PROG_POP;
    res.pop_next = (list_at(d, 1)->integer_value);
  } else if (!strcmp(opsym, ":set-closures")) {
    res.type = PROG_SET_CLOSURES;
    res.set_closures_prog = (list_at(d, 1)->integer_value);
    res.set_closures_hat = list_at(d, 2)->integer_value;
    res.set_closures_next = (list_at(d, 3)->integer_value);
  } else if (!strcmp(opsym, ":put-prog")) {
    res.type = PROG_PUT_PROG;
    res.put_prog_value = (list_at(d, 1)->integer_value);
    res.put_prog_capture = list_at(d, 2)->integer_value;
    res.put_prog_next = (list_at(d, 3)->integer_value);
  } else if (!strcmp(opsym, ":yield")) {
    res.type = PROG_YIELD;
    res.yield_hat = list_at(d, 1)->integer_value;
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

LOCAL fdatum state_stack_at(datum *ns, int offset) {
  datum *entry = list_at(ns, offset);
  return fdatum_make_ok(entry);
}

LOCAL void state_stack_put(datum **ns, datum *value) {
  *ns = datum_make_list(value, (*ns));
}

LOCAL void state_stack_put_all(datum **ns, datum *list) {
  if (!datum_is_list(list)) {
    fprintf(stderr, "put_all expected a list\n");
    exit(EXIT_FAILURE);
  }
  for (datum *rest = list; !datum_is_nil(rest); rest = rest->list_tail) {
    state_stack_put(ns, rest->list_head);
  }
}

LOCAL datum *state_stack_pop(datum **s) {
  datum *res = list_at(*s, 0);
  *s = list_tail(*s);
  return res;
}

LOCAL datum *state_stack_top(datum **s) {
  return list_at(*s, 0);
}

LOCAL datum *state_stack_collect(datum **s, size_t count) {
  datum *form = datum_make_nil();
  for (size_t i = 0; i < count; ++i) {
    datum *arg = state_stack_pop(s);
    form = datum_make_list(arg, form);
  }
  return form;
}
