#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <extern.h>

struct routine_0 {
  struct prog *prog_;
  struct state *state_;
};

struct routine_1 {
  struct routine_0 cur;
  struct routine_1 *par;
};

struct routine_2 {
  struct routine_1 cur;
  struct routine_2 *par;
};

#if INTERFACE
typedef struct routine_0 routine_0;
typedef struct routine_1 routine_1;
typedef struct routine_2 routine_2;
#endif

EXPORT fdatum routine_run_and_get_value(prog_slice sl, state **ctxt, prog *p,
                                 fdatum (*perform_host_instruction)(datum *,
                                                                    datum *)) {
  routine_0 r0 = {.prog_ = p, .state_ = *ctxt};
  routine_1 r1 = {.cur = r0, .par = NULL};
  routine_2 r = {.cur = r1, .par = NULL};
  char *s = routine_2_run(sl, &r, perform_host_instruction);
  if (s != NULL) {
    return fdatum_make_panic(s);
  }
  datum *d = state_stack_pop(&r.cur.cur.state_);
  *ctxt = r.cur.cur.state_;
  return fdatum_make_ok(d);
}

LOCAL char *routine_2_run(prog_slice sl, routine_2 *r,
                          fdatum (*perform_host_instruction)(datum *,
                                                             datum *)) {
  for (; r->cur.cur.prog_->type != PROG_END;) {
    // printf("%d %s\n", p->type, datum_repr(s->stack));
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

LOCAL datum *routine_0_to_datum(prog_slice sl, routine_0 r) {
  return datum_make_list_2(prog_to_offset(sl, r.prog_),
                           datum_make_list_2(r.state_->vars, r.state_->stack));
}

LOCAL datum *routine_1_to_datum(prog_slice sl, routine_1 r) {
  if (r.par == NULL) {
    return datum_make_list_1(routine_0_to_datum(sl, r.cur));
  }
  return datum_make_list(routine_0_to_datum(sl, r.cur),
                         routine_1_to_datum(sl, *r.par));
}

LOCAL char *datum_to_routine_0(routine_0 *res, prog_slice sl, datum *fn) {
  if (!(datum_is_list(fn) && list_length(fn) == 2 &&
        datum_is_integer(fn->list_head) &&
        datum_is_list(fn->list_tail->list_head) &&
        list_length(fn->list_tail->list_head) == 2)) {
    return "cannot convert datum to routine-0";
  }
  res->prog_ = prog_slice_at(sl, fn->list_head->integer_value);
  res->state_ = state_make(fn->list_tail->list_head->list_head,
                           fn->list_tail->list_head->list_tail->list_head);
  return NULL;
}

LOCAL char *datum_to_routine_1(routine_1 *res, prog_slice sl, datum *fns) {
  if (!datum_is_list(fns) || datum_is_nil(fns)) {
    return "cannot convert datum to routine-1";
  }
  char *err = datum_to_routine_0(&res->cur, sl, fns->list_head);
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
  prog **p = &r->cur.cur.prog_;
  state **st = &r->cur.cur.state_;
  switch ((*p)->type) {
  case PROG_CALL: {
    if (!(*p)->call_hat) {
      break;
    }
    // return fstate_make_panic("disabled ATM");
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
    r->cur.cur.prog_ = r->cur.cur.prog_->call_next;
    routine_2_push_frame(r, callee);
    state_stack_put(&r->cur.cur.state_, args);
    return NULL;
  } break;
  case PROG_SET_CLOSURES: {
    if (!(*p)->set_closures_hat) {
      break;
    }
    datum *clos = datum_make_nil();
    state_set_var(st, (*p)->set_closures_name, clos);
    routine_1 callee = r->cur;
    callee.cur.prog_ = (*p)->set_closures_prog;
    *clos = *routine_1_to_datum(
        sl, callee); // modifying the datum because there is a self-reference:(
    *p = (*p)->set_closures_next;
    return NULL;
  } break;
  case PROG_RETURN: {
    if (!(*p)->return_hat) {
      break;
    }
    datum *result = state_stack_pop(st);
    routine_2_pop_frame(r);
    state_stack_put(st, result);
    return NULL;
  } break;
  case PROG_YIELD: {
    if (!(*p)->yield_hat) {
      break;
    }
    datum *val = state_stack_pop(st);
    *p = (*p)->yield_next;
    routine_1 fr = routine_2_pop_frame(r);
    datum *conti = routine_1_to_datum(sl, fr);
    datum *result = datum_make_list_2(val, conti);
    state_stack_put(st, result);
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
  prog **p = &r->cur.prog_;
  state **st = &r->cur.state_;
  switch ((*p)->type) {
  case PROG_CALL: {
    if ((*p)->call_hat) {
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
        list_length(fn->list_tail->list_head) == 2) {
      int64_t offset = fn->list_head->integer_value;
      datum *vars = fn->list_tail->list_head->list_head;
      datum *stack = fn->list_tail->list_head->list_tail->list_head;
      callee.prog_ = prog_slice_at(sl, offset);
      callee.state_ = state_make(vars, stack);
    } else {
      return ("tried to plain-call a non-routine-0");
    }
    r->cur.prog_ = r->cur.prog_->call_next;
    routine_1_push_frame(r, callee);
    state_stack_put(st, args);
    return NULL;
  } break;
  case PROG_SET_CLOSURES: {
    if ((*p)->set_closures_hat) {
      break;
    }
    datum *clos = datum_make_list_2(prog_to_offset(sl, (*p)->set_closures_prog),
                                    datum_make_nil());
    state_set_var(st, (*p)->set_closures_name, clos);
    clos->list_tail->list_head = datum_make_list_2(
        (*st)->vars, (*st)->stack); // modifying a datum because we need to
                                    // create a circular reference:(
    *p = (*p)->set_closures_next;
    return NULL;
  } break;
  case PROG_RETURN: {
    if ((*p)->return_hat) {
      break;
    }
    datum *result = state_stack_pop(st);
    routine_1_pop_frame(r);
    state_stack_put(st, result);
    return NULL;
  } break;
  case PROG_YIELD: {
    // return fstate_make_panic("disabled ATM");
    if ((*p)->yield_hat) {
      break;
    }
    datum *val = state_stack_pop(st);
    *p = (*p)->yield_next;
    routine_0 fr = routine_1_pop_frame(r);
    datum *conti =
        datum_make_list_2(prog_to_offset(sl, fr.prog_),
                          datum_make_list_2(fr.state_->vars, fr.state_->stack));
    datum *result = datum_make_list_2(val, conti);
    state_stack_put(st, result);
    return NULL;
  } break;
  default:
    break;
  }
  char *err = routine_0_step(&r->cur, perform_host_instruction);
  return err;
}

LOCAL char *routine_0_step(routine_0 *r,
                           fdatum (*perform_host_instruction)(datum *,
                                                              datum *)) {
  prog **p = &r->prog_;
  state **st = &r->state_;
  // routine c = routine_make(*p, s);
  switch ((*p)->type) {
  case PROG_NOP: {
    *p = (*p)->nop_next;
    return NULL;
  } break;
  case PROG_IF: {
    datum *v = state_stack_pop(st);
    if (!datum_is_nil(v)) {
      *p = (*p)->if_true;
    } else {
      *p = (*p)->if_false;
    }
    return NULL;
  } break;
  case PROG_PUT_CONST: {
    state_stack_put(st, (*p)->put_const_value);
    *p = (*p)->put_const_next;
    return NULL;
  } break;
  case PROG_PUT_VAR: {
    fdatum er = state_get_var(*st, (*p)->put_var_value);
    if (fdatum_is_panic(er)) {
      return (er.panic_message);
    }
    state_stack_put(st, er.ok_value);
    *p = (*p)->put_var_next;
    return NULL;
  } break;
  case PROG_POP: {
    datum *v = state_stack_pop(st);
    if (!datum_is_symbol((*p)->pop_var)) {
      return "inappropriate variable name in POP instruction";
    }
    if (!datum_is_the_symbol((*p)->pop_var, ":void")) {
      state_set_var(st, (*p)->pop_var, v);
    }
    *p = (*p)->pop_next;
    return NULL;
  } break;
  case PROG_ARGS: {
    state_stack_new(st);
    *p = (*p)->args_next;
    return NULL;
  } break;
  case PROG_HOST: {
    datum *name = (*p)->host_instruction;
    datum *arg = state_stack_pop(st);
    fdatum res = perform_host_instruction(name, arg);
    if (fdatum_is_panic(res)) {
      return (res.panic_message);
    }
    state_stack_put(st, res.ok_value);
    *p = (*p)->host_next;
    return NULL;
  } break;
  case PROG_COLLECT: {
    datum *form = state_stack_collect(st);
    state_stack_put(st, form);
    *p = (*p)->collect_next;
    return NULL;
  } break;
  case PROG_IMPORT: {
    // fprintf(stderr, "MODULE_END\n");
    datum *pair = state_stack_pop(st);
    if (!datum_is_list(pair) || list_length(pair) != 2) {
      return ("expected a pair after a submodule call");
    }
    datum *fn = pair->list_tail->list_head;
    state *module_state;
    if (datum_is_list(fn) && list_length(fn) == 2 &&
        datum_is_integer(fn->list_head) &&
        datum_is_list(fn->list_tail->list_head) &&
        list_length(fn->list_tail->list_head) == 2) {
      module_state = state_make(fn->list_tail->list_head->list_head,
                                fn->list_tail->list_head->list_tail->list_head);
    } else {
      return ("expected a routine after a submodule call");
    }

    datum *imported_bindings = state_list_vars(module_state);
    for (; !datum_is_nil(imported_bindings);
         imported_bindings = imported_bindings->list_tail) {
      datum *sym = imported_bindings->list_head->list_head;
      datum *val = imported_bindings->list_head->list_tail->list_head;

      state_set_var(st, sym, val);
    }
    *p = (*p)->import_next;
    return NULL;
  } break;
  default:
    break;
  }
  return ("unhandled state type");
}
