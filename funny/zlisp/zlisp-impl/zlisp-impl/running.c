#include <zlisp-impl/zlisp-impl.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>


fdatum routine_run_and_get_value(state **ctxt, prog *p, fdatum (*perform_host_instruction)(datum *, datum *)) {
  if (!routine_1_is_null((*ctxt)->parent) || !routine_2_is_null((*ctxt)->hat_parent)) {
    return fdatum_make_panic("non-flat states are not supported here");
  }
  routine_2 r = routine_2_make(p, *ctxt);
  char *s = routine_2_run(&r, perform_host_instruction);
  if (s != NULL) {
    return fdatum_make_panic(s);
  }
  datum *d = state_stack_pop(&r.state_);
  *ctxt = r.state_;
  return fdatum_make_ok(d);
}

LOCAL char *routine_2_run(routine_2 *r, fdatum (*perform_host_instruction)(datum *, datum *)) {
  for (; r->prog_->type != PROG_END; ) {
    // printf("%d %s\n", p->type, datum_repr(s->stack));
    char *err = routine_2_step(r, perform_host_instruction);
    if (err != NULL) {
      return err;
    }
  }
  return NULL;
}

LOCAL char *routine_2_step(routine_2 *r, fdatum (*perform_host_instruction)(datum *, datum *)) {
  prog **p = &r->prog_;
  state **st = &r->state_;
  switch ((*p)->type) {
  case PROG_CALL: {
    if (!(*p)->call_hat) {
      break;
    }
    //return fstate_make_panic("disabled ATM");
    datum *form = state_stack_pop(st);
    if (!datum_is_list(form) || datum_is_nil(form)) {
      return ("a call instruction with a malformed form");
    }
    datum *fn = form->list_head;
    datum *args = form->list_tail;
    if (!datum_is_routine_1(fn)) {
      return ("tried to hat-call a non-routine-1");
    }
    routine_2 parent_cont = routine_2_make((*p)->call_next, *st);
    *p = fn->routine_1_value.prog_;
    *st = fn->routine_1_value.state_;
    state_stack_put(st, args);
    *st = state_change_hat_parent(*st, parent_cont);
    return NULL;
  } break;
  case PROG_SET_CLOSURES: {
    if (!(*p)->set_closures_hat){
      break;
    }
    datum *clos = datum_make_routine_1((*p)->set_closures_prog, NULL);
    *st = state_set_var(*st, (*p)->set_closures_name, clos);
    clos->routine_1_value.state_ = *st;
    *p = (*p)->set_closures_next;
    return NULL;
  } break;
  case PROG_RETURN: {
    if (!(*p)->return_hat) {
      break;
    }
    //return fstate_make_panic("disabled ATM");
    routine_2 yield_to = (*st)->hat_parent;
    if (routine_2_is_null(yield_to)) {
      return ("bad return");
    }
    *st = state_change_hat_parent(*st, routine_2_make_null());
    datum *result = state_stack_pop(st);
    *p = yield_to.prog_;
    *st = yield_to.state_;
    state_stack_put(st, result);
    return NULL;
  } break;
  case PROG_YIELD: {
    if (!(*p)->yield_hat) {
      break;
    }
    //return fstate_make_panic("disabled ATM");
    routine_2 yield_to = (*st)->hat_parent;
    if (routine_2_is_null(yield_to)) {
      return ("bad yield");
    }
    *st = state_change_hat_parent(*st, routine_2_make_null());
    datum *val = state_stack_pop(st);
    datum *conti = datum_make_routine_1((*p)->yield_next, *st);
    datum *result = datum_make_list_2(val, conti);
    *p = yield_to.prog_;
    *st = yield_to.state_;
    state_stack_put(st, result);
    return NULL;
  } break;
  default: break;
  }
  routine_1 cr = routine_1_make(r->prog_, r->state_);
  char *err = routine_1_step(&cr, perform_host_instruction);
  r->prog_ = cr.prog_;
  r->state_ = cr.state_;
  return err;
}

LOCAL char *routine_1_step(routine_1 *r, fdatum (*perform_host_instruction)(datum *, datum *)) {
  prog **p = &r->prog_;
  state **st = &r->state_;
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
    if (!datum_is_routine_0(fn)) {
      return ("tried to plain-call a non-routine-0");
    }
    routine_1 parent_cont = routine_1_make((*p)->call_next, *st);
    *p = fn->routine_0_value.prog_;
    *st = fn->routine_0_value.state_;
    state_stack_put(st, args);
    *st = state_change_plain_parent(*st, parent_cont);
    *st =
      state_change_hat_parent(*st, parent_cont.state_->hat_parent);
    return NULL;
  } break;
  case PROG_SET_CLOSURES: {
    if ((*p)->set_closures_hat){
      break;
    }
    datum *clos = datum_make_routine_0((*p)->set_closures_prog, NULL);
    *st = state_set_var(*st, (*p)->set_closures_name, clos);
    clos->routine_0_value.state_ = *st;
    *p = (*p)->set_closures_next;
    return NULL;
  } break;
  case PROG_RETURN: {
    if ((*p)->return_hat) {
      break;
    }
    routine_2 hat_par = (*st)->hat_parent;
    routine_1 yield_to = (*st)->parent;
    if (routine_1_is_null(yield_to)) {
      return ("bad return");
    }
    *st = state_change_plain_parent(*st, routine_1_make_null());
    datum *result = state_stack_pop(st);
    *p = yield_to.prog_;
    *st = yield_to.state_;
    state_stack_put(st, result);
    (*st)->hat_parent =
      hat_par; /* Because the caller hat parent might be out-of-date.*/
    return NULL;
  } break;
  case PROG_YIELD: {
    // return fstate_make_panic("disabled ATM");
    if ((*p)->yield_hat) {
      break;
    }
    routine_2 hat_par = (*st)->hat_parent;
    routine_1 yield_to = (*st)->parent;
    if (routine_1_is_null(yield_to)) {
      return ("bad yield");
    }
    *st = state_change_plain_parent(*st, routine_1_make_null());
    datum *val = state_stack_pop(st);
    datum *conti = datum_make_routine_0((*p)->yield_next, *st);
    datum *result = datum_make_list_2(val, conti);
    *p = yield_to.prog_;
    *st = yield_to.state_;
    state_stack_put(st, result);
    (*st)->hat_parent =
      hat_par; /* Because the caller hat parent might be out-of-date.*/
    return NULL;
  } break;
  default: break;
  }
  return routine_0_step(p, st, perform_host_instruction);
}

LOCAL char *routine_0_step(prog **p, state **st,
                            fdatum (*perform_host_instruction)(datum *,
                                                               datum *)) {
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
    if ((*p)->pop_var != NULL) {
      *st = state_set_var(*st, (*p)->pop_var, v);
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
    datum *submodule_state = pair->list_tail->list_head;
    if (!datum_is_routine_0(submodule_state)) {
      return ("expected a routine after a submodule call");
    }
    state *module_state = submodule_state->routine_0_value.state_;

    datum *imported_bindings = state_list_vars(module_state);
    for (; !datum_is_nil(imported_bindings);
         imported_bindings = imported_bindings->list_tail) {
      datum *sym = imported_bindings->list_head->list_head;
      datum *val = imported_bindings->list_head->list_tail->list_head;

      *st = state_set_var(*st, sym, val);
    }
    *p = (*p)->import_next;
    return NULL;
  } break;
  default:break;
  }
  return ("unhandled state type");
}
