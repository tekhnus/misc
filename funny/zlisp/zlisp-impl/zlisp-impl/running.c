#include <zlisp-impl/zlisp-impl.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>


fdatum routine_run_and_get_value(state **ctxt, prog *p, fdatum (*perform_host_instruction)(datum *, datum *)) {
  // routine r = routine_make(p, *ctxt); fstate s = routine_run(r, perform_host_instruction);
  fstate s = routine_2_run(p, *ctxt, perform_host_instruction);
  if (fstate_is_panic(s)) {
    return fdatum_make_panic(s.panic_message);
  }
  *ctxt = s.ok_value;
  return fdatum_make_ok(state_stack_pop(ctxt));
}

LOCAL fstate routine_2_run(prog *p, state *s, fdatum (*perform_host_instruction)(datum *, datum *)) {
  fstate res = fstate_make_ok(s);
  for (; p->type != PROG_END && !fstate_is_panic(res); ) {
    // printf("%d %s\n", p->type, datum_repr(s->stack));
    res = routine_2_step(&p, res.ok_value, perform_host_instruction);
  }
  return res;
}

LOCAL fstate routine_2_step(prog **p, state *s, fdatum (*perform_host_instruction)(datum *, datum *)) {
  switch ((*p)->type) {
  case PROG_CALL: {
    if (!(*p)->call_hat) {
      break;
    }
    //return fstate_make_panic("disabled ATM");
    datum *form = state_stack_pop(&s);
    if (!datum_is_list(form) || datum_is_nil(form)) {
      return fstate_make_panic("a call instruction with a malformed form");
    }
    datum *fn = form->list_head;
    datum *args = form->list_tail;
    if (!datum_is_routine_1(fn)) {
      return fstate_make_panic("tried to hat-call a non-routine-1");
    }
    routine parent_cont = routine_2_make((*p)->call_next, s);
    *p = fn->routine_1_value.prog_;
    s = fn->routine_1_value.state_;
    state_stack_put(&s, args);
    s = state_change_parent(s, parent_cont, true);
    return fstate_make_ok(s);
  } break;
  case PROG_SET_CLOSURES: {
    if (!(*p)->set_closures_hat){
      break;
    }
    datum *clos = datum_make_routine_1((*p)->set_closures_prog, NULL);
    s = state_set_var(s, (*p)->set_closures_name, clos);
    clos->routine_1_value.state_ = s;
    *p = (*p)->set_closures_next;
    return fstate_make_ok(s);
  } break;
  case PROG_RETURN: {
    if (!(*p)->return_hat) {
      break;
    }
    //return fstate_make_panic("disabled ATM");
    routine yield_to = s->hat_parent;
    if (routine_is_null(yield_to)) {
      return fstate_make_panic("bad return");
    }
    s = state_change_parent(s, routine_2_make_null(), true);
    datum *result = state_stack_pop(&s);
    *p = yield_to.prog_;
    s = yield_to.state_;
    state_stack_put(&s, result);
    return fstate_make_ok(s);
  } break;
  case PROG_YIELD: {
    if (!(*p)->yield_hat) {
      break;
    }
    //return fstate_make_panic("disabled ATM");
    routine yield_to = s->hat_parent;
    if (routine_is_null(yield_to)) {
      return fstate_make_panic("bad yield");
    }
    s = state_change_parent(s, routine_2_make_null(), true);
    datum *val = state_stack_pop(&s);
    datum *conti = datum_make_routine_1((*p)->yield_next, s);
    datum *result = datum_make_list_2(val, conti);
    *p = yield_to.prog_;
    s = yield_to.state_;
    state_stack_put(&s, result);
    return fstate_make_ok(s);
  } break;
  default: break;
  }
  return routine_1_step(p, s, perform_host_instruction);
}

LOCAL fstate routine_1_step(prog **p, state *s, fdatum (*perform_host_instruction)(datum *, datum *)) {
  switch ((*p)->type) {
  case PROG_CALL: {
    if ((*p)->call_hat) {
      break;
    }
    datum *form = state_stack_pop(&s);
    if (!datum_is_list(form) || datum_is_nil(form)) {
      return fstate_make_panic("a call instruction with a malformed form");
    }
    datum *fn = form->list_head;
    datum *args = form->list_tail;
    if (!datum_is_routine_0(fn)) {
      return fstate_make_panic("tried to plain-call a non-routine-0");
    }
    routine parent_cont = routine_1_make((*p)->call_next, s);
    *p = fn->routine_0_value.prog_;
    s = fn->routine_0_value.state_;
    state_stack_put(&s, args);
    s = state_change_parent(s, parent_cont, false);
    s =
      state_change_parent(s, parent_cont.state_->hat_parent, true);
    return fstate_make_ok(s);
  } break;
  case PROG_SET_CLOSURES: {
    if ((*p)->set_closures_hat){
      break;
    }
    datum *clos = datum_make_routine_0((*p)->set_closures_prog, NULL);
    s = state_set_var(s, (*p)->set_closures_name, clos);
    clos->routine_0_value.state_ = s;
    *p = (*p)->set_closures_next;
    return fstate_make_ok(s);
  } break;
  case PROG_RETURN: {
    if ((*p)->return_hat) {
      break;
    }
    routine hat_par = s->hat_parent;
    routine yield_to = s->parent;
    if (routine_is_null(yield_to)) {
      return fstate_make_panic("bad return");
    }
    s = state_change_parent(s, routine_1_make_null(), false);
    datum *result = state_stack_pop(&s);
    *p = yield_to.prog_;
    s = yield_to.state_;
    state_stack_put(&s, result);
    s->hat_parent =
      hat_par; /* Because the caller hat parent might be out-of-date.*/
    return fstate_make_ok(s);
  } break;
  case PROG_YIELD: {
    // return fstate_make_panic("disabled ATM");
    if ((*p)->yield_hat) {
      break;
    }
    routine hat_par = s->hat_parent;
    routine yield_to = s->parent;
    if (routine_is_null(yield_to)) {
      return fstate_make_panic("bad yield");
    }
    s = state_change_parent(s, routine_1_make_null(), false);
    datum *val = state_stack_pop(&s);
    datum *conti = datum_make_routine_0((*p)->yield_next, s);
    datum *result = datum_make_list_2(val, conti);
    *p = yield_to.prog_;
    s = yield_to.state_;
    state_stack_put(&s, result);
    s->hat_parent =
      hat_par; /* Because the caller hat parent might be out-of-date.*/
    return fstate_make_ok(s);
  } break;
  default: break;
  }
  return routine_0_step(p, s, perform_host_instruction);
}

LOCAL fstate routine_0_step(prog **p, state *s,
                            fdatum (*perform_host_instruction)(datum *,
                                                               datum *)) {
  // routine c = routine_make(*p, s);
  switch ((*p)->type) {
  case PROG_NOP: {
    *p = (*p)->nop_next;
    return fstate_make_ok(s);
  } break;
  case PROG_IF: {
    datum *v = state_stack_pop(&s);
    if (!datum_is_nil(v)) {
      *p = (*p)->if_true;
    } else {
      *p = (*p)->if_false;
    }
    return fstate_make_ok(s);
  } break;
  case PROG_PUT_CONST: {
    state_stack_put(&s, (*p)->put_const_value);
    *p = (*p)->put_const_next;
    return fstate_make_ok(s);
  } break;
  case PROG_PUT_VAR: {
    fdatum er = state_get_var(s, (*p)->put_var_value);
    if (fdatum_is_panic(er)) {
      return fstate_make_panic(er.panic_message);
    }
    state_stack_put(&s, er.ok_value);
    *p = (*p)->put_var_next;
    return fstate_make_ok(s);
  } break;
  case PROG_POP: {
    datum *v = state_stack_pop(&s);
    if ((*p)->pop_var != NULL) {
      s = state_set_var(s, (*p)->pop_var, v);
    }
    *p = (*p)->pop_next;
    return fstate_make_ok(s);
  } break;
  case PROG_ARGS: {
    state_stack_new(&s);
    *p = (*p)->args_next;
    return fstate_make_ok(s);
  } break;
  case PROG_HOST: {
    datum *name = (*p)->host_instruction;
    datum *arg = state_stack_pop(&s);
    fdatum res = perform_host_instruction(name, arg);
    if (fdatum_is_panic(res)) {
      return fstate_make_panic(res.panic_message);
    }
    state_stack_put(&s, res.ok_value);
    *p = (*p)->host_next;
    return fstate_make_ok(s);
  } break;
  case PROG_COLLECT: {
    datum *form = state_stack_collect(&s);
    state_stack_put(&s, form);
    *p = (*p)->collect_next;
    return fstate_make_ok(s);
  } break;
  case PROG_IMPORT: {
    // fprintf(stderr, "MODULE_END\n");
    datum *pair = state_stack_pop(&s);
    if (!datum_is_list(pair) || list_length(pair) != 2) {
      return fstate_make_panic("expected a pair after a submodule call");
    }
    datum *submodule_state = pair->list_tail->list_head;
    if (!datum_is_routine_0(submodule_state)) {
      return fstate_make_panic("expected a routine after a submodule call");
    }
    state *module_state = submodule_state->routine_0_value.state_;

    datum *imported_bindings = state_list_vars(module_state);
    for (; !datum_is_nil(imported_bindings);
         imported_bindings = imported_bindings->list_tail) {
      datum *sym = imported_bindings->list_head->list_head;
      datum *val = imported_bindings->list_head->list_tail->list_head;

      s = state_set_var(s, sym, val);
    }
    *p = (*p)->import_next;
    return fstate_make_ok(s);
  } break;
  default:break;
  }
  return fstate_make_panic("unhandled state type");
}
