#include <zlisp-impl/zlisp-impl.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>


fdatum routine_run_and_get_value(prog_slice sl, state **ctxt, prog *p, fdatum (*perform_host_instruction)(datum *, datum *)) {
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

LOCAL char *routine_2_run(prog_slice sl, routine_2 *r, fdatum (*perform_host_instruction)(datum *, datum *)) {
  for (; r->cur.cur.prog_->type != PROG_END; ) {
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

LOCAL routine_1 routine_1_deep_copy(routine_1 r) {
  routine_1 res = r;
  if (res.par != NULL) {
    routine_1 *new_par = malloc(sizeof(routine_1));
    *new_par = routine_1_deep_copy(*res.par);
    res.par = new_par;
  }
  return res;
}

LOCAL char *routine_2_step(prog_slice sl, routine_2 *r, fdatum (*perform_host_instruction)(datum *, datum *)) {
  prog **p = &r->cur.cur.prog_;
  state **st = &r->cur.cur.state_;
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
    r->cur.cur.prog_ = r->cur.cur.prog_->call_next;
    routine_2_push_frame(r, routine_1_deep_copy(fn->routine_1_value));
    state_stack_put(&r->cur.cur.state_, args);
    return NULL;
  } break;
  case PROG_SET_CLOSURES: {
    if (!(*p)->set_closures_hat){
      break;
    }
    datum *clos = datum_make_routine_1(routine_1_make_null());
    state_set_var(st, (*p)->set_closures_name, clos);
    clos->routine_1_value = routine_1_deep_copy(r->cur);
    clos->routine_1_value.cur.prog_ = (*p)->set_closures_prog;
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
    datum *conti = datum_make_routine_1(routine_1_deep_copy(fr));
    datum *result = datum_make_list_2(val, conti);
    state_stack_put(st, result);
    return NULL;
  } break;
  default: break;
  }
  char *err = routine_1_step(sl, &r->cur, perform_host_instruction);
  return err;
}

LOCAL char *routine_1_step(prog_slice sl, routine_1 *r, fdatum (*perform_host_instruction)(datum *, datum *)) {
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
    if (datum_is_list(fn) && list_length(fn) == 2 && datum_is_integer(fn->list_head) && datum_is_list(fn->list_tail->list_head) && list_length(fn->list_tail->list_head) == 2) {
      int64_t offset = fn->list_head->integer_value;
      datum *vars = fn->list_tail->list_head->list_head;
      datum *stack = fn->list_tail->list_head->list_tail->list_head;
      callee = routine_0_make(prog_slice_at(sl, offset), state_make(vars, stack));
    }
    else if (datum_is_routine_0(fn)) {
      callee = fn->routine_0_value;
    } else {
      return ("tried to plain-call a non-routine-0");
    }
    r->cur.prog_ = r->cur.prog_->call_next;
    routine_1_push_frame(r, callee);
    state_stack_put(st, args);
    return NULL;
  } break;
  case PROG_SET_CLOSURES: {
    if ((*p)->set_closures_hat){
      break;
    }
    if (&sl == &sl) {}
    datum *clos = datum_make_list_2(prog_to_offset(sl, (*p)->set_closures_prog), datum_make_nil());
    state_set_var(st, (*p)->set_closures_name, clos);
    clos->list_tail->list_head = datum_make_list_2((*st)->vars, (*st)->stack); // modifying a datum because we need to create a circular reference:(
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
    datum *conti = datum_make_routine_0(fr);
    datum *result = datum_make_list_2(val, conti);
    state_stack_put(st, result);
    return NULL;
  } break;
  default: break;
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

      state_set_var(st, sym, val);
    }
    *p = (*p)->import_next;
    return NULL;
  } break;
  default:break;
  }
  return ("unhandled state type");
}
