#include <zlisp-impl/running.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <dlfcn.h>

fstate routine_run(routine c) {
  for (;;) {
    // printf("%d\n", c.prog->type);
    switch (c.prog_->type) {
    case PROG_END: {
      if (!routine_is_null(c.state_->parent)) {
        return fstate_make_panic("reached the end state in a subroutine");
      }
      return fstate_make_ok(c.state_);
    } break;
    case PROG_NOP: {
      c.prog_ = c.prog_->nop_next;
    } break;
    case PROG_IF: {
      datum *v = state_stack_pop(&c.state_);
      if (!datum_is_nil(v)) {
        c.prog_ = c.prog_->if_true;
      } else {
        c.prog_ = c.prog_->if_false;
      }
    } break;
    case PROG_PUT_CONST: {
      state_stack_put(&c.state_, c.prog_->put_const_value);
      c.prog_ = c.prog_->put_const_next;
    } break;
    case PROG_PUT_ROUTINE: {
      if (!datum_is_routine(c.prog_->put_routine_value)) {
        return fstate_make_panic("a routine was expected");
      }
      routine r = c.prog_->put_routine_value->routine_value;
      if (r.state_ != NULL) {
        return fstate_make_panic("the routine context was expected to be null");
      }
      r.state_ = state_make(c.state_->vars, datum_make_nil(),
                            routine_make_null(), routine_make_null());
      state_stack_put(&c.state_, datum_make_routine(r.prog_, r.state_));
      c.prog_ = c.prog_->put_routine_next;
    } break;
    case PROG_PUT_VAR: {
      fdatum er = state_get_var(c.state_, c.prog_->put_var_value);
      if (fdatum_is_panic(er)) {
        return fstate_make_panic(er.panic_message);
      }
      state_stack_put(&c.state_, er.ok_value);
      c.prog_ = c.prog_->put_var_next;
    } break;
    case PROG_POP: {
      datum *v = state_stack_pop(&c.state_);
      if (c.prog_->pop_var != NULL) {
        c.state_ = state_set_var(c.state_, c.prog_->pop_var, v);
      }
      c.prog_ = c.prog_->pop_next;
    } break;
    case PROG_POP_PROG: {
      datum *v = state_stack_pop(&c.state_);
      if (c.prog_->pop_prog_var != NULL) {
        c.state_ = state_set_fn(c.state_, c.prog_->pop_prog_var, v);
      }
      c.prog_ = c.prog_->pop_prog_next;
    } break;
    case PROG_ARGS: {
      state_stack_new(&c.state_);
      c.prog_ = c.prog_->args_next;
    } break;
    case PROG_CALL: {
      datum *form = state_stack_pop(&c.state_);
      if (datum_is_nil(form)) {
        return fstate_make_panic("a call instruction with empty form");
      }
      datum *fn = form->list_head;
      datum *args = form->list_tail;
      if (!datum_is_routine(fn)) {
        return fstate_make_panic("tried to call a non-routine");
      }
      bool hat = c.prog_->call_hat;
      routine parent_cont = routine_make(c.prog_->call_next, c.state_);
      switch_context(&c, fn->routine_value, args);
      if (!routine_is_null(state_get_parent(c.state_, hat))) {
        return fstate_make_panic(
            "attempt to call routine with existing parent");
      }
      c.state_ = state_change_parent(c.state_, parent_cont, hat);
      if (!hat) {
        if (!routine_is_null(state_get_parent(c.state_, true))) {
          return fstate_make_panic(
              "attempt to call routine with existing hat-parent");
        }
        c.state_ =
            state_change_parent(c.state_, parent_cont.state_->hat_parent, true);
      }
    } break;
    case PROG_POINTER_CALL: {
      datum *form = state_stack_pop(&c.state_);
      if (!datum_is_list(form) || list_length(form) != 2) {
        return fstate_make_panic("pointer-call expected a pair on stack");
      }
      datum *fn = form->list_head;
      datum *args = form->list_tail->list_head;
      fdatum res = pointer_call(fn, args);
      if (fdatum_is_panic(res)) {
        return fstate_make_panic(res.panic_message);
      }
      state_stack_put(&c.state_, res.ok_value);
      c.prog_ = c.prog_->pointer_call_next;
    } break;
    case PROG_BUILTIN_POINTER: {
      datum *name = c.prog_->builtin_pointer_name;
      if (!datum_is_bytestring(name)) {
        return fstate_make_panic("builtin-pointer name should be a string");
      }
      datum *res;
      if (!strcmp(name->bytestring_value, "lowlevel-shared-library")) {
        res = datum_make_pointer((void *)builtin_ptr_lowlevel_shared_library, datum_make_list_2(datum_make_list_1(datum_make_symbol("datum")), datum_make_symbol("val")));
      } else if (!strcmp(name->bytestring_value, "lowlevel-extern-pointer")) {
        res = datum_make_pointer((void *)builtin_ptr_lowlevel_extern_pointer, datum_make_list_2(datum_make_list_3(datum_make_symbol("datum"), datum_make_symbol("datum"), datum_make_symbol("datum")), datum_make_symbol("val")));
      } else if (!strcmp(name->bytestring_value, "not-null-pointer")) {
        res = datum_make_pointer((void *)builtin_ptr_not_null_pointer, datum_make_list_2(datum_make_list_1(datum_make_symbol("datum")), datum_make_symbol("val")));
      } else if (!strcmp(name->bytestring_value, "dlopen")) {
        res = datum_make_pointer((void *)simplified_dlopen, datum_make_list_2(datum_make_list_1(datum_make_symbol("string")), datum_make_symbol("pointer")));
      } else if (!strcmp(name->bytestring_value, "dlsym")) {
        res = datum_make_pointer((void *)dlsym, datum_make_list_2(datum_make_list_2(datum_make_symbol("pointer"), datum_make_symbol("string")), datum_make_symbol("pointer")));
      } else if (!strcmp(name->bytestring_value, "dereference-and-cast")) {
        res = datum_make_pointer((void *)builtin_ptr_dereference_and_cast, datum_make_list_2(datum_make_list_2(datum_make_symbol("datum"), datum_make_symbol("datum")), datum_make_symbol("val")));
      } else {
        return fstate_make_panic("unknown builtin-pointer");
      }
      state_stack_put(&c.state_, res);
      c.prog_ = c.prog_->builtin_pointer_next;
    } break;
    case PROG_COLLECT: {
      datum *form = state_stack_collect(&c.state_);
      state_stack_put(&c.state_, form);
      c.prog_ = c.prog_->collect_next;
    } break;
    case PROG_RETURN: {
      routine hat_par = c.state_->hat_parent;
      routine return_to;
      if (c.prog_->return_hat) {
        return fstate_make_panic("^return not implemented yet");
      } else {
        return_to = c.state_->parent;
      }
      if (routine_is_null(return_to)) {
        return fstate_make_panic("bad return");
      }
      datum *res = state_stack_pop(&c.state_);
      switch_context(&c, return_to, res);
      c.state_->hat_parent =
          hat_par; /* Because the caller hat parent might be out-of-date.*/
    } break;
    case PROG_YIELD: {
      bool hat;
      routine yield_to;
      if (c.prog_->yield_hat) {
        hat = true;
        yield_to = c.state_->hat_parent;
      } else {
        hat = false;
        yield_to = c.state_->parent;
      }
      if (routine_is_null(yield_to)) {
        return fstate_make_panic("bad yield");
      }
      c.state_ = state_change_parent(c.state_, routine_make_null(), hat);
      datum *res = state_stack_pop(&c.state_);
      datum *resume = datum_make_routine(c.prog_->yield_next, c.state_);
      datum *r = datum_make_list_2(res, resume);
      switch_context(&c, yield_to, r);
    } break;
    case PROG_MODULE_END: {
      state *module_state = c.state_;
      routine return_to = c.state_->parent;
      if (routine_is_null(return_to)) {
        return fstate_make_ok(c.state_);
      }
      state_stack_pop(&c.state_);
      switch_context(&c, return_to, datum_make_void());

      datum *imported_bindings = state_list_vars(module_state);
      for (; !datum_is_nil(imported_bindings);
           imported_bindings = imported_bindings->list_tail) {
        datum *sym = imported_bindings->list_head->list_head;
        datum *val = imported_bindings->list_head->list_tail->list_head;

        c.state_ = state_set_var(c.state_, sym, val);
      }
    } break;
    default: {
      return fstate_make_panic("unhandled state type");
    } break;
    }
  }
}

fdatum routine_run_and_get_value(state **ctxt, prog *p) {
  routine r = routine_make(p, *ctxt);
  fstate s = routine_run(r);
  if (fstate_is_panic(s)) {
    return fdatum_make_panic(s.panic_message);
  }
  *ctxt = s.ok_value;
  return fdatum_make_ok(state_stack_pop(ctxt));
}

LOCAL void switch_context(routine *c, routine b, datum *v) {
  *c = b;
  state_stack_put(&c->state_, v);
}

void *simplified_dlopen(char *path) {
  if (strlen(path) == 0) {
    return RTLD_DEFAULT;
  }
  return dlopen(path, RTLD_LAZY);
}

LOCAL fdatum builtin_ptr_lowlevel_shared_library(datum *library_name) {
  if (!datum_is_bytestring(library_name)) {
    return fdatum_make_panic("load-shared-library expects a bytestring");
  }
  void **handle = malloc(sizeof(void *));
  if (strlen(library_name->bytestring_value) == 0) {
    *handle = RTLD_DEFAULT;
  } else {
    *handle = dlopen(library_name->bytestring_value, RTLD_LAZY);
  }
  char *err = dlerror();
  if (!*handle) {
    fprintf(stderr, "WARNING: dlopen failed: %s\n", err);
    return fdatum_make_ok(datum_make_pointer_to_pointer(NULL));
  }

  return fdatum_make_ok(datum_make_pointer_to_pointer(handle));
}

LOCAL fdatum builtin_ptr_lowlevel_extern_pointer(datum *shared_library, datum *name,
                              datum *descriptor) {
  if (!datum_is_pointer(shared_library) ||
      !datum_is_symbol(shared_library->pointer_descriptor) ||
      strcmp(shared_library->pointer_descriptor->symbol_value, "pointer")) {
    return fdatum_make_panic("wrong externcdata usage");
  }
  void *handle = *(void **)shared_library->pointer_value;
  if (!datum_is_bytestring(name)) {
    return fdatum_make_panic("externcdata expected a string");
  }
  void *call_ptr = dlsym(handle, name->bytestring_value);
  char *err = dlerror();
  if (err != NULL) {
    fprintf(stderr, "WARNING: dlsym failed: %s\n", err);
    return fdatum_make_ok(datum_make_pointer(NULL, descriptor));
  }
  return fdatum_make_ok(datum_make_pointer(call_ptr, descriptor));
}

LOCAL fdatum builtin_ptr_not_null_pointer(datum *pointer) {
  if (!datum_is_pointer(pointer)) {
    return fdatum_make_panic("not-null-pointer expects a pointer");
  }
  if (pointer->pointer_value != NULL) {
    return fdatum_make_ok(datum_make_list_1(datum_make_nil()));
  }
  return fdatum_make_ok(datum_make_nil());
}

LOCAL fdatum builtin_ptr_dereference_and_cast(datum *ptpt, datum *new_descriptor) {
  if (!datum_is_pointer(ptpt) || !datum_is_the_symbol(ptpt->pointer_descriptor, "pointer")) {
    return fdatum_make_panic("dereference expected a pointer to pointer");
  }
  return fdatum_make_ok(datum_make_pointer(*((void **)ptpt->pointer_value), new_descriptor));
}
