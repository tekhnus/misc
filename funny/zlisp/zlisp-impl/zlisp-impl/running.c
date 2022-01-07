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

fdatum state_run_prog(state **ctxt, datum *v,
                      routine (*module_source)(char *)) {
  prog *s = prog_make();
  char *err =
      prog_init_module(s, datum_make_list(v, datum_make_nil()), module_source);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  routine c = routine_make(s, *ctxt);
  fstate res = routine_run(c);
  if (fstate_is_panic(res)) {
    return fdatum_make_panic(res.panic_message);
  }
  *ctxt = res.ok_value;
  return fdatum_make_ok(state_stack_pop(ctxt));
}

state *state_make_builtins() {
  state *ns = state_make_fresh();

  namespace_def_extern_fn(&ns, "panic--", builtin_panic, 1);
  namespace_def_extern_fn(&ns, "shared-library--", builtin_shared_library, 1);
  namespace_def_extern_fn(&ns, "extern-pointer--", builtin_extern_pointer, 3);
  namespace_def_extern_fn(&ns, "cons--", builtin_cons, 2);
  namespace_def_extern_fn(&ns, "head--", builtin_head, 1);
  namespace_def_extern_fn(&ns, "tail--", builtin_tail, 1);
  namespace_def_extern_fn(&ns, "eq--", builtin_eq, 2);
  namespace_def_extern_fn(&ns, "annotate--", builtin_annotate, 1);
  namespace_def_extern_fn(&ns, "is-constant--", builtin_is_constant, 1);
  namespace_def_extern_fn(&ns, "repr--", builtin_repr, 1);
  namespace_def_extern_fn(&ns, "concat-bytestrings--",
                          builtin_concat_bytestrings, 2);
  namespace_def_extern_fn(&ns, "+--", builtin_add, 2);

  char *prelude_src =
      "(builtin.defn panic (return (pointer-call panic-- args)))"
      "(builtin.defn shared-library (return (pointer-call shared-library-- "
      "args)))"
      "(builtin.defn extern-pointer (return (pointer-call extern-pointer-- "
      "args)))"
      "(builtin.defn cons (return (pointer-call cons-- args)))"
      "(builtin.defn head (return (pointer-call head-- args)))"
      "(builtin.defn tail (return (pointer-call tail-- args)))"
      "(builtin.defn eq (return (pointer-call eq-- args)))"
      "(builtin.defn annotate (return (pointer-call annotate-- args)))"
      "(builtin.defn is-constant (return (pointer-call is-constant-- args)))"
      "(builtin.defn repr (return (pointer-call repr-- args)))"
      "(builtin.defn concat-bytestrings (return (pointer-call "
      "concat-bytestrings-- args)))"
      "(builtin.defn + (return (pointer-call +-- args)))";
  FILE *prelude_f = fmemopen(prelude_src, strlen(prelude_src), "r");
  fdatum prelude_d = datum_read_all(prelude_f);
  if (fdatum_is_panic(prelude_d)) {
    fprintf(stderr, "prelude syntax: %s", prelude_d.panic_message);
    exit(EXIT_FAILURE);
  }
  for (datum *rest = prelude_d.ok_value; !datum_is_nil(rest);
       rest = rest->list_tail) {
    fdatum err = state_run_prog(&ns, rest->list_head, NULL);
    if (fdatum_is_panic(err)) {
      fprintf(stderr, "prelude compilation: %s", err.panic_message);
      exit(EXIT_FAILURE);
    }
  }
  return ns;
}

LOCAL void switch_context(routine *c, routine b, datum *v) {
  *c = b;
  state_stack_put(&c->state_, v);
}

LOCAL fdatum builtin_eq(datum *x, datum *y) {
  datum *t = datum_make_list_1(datum_make_nil());
  datum *f = datum_make_nil();
  if (datum_eq(x, y)) {
    return fdatum_make_ok(t);
  }
  return fdatum_make_ok(f);
}

LOCAL fdatum builtin_annotate(datum *arg_value) {
  char *type;
  if (datum_is_list(arg_value)) {
    type = ":list";
  } else if (datum_is_symbol(arg_value)) {
    type = ":symbol";
  } else if (datum_is_bytestring(arg_value)) {
    type = ":bytestring";
  } else if (datum_is_integer(arg_value)) {
    type = ":integer";
  } else if (datum_is_routine(arg_value)) {
    type = ":operator";
  } else if (datum_is_pointer(arg_value)) {
    type = ":pointer";
  } else {
    return fdatum_make_panic("incomplete implementation of type");
  }
  return fdatum_make_ok(datum_make_list_2(datum_make_symbol(type), arg_value));
}

LOCAL fdatum builtin_is_constant(datum *arg_value) {
  if (datum_is_constant(arg_value)) {
    return fdatum_make_ok(datum_make_list_1(datum_make_nil()));
  }
  return fdatum_make_ok(datum_make_nil());
}

LOCAL fdatum builtin_panic(datum *arg_value) {
  if (!datum_is_bytestring(arg_value)) {
    return fdatum_make_panic("panic expects a bytestring");
  }
  return fdatum_make_panic(arg_value->bytestring_value);
}

LOCAL fdatum builtin_shared_library(datum *library_name) {
  if (!datum_is_bytestring(library_name)) {
    return fdatum_make_panic("load-shared-library expects a bytestring");
  }
  void **handle = malloc(sizeof(void *));
  *handle = dlopen(library_name->bytestring_value, RTLD_LAZY);
  char *err = dlerror();
  if (!*handle) {
    return fdatum_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                            datum_make_bytestring(err)));
  }
  return fdatum_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"), datum_make_pointer_to_pointer(handle)));
}

LOCAL fdatum builtin_extern_pointer(datum *shared_library, datum *name,
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
    return fdatum_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                            datum_make_bytestring(err)));
  }
  return fdatum_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"), datum_make_pointer(call_ptr, descriptor)));
}

LOCAL fdatum builtin_repr(datum *v) {
  return fdatum_make_ok(datum_make_bytestring(datum_repr(v)));
}


LOCAL fdatum builtin_concat_bytestrings(datum *x, datum *y) {
  if (!datum_is_bytestring(x) || !datum_is_bytestring(y)) {
    return fdatum_make_panic("expected integers");
  }
  char *buf =
      malloc(strlen(x->bytestring_value) + strlen(y->bytestring_value) + 1);
  buf[0] = '\0';
  strcat(buf, x->bytestring_value);
  strcat(buf, y->bytestring_value);
  return fdatum_make_ok(datum_make_bytestring(buf));
}

LOCAL fdatum builtin_add(datum *x, datum *y) {
  if (!datum_is_integer(x) || !datum_is_integer(y)) {
    return fdatum_make_panic("expected integers");
  }
  return fdatum_make_ok(datum_make_int(x->integer_value + y->integer_value));
}

LOCAL fdatum builtin_cons(datum *head, datum *tail) {
  if (!datum_is_list(tail)) {
    return fdatum_make_panic("cons requires a list as a second argument");
  }
  return fdatum_make_ok(datum_make_list(head, tail));
}

LOCAL fdatum builtin_head(datum *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("car expects a nonempty list");
  }
  return fdatum_make_ok(list->list_head);
}

LOCAL fdatum builtin_tail(datum *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("cdr expects a nonempty list");
  }
  return fdatum_make_ok(list->list_tail);
}

LOCAL void namespace_def_extern_fn(state **ctxt, char *name, fdatum (*fn)(),
                             int cnt) {
  datum *sig = datum_make_nil();
  for (int i = 0; i < cnt; ++i) {
    sig = datum_make_list(datum_make_symbol("datum"), sig);
  }
  datum *wrapped_fn =
      datum_make_pointer(__extension__(void *) fn,
                         datum_make_list_2(sig, datum_make_symbol("val")));
  *ctxt = state_set_var(*ctxt, datum_make_symbol(name), wrapped_fn);
}
