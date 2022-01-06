// zlisp interpreter.
#include <zlisp-impl/main.h>

#include <ctype.h>
#include <dlfcn.h>


#include <libgen.h>

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#if INTERFACE
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <ffi.h>
enum fdatumype {
  FDATUM_OK,
  FDATUM_PANIC,
};

enum fstate_type {
  FSTATE_OK,
  FSTATE_PANIC,
};

enum datum_type {
  DATUM_NIL,
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
  DATUM_ROUTINE,
  DATUM_POINTER,
  DATUM_VOID,
};

struct routine {
  prog *prog_;
  state *state_;
};

struct datum {
  enum datum_type type;
  union {
    struct {
      datum *list_head;
      datum *list_tail;
    };
    char *symbol_value;
    char *bytestring_value;
    int64_t integer_value;
    routine routine_value;
    struct {
      void *pointer_value;
      datum *pointer_descriptor;
    };
  };
};

struct state {
  datum *vars;
  datum *stack;
  routine parent;
  routine hat_parent;
};

enum read_result_type {
  READ_RESULT_OK,
  READ_RESULT_PANIC,
  READ_RESULT_EOF,
  READ_RESULT_RIGHT_PAREN,
};

struct read_result {
  enum read_result_type type;
  union {
    datum *ok_value;
    char *panic_message;
  };
};

struct fdatum {
  int type;
  datum *ok_value;
  char *panic_message;
};

struct fstate {
  int type;
  state *ok_value;
  char *panic_message;
};

enum prog_type {
  PROG_END,
  PROG_IF,
  PROG_NOP,
  PROG_PUT_CONST,
  PROG_PUT_ROUTINE,
  PROG_PUT_VAR,
  PROG_ARGS,
  PROG_CALL,
  PROG_COLLECT,
  PROG_POP,
  PROG_POP_PROG,
  PROG_RETURN,
  PROG_YIELD,
  PROG_MODULE_END,
};

struct prog {
  enum prog_type type;
  union {
    struct {
      prog *if_true;
      prog *if_false;
    };
    struct {
      prog *nop_next;
    };
    struct {
      datum *put_const_value;
      prog *put_const_next;
    };
    struct {
      datum *put_routine_value;
      prog *put_routine_next;
    };
    struct {
      datum *put_var_value;
      prog *put_var_next;
    };
    struct prog *args_next;
    struct {
      bool call_hat;
      prog *call_next;
    };
    struct prog *collect_next;
    struct {
      datum *pop_var;
      prog *pop_next;
    };
    struct {
      datum *pop_prog_var;
      prog *pop_prog_next;
    };
    bool return_hat;
    struct {
      bool yield_hat;
      prog *yield_next;
    };
  };
};
#endif

LOCAL bool datum_is_the_symbol(datum *d, char *val) {
  return datum_is_symbol(d) && !strcmp(d->symbol_value, val);
}

LOCAL void state_stack_put(state **ns, datum *value) {
  *ns = state_make((*ns)->vars, datum_make_list(value, (*ns)->stack), (*ns)->parent,
                    (*ns)->hat_parent);
}

LOCAL datum *state_stack_pop(state **s) {
  datum *res = (*s)->stack->list_head;
  *s = state_make((*s)->vars, (*s)->stack->list_tail, (*s)->parent, (*s)->hat_parent);
  return res;
}

LOCAL void state_stack_new(state **s) {
  state_stack_put(s, datum_make_symbol("__function_call"));
}

LOCAL datum *state_stack_collect(state **s) {
  datum *form = datum_make_nil();
  for (;;) {
    datum *arg = state_stack_pop(s);
    if (datum_is_the_symbol(arg, "__function_call")) {
      break;
    }
    form = datum_make_list(arg, form);
  }
  return form;
}

prog *prog_make() {
  prog *res = malloc(sizeof(prog));
  res->type = PROG_END;
  return res;
}

void prog_append_module_end(prog **begin) {
  (*begin)->type = PROG_MODULE_END;
  *begin = prog_make();
}

char *prog_init_module(prog *s, datum *source,
                            fdatum (*module_source)(char *module)) {
  for (datum *rest = source; !datum_is_nil(rest); rest = rest->list_tail) {
    datum *stmt = rest->list_head;
    char *err = prog_append_statement(&s, stmt, module_source);
    if (err != NULL) {
      return err;
    }
  }
  prog_append_module_end(&s);
  return NULL;
}

routine routine_make(prog *s, state *ctxt) {
  routine res = {.prog_ = s, .state_ = ctxt};
  return res;
}

routine routine_make_null() {
  routine res = {};
  return res;
}

bool routine_is_null(routine r) { return r.prog_ == NULL && r.state_ == NULL; }

routine state_get_parent(state *ns, bool hat) {
  if (hat) {
    return ns->hat_parent;
  }
  return ns->parent;
}

state *state_change_parent(state *ns, routine new_parent, bool hat) {
  if (hat) {
    return state_make(ns->vars, ns->stack, ns->parent, new_parent);
  }
  return state_make(ns->vars, ns->stack, new_parent, ns->hat_parent);
}

int list_length(datum *seq) {
  if (!datum_is_list(seq)) {
    return -1;
  }
  int res;
  for (res = 0; !datum_is_nil(seq); seq = seq->list_tail, ++res) {
  }
  return res;
}

LOCAL bool datum_is_the_symbol_pair(datum *d, char *val1, char *val2) {
  return datum_is_list(d) && list_length(d) == 2 &&
         datum_is_the_symbol(d->list_head, val1) &&
         datum_is_the_symbol(d->list_tail->list_head, val2);
}

void prog_join(prog *a, prog *b, prog *e) {
  if (a->type != PROG_END || b->type != PROG_END) {
    fprintf(stderr, "wrong usage\n");
    exit(1);
  }
  a->type = PROG_NOP;
  a->nop_next = e;
  b->type = PROG_NOP;
  b->nop_next = e;
}

void prog_append_put_const(prog **begin, datum *val) {
  (*begin)->type = PROG_PUT_CONST;
  (*begin)->put_const_value = val;
  (*begin)->put_const_next = prog_make();
  *begin = (*begin)->put_const_next;
}

void prog_append_put_routine(prog **begin, datum *val) {
  (*begin)->type = PROG_PUT_ROUTINE;
  (*begin)->put_routine_value = val;
  (*begin)->put_routine_next = prog_make();
  *begin = (*begin)->put_routine_next;
}

void prog_append_put_var(prog **begin, datum *val) {
  (*begin)->type = PROG_PUT_VAR;
  (*begin)->put_var_value = val;
  (*begin)->put_var_next = prog_make();
  *begin = (*begin)->put_var_next;
}

void prog_append_args(prog **begin) {
  (*begin)->type = PROG_ARGS;
  (*begin)->args_next = prog_make();
  *begin = (*begin)->args_next;
}

void prog_append_call(prog **begin, bool hat) {
  (*begin)->type = PROG_CALL;
  (*begin)->call_hat = hat;
  (*begin)->call_next = prog_make();
  *begin = (*begin)->call_next;
}

void prog_append_collect(prog **begin) {
  (*begin)->type = PROG_COLLECT;
  (*begin)->collect_next = prog_make();
  *begin = (*begin)->collect_next;
}

void prog_append_pop(prog **begin, datum *var) {
  (*begin)->type = PROG_POP;
  (*begin)->pop_var = var;
  (*begin)->pop_next = prog_make();
  *begin = (*begin)->pop_next;
}

void prog_append_pop_prog(prog **begin, datum *var) {
  (*begin)->type = PROG_POP_PROG;
  (*begin)->pop_prog_var = var;
  (*begin)->pop_prog_next = prog_make();
  *begin = (*begin)->pop_prog_next;
}

void prog_append_return(prog **begin, bool hat) {
  (*begin)->type = PROG_RETURN;
  (*begin)->return_hat = hat;
  *begin = prog_make();
}

void prog_append_yield(prog **begin, bool hat) {
  (*begin)->type = PROG_YIELD;
  (*begin)->yield_hat = hat;
  (*begin)->yield_next = prog_make();
  *begin = (*begin)->yield_next;
}

char *prog_append_require(prog **begin, datum *src,
                           fdatum (*module_source)(char *module)) {
  prog *pr = prog_make();
  char *err = prog_init_module(pr, src, module_source);
  if (err != NULL) {
    return err;
  }
  datum *r = datum_make_routine(pr, state_make_builtins());
  prog_append_args(begin);
  prog_append_put_const(begin, r);
  prog_append_collect(begin);
  prog_append_call(begin, false); // TODO(harius): bare call
  return NULL;
}

char *prog_append_statement(prog **begin, datum *stmt,
                   fdatum (*module_source)(char *module)) {
  if ((*begin)->type != PROG_END) {
    return "expected an end state";
  }
  if (datum_is_constant(stmt)) {
    prog_append_put_const(begin, stmt);
    return NULL;
  }
  if (datum_is_symbol(stmt)) {
    prog_append_put_var(begin, stmt);
    return NULL;
  }
  if (!datum_is_list(stmt)) {
    return "this datum cannot be a statement";
  }
  if (datum_is_nil(stmt)) {
    return "an empty list is not a statement";
  }
  datum *op = stmt->list_head;

  if (datum_is_the_symbol(op, "if")) {
    if (list_length(stmt->list_tail) != 3) {
      return "if should have three args";
    }
    char *err;
    err = prog_append_statement(begin, stmt->list_tail->list_head, module_source);
    if (err != NULL) {
      return err;
    }
    (*begin)->type = PROG_IF;

    prog *true_end = prog_make(), *false_end = prog_make();
    (*begin)->if_true = true_end;
    (*begin)->if_false = false_end;
    err = prog_append_statement(&true_end, stmt->list_tail->list_tail->list_head,
                       module_source);
    if (err != NULL) {
      return err;
    }
    err = prog_append_statement(&false_end,
                       stmt->list_tail->list_tail->list_tail->list_head,
                       module_source);
    if (err != NULL) {
      return err;
    }
    *begin = prog_make();
    prog_join(true_end, false_end, *begin);
    return NULL;
  }
  if (datum_is_the_symbol(op, "progn")) {
    prog_append_put_const(begin, datum_make_void());
    for (datum *rest = stmt->list_tail; !datum_is_nil(rest);
         rest = rest->list_tail) {
      prog_append_pop(begin, NULL);
      datum *step = rest->list_head;
      char *err = prog_append_statement(begin, step, module_source);
      if (err != NULL) {
        return err;
      }
    }
    return NULL;
  }
  if (datum_is_the_symbol(op, "quote")) {
    if (list_length(stmt->list_tail) != 1) {
      return "quote should have a single arg";
    }
    prog_append_put_const(begin, stmt->list_tail->list_head);
    return NULL;
  }
  if (datum_is_the_symbol(op, "def")) {
    if (list_length(stmt->list_tail) != 2) {
      return "def should have two args";
    }
    char *err = prog_append_statement(begin, stmt->list_tail->list_tail->list_head,
                             module_source);
    if (err != NULL) {
      return err;
    }
    prog_append_pop(begin, stmt->list_tail->list_head);
    prog_append_put_const(begin, datum_make_void());
    return NULL;
  }
  if (datum_is_the_symbol(op, "builtin.defn")) {
    if (list_length(stmt->list_tail) != 2) {
      return "defn should have two args";
    }
    prog *s = prog_make();
    char *err = prog_init_routine(s, stmt->list_tail->list_tail->list_head, module_source);
    if (err != NULL) {
      return err;
    }
    datum *f = datum_make_routine(s, NULL); // The null state will be overriden at runtime.
    prog_append_put_const(begin, f);
    prog_append_pop_prog(begin, stmt->list_tail->list_head);
    prog_append_put_const(begin, datum_make_void());
    return NULL;
  }
  if (datum_is_the_symbol(op, "builtin.fn")) {
    if (list_length(stmt->list_tail) != 1) {
      return "fn should have one arg";
    }

    prog *s = prog_make();
    char *err = prog_init_routine(s, stmt->list_tail->list_head, module_source);
    if (err != NULL) {
      return err;
    }
    datum *f = datum_make_routine(s, NULL); // The null state will be overriden at runtime.
    prog_append_put_routine(begin, f);
    return NULL;
  }
  if (datum_is_the_symbol(op, "require")) {
    if (list_length(stmt->list_tail) != 1 ||
        !datum_is_bytestring(stmt->list_tail->list_head)) {
      return "require should have a single string arg";
    }
    char *pkg = stmt->list_tail->list_head->bytestring_value;
    if (module_source == NULL) {
      return "require was used in a context where it's not supported";
    }
    fdatum pkg_src = module_source(pkg);
    if (fdatum_is_panic(pkg_src)) {
      return pkg_src.panic_message;
    }
    return prog_append_require(begin, pkg_src.ok_value, module_source);
  }
  if (datum_is_the_symbol(op, "return") ||
      datum_is_the_symbol_pair(op, "hat", "return")) {
    bool hat = datum_is_the_symbol_pair(op, "hat", "return");
    if (list_length(stmt->list_tail) != 1) {
      return "return should have a single arg";
    }
    char *err = prog_append_statement(begin, stmt->list_tail->list_head, module_source);
    if (err != NULL) {
      return err;
    }
    prog_append_return(begin, hat);
    return NULL;
  }
  if (datum_is_the_symbol(op, "yield") ||
      datum_is_the_symbol_pair(op, "hat", "yield")) {
    bool hat = datum_is_the_symbol_pair(op, "hat", "yield");
    if (list_length(stmt->list_tail) != 1) {
      return "yield should have a single arg";
    }
    char *err = prog_append_statement(begin, stmt->list_tail->list_head, module_source);
    if (err != NULL) {
      return err;
    }
    prog_append_yield(begin, hat);
    return NULL;
  }
  if (datum_is_the_symbol(op, "backquote")) {
    if (list_length(stmt->list_tail) != 1) {
      return "backquote should have a single arg";
    }
    return prog_append_backquoted_statement(begin, stmt->list_tail->list_head,
                                   module_source);
  }

  datum *fn = stmt->list_head;
  bool hash = false;
  bool hat = false;
  for (; datum_is_list(fn) && list_length(fn) == 2 &&
         datum_is_symbol(fn->list_head);
       fn = fn->list_tail->list_head) {
    char *tag = fn->list_head->symbol_value;
    if (!strcmp(tag, "hash")) {
      hash = true;
    } else if (!strcmp(tag, "hat")) {
      hat = true;
    } else {
      break;
    }
  }
  prog_append_args(begin);
  char *err = prog_append_statement(begin, fn, module_source);
  if (err != NULL) {
    return err;
  }
  for (datum *rest_args = stmt->list_tail; !datum_is_nil(rest_args);
       rest_args = rest_args->list_tail) {
    datum *arg = rest_args->list_head;
    if (hash) {
      prog_append_put_const(begin, arg);
    } else {
      char *err = prog_append_statement(begin, arg, module_source);
      if (err != NULL) {
        return err;
      }
    }
  }
  prog_append_collect(begin);
  prog_append_call(begin, hat);
  return NULL;
}

LOCAL bool datum_is_constant(datum *d) {
  return (datum_is_integer(d) || datum_is_bytestring(d) ||
          (datum_is_symbol(d) && d->symbol_value[0] == ':'));
}

char *prog_append_backquoted_statement(prog **begin, datum *stmt,
                              fdatum (*module_source)(char *module)) {
  if (!datum_is_list(stmt)) {
    prog_append_put_const(begin, stmt);
    return NULL;
  }
  prog_append_args(begin);
  for (datum *rest_elems = stmt; !datum_is_nil(rest_elems);
       rest_elems = rest_elems->list_tail) {
    datum *elem = rest_elems->list_head;
    char *err;
    if (datum_is_list(elem) && list_length(elem) == 2 &&
        datum_is_the_symbol(elem->list_head, "tilde")) {
      err = prog_append_statement(begin, elem->list_tail->list_head, module_source);
    } else {
      err = prog_append_backquoted_statement(begin, elem, module_source);
    }
    if (err != NULL) {
      return err;
    }
  }
  prog_append_collect(begin);
  return NULL;
}

char *prog_init_routine(prog *s, datum *stmt, fdatum (*module_source)(char *module)) {
  prog_append_pop(&s, datum_make_symbol("args"));
  return prog_append_statement(&s, stmt, module_source);
}

void switch_context(routine *c, routine b, datum *v) {
  *c = b;
  state_stack_put(&c->state_, v);
}

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
      if (datum_is_routine(fn)) {
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
          c.state_ = state_change_parent(
              c.state_, parent_cont.state_->hat_parent, true);
        }
      } else if (datum_is_pointer(fn)) {
        if (c.prog_->call_hat) {
          return fstate_make_panic("hat-call makes no sense for native calls");
        }
        fdatum res = pointer_call(fn, args);
        if (fdatum_is_panic(res)) {
          return fstate_make_panic(res.panic_message);
        }
        state_stack_put(&c.state_, res.ok_value);
        c.prog_ = c.prog_->call_next;
      } else {
        return fstate_make_panic("non-callable datum");
      }
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

bool datum_is_nil(datum *e) { return e->type == DATUM_NIL; }

bool datum_is_list(datum *e) {
  return e->type == DATUM_NIL || e->type == DATUM_LIST;
}

bool datum_is_symbol(datum *e) { return e->type == DATUM_SYMBOL; }

bool datum_is_integer(datum *e) { return e->type == DATUM_INTEGER; }

bool datum_is_bytestring(datum *e) { return e->type == DATUM_BYTESTRING; }

bool datum_is_routine(datum *e) { return e->type == DATUM_ROUTINE; }

bool datum_is_pointer(datum *e) { return e->type == DATUM_POINTER; }

bool datum_is_void(datum *e) { return e->type == DATUM_VOID; }

datum *datum_make_nil() {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_NIL;
  return e;
}

datum *datum_make_list(datum *head, datum *tail) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_LIST;
  e->list_head = head;
  e->list_tail = tail;
  return e;
}

datum *datum_make_list_1(datum *head) {
  return datum_make_list(head, datum_make_nil());
}

datum *datum_make_list_2(datum *head, datum *second) {
  return datum_make_list(head, datum_make_list_1(second));
}

datum *datum_make_list_3(datum *head, datum *second, datum *third) {
  return datum_make_list(head, datum_make_list_2(second, third));
}

datum *datum_make_symbol(char *name) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_SYMBOL;
  size_t length = strlen(name);
  e->symbol_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->symbol_value[i] = name[i];
  }
  return e;
}

datum *datum_make_bytestring(char *text) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_BYTESTRING;
  size_t length = strlen(text);
  e->bytestring_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->bytestring_value[i] = text[i];
  }
  return e;
}

datum *datum_make_int(int64_t value) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_INTEGER;
  e->integer_value = value;
  return e;
}

datum *datum_make_routine(prog *s, state *lexical_bindings) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_ROUTINE;
  e->routine_value.prog_ = s;
  e->routine_value.state_ = lexical_bindings;
  return e;
}

datum *datum_make_pointer(void *data, datum *signature) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_POINTER;
  e->pointer_descriptor = signature;
  e->pointer_value = data;
  return e;
}

datum *datum_make_pointer_to_pointer(void **ptr) {
  return datum_make_pointer(ptr, datum_make_symbol("pointer"));
}

datum *datum_make_void() {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_VOID;
  return e;
}

bool read_result_is_ok(read_result x) { return x.type == READ_RESULT_OK; }

bool read_result_is_panic(read_result x) {
  return x.type == READ_RESULT_PANIC;
}

bool read_result_is_eof(read_result x) { return x.type == READ_RESULT_EOF; }

bool read_result_is_right_paren(read_result x) {
  return x.type == READ_RESULT_RIGHT_PAREN;
}

read_result read_result_make_ok(datum *e) {
  read_result result = {.type = READ_RESULT_OK, .ok_value = e};
  return result;
}

read_result read_result_make_panic(char *message) {
  read_result result = {.type = READ_RESULT_PANIC, .panic_message = message};
  return result;
}

read_result read_result_make_eof(void) {
  read_result result = {.type = READ_RESULT_EOF};
  return result;
}

read_result read_result_make_right_paren(void) {
  read_result result = {.type = READ_RESULT_RIGHT_PAREN};
  return result;
}

bool is_whitespace(char c) { return isspace(c) || c == ','; }

bool is_allowed_inside_symbol(char c) {
  return isalnum(c) || c == '.' || c == '-' || c == '_' || c == ':' || c == '+';
}

bool consume_control_sequence(char c, datum **form) {
  if (c == '\'') {
    *form = datum_make_symbol("quote");
    return true;
  }
  if (c == '`') {
    *form = datum_make_symbol("backquote");
    return true;
  }
  if (c == '~') {
    *form = datum_make_symbol("tilde");
    return true;
  }
  if (c == '!') {
    *form = datum_make_symbol("bang");
    return true;
  }
  if (c == '#') {
    *form = datum_make_symbol("hash");
    return true;
  }
  if (c == '^') {
    *form = datum_make_symbol("hat");
    return true;
  }
  return false;
}

read_result datum_read(FILE *strm) {
  char c;
  for (; !feof(strm) && is_whitespace(c = getc(strm));) {
  }
  if (feof(strm)) {
    return read_result_make_eof();
  }
  if (c == ')') {
    return read_result_make_right_paren();
  }
  if (c == '(') {
    read_result elem;
    datum *list = datum_make_nil();
    datum **end_marker = &list;
    for (;;) {
      while (read_result_is_ok(elem = datum_read(strm))) {
        *end_marker = datum_make_list_1(elem.ok_value);
        end_marker = &((*end_marker)->list_tail);
      }
      if (read_result_is_right_paren(elem)) {
        return read_result_make_ok(list);
      }
      if (read_result_is_eof(elem)) {
        return read_result_make_panic("expected ')', got EOS");
      } else {
        break;
      }
    }
    return elem;
  }
  if (isdigit(c) || c == '-') {
    int64_t sign = 1;
    char h;
    if (c == '-') {
      sign = -1;
      c = getc(strm);
      if (!isdigit(c)) {
        return read_result_make_panic("expected a number after unary minus");
      }
    }
    int val = c - '0';
    for (; !feof(strm) && isdigit(h = getc(strm));) {
      val *= 10;
      val += h - '0';
    }
    if (!feof(strm)) {
      ungetc(h, strm);
    }
    return read_result_make_ok(datum_make_int(sign * val));
  }
  if (is_allowed_inside_symbol(c)) {
    char *nm = malloc(128);
    nm[0] = c;
    int i;
    char x;
    for (i = 1; !feof(strm) && is_allowed_inside_symbol(x = getc(strm));
         nm[i++] = x) {
    }
    if (!feof(strm)) {
      ungetc(x, strm);
    }
    nm[i] = '\0';
    datum *sym = datum_make_symbol(nm);
    return read_result_make_ok(sym);
  }
  if (c == '"') {
    char literal[256];
    char x;
    size_t i;
    for (i = 0; (x = getc(strm)) != '"'; ++i) {
      if (x == '\\') {
        x = getc(strm);
        if (x == 'n') {
          literal[i] = '\n';
          continue;
        }
        return read_result_make_panic("unknown escape code");
      }
      literal[i] = x;
    }
    literal[i] = '\0';
    return read_result_make_ok(datum_make_bytestring(literal));
  }
  datum *form;
  if (consume_control_sequence(c, &form)) {
    read_result v = datum_read(strm);
    if (read_result_is_panic(v)) {
      return v;
    }
    if (!read_result_is_ok(v)) {
      return read_result_make_panic(
          "expected an expression after a control character");
    }
    datum *res = datum_make_list_1(form);
    res->list_tail = datum_make_list_1(v.ok_value);
    return read_result_make_ok(res);
  }
  char *err = malloc(1024);
  sprintf(err, "unexpected symbol: 0x%x", c);
  return read_result_make_panic(err);
}

char *datum_repr(datum *e) {
  char *buf = malloc(1024 * sizeof(char));
  char *end = buf;
  if (datum_is_integer(e)) {
    sprintf(buf, "%" PRId64, e->integer_value);
  } else if (datum_is_list(e)) {
    end += sprintf(end, "(");
    for (datum *item = e; !datum_is_nil(item); item = item->list_tail) {
      end += sprintf(end, "%s ", datum_repr(item->list_head));
    }
    end += sprintf(end, ")");
  } else if (datum_is_symbol(e)) {
    end += sprintf(end, "%s", e->symbol_value);
  } else if (datum_is_bytestring(e)) {
    end += sprintf(end, "\"%s\"", e->bytestring_value);
  } else if (datum_is_routine(e)) {
    end += sprintf(end, "<form>");
  } else if (datum_is_pointer(e)) {
    end += sprintf(end, "<externcdata %p %s>", e->pointer_value,
                   datum_repr(e->pointer_descriptor));
  } else if (datum_is_void(e)) {
    end += sprintf(end, "<void>");
  } else {
    sprintf(buf, "<fmt not implemented>");
  }
  return buf;
}

bool fdatum_is_ok(fdatum result) { return result.type == FDATUM_OK; }

bool fdatum_is_panic(fdatum result) { return result.type == FDATUM_PANIC; }

fdatum fdatum_make_ok(datum *v) {
  fdatum result = {.type = FDATUM_OK, .ok_value = v};
  return result;
}

fdatum fdatum_make_panic(char *message) {
  fdatum result = {.type = FDATUM_PANIC, .panic_message = message};
  return result;
}

bool fstate_is_ok(fstate result) { return result.type == FSTATE_OK; }

bool fstate_is_panic(fstate result) { return result.type == FSTATE_PANIC; }

fstate fstate_make_ok(state *v) {
  fstate result = {.type = FSTATE_OK, .ok_value = v};
  return result;
}

fstate fstate_make_panic(char *message) {
  fstate result = {.type = FSTATE_PANIC, .panic_message = message};
  return result;
}

state *state_make(datum *vars, datum *stack, routine parent,
                    routine hat_parent) {
  state *res = malloc(sizeof(state));
  res->vars = vars;
  res->stack = stack;
  res->parent = parent;
  res->hat_parent = hat_parent;
  return res;
}

state *state_make_fresh() {
  routine zero = routine_make_null();
  return state_make(datum_make_nil(), datum_make_nil(), zero, zero);
}

state *state_set_var(state *ns, datum *symbol, datum *value) {
  datum *kv = datum_make_list_3(symbol, datum_make_symbol(":value"), value);
  return state_make(datum_make_list(kv, ns->vars), ns->stack, ns->parent,
                    ns->hat_parent);
}

state *state_set_fn(state *ns, datum *symbol, datum *value) {
  datum *kv = datum_make_list_3(symbol, datum_make_symbol(":fn"), value);
  return state_make(datum_make_list(kv, ns->vars), ns->stack, ns->parent,
                    ns->hat_parent);
}

datum *namespace_cell_get_value(datum *cell, state *ns) {
  datum *raw_value = cell->list_tail->list_head;
  if (!strcmp(cell->list_head->symbol_value, ":value")) {
    return raw_value;
  } else if (!strcmp(cell->list_head->symbol_value, ":fn")) {
    if (!datum_is_routine(raw_value)) {
      fprintf(stderr, "namespace implementation error");
      exit(EXIT_FAILURE);
    }
    state *routine_ns = state_make(ns->vars, datum_make_nil(),
                                     routine_make_null(), routine_make_null());
    return datum_make_routine(raw_value->routine_value.prog_, routine_ns);
  } else {
    fprintf(stderr, "namespace implementation error");
    exit(EXIT_FAILURE);
  }
}

fdatum state_get_var(state *ns, datum *symbol) {
  for (datum *cur = ns->vars; !datum_is_nil(cur); cur = cur->list_tail) {
    datum *entry = cur->list_head;
    if (!strcmp(entry->list_head->symbol_value, symbol->symbol_value)) {
      datum *cell = entry->list_tail;
      return fdatum_make_ok(namespace_cell_get_value(cell, ns));
    }
  }
  char *msg = malloc(1024);
  sprintf(msg, "unbound symbol: %s", symbol->symbol_value);
  return fdatum_make_panic(msg);
}

fdatum list_map(fdatum (*fn)(datum *, state *), datum *items,
                  state *ctxt) {
  if (!datum_is_list(items)) {
    return fdatum_make_panic("expected a list");
  }
  datum *evaled_items = datum_make_nil();
  datum **tail = &evaled_items;
  for (datum *arg = items; !datum_is_nil(arg); arg = arg->list_tail) {
    fdatum evaled_arg = fn(arg->list_head, ctxt);
    if (fdatum_is_panic(evaled_arg)) {
      return evaled_arg;
    }
    *tail = datum_make_list_1(evaled_arg.ok_value);
    tail = &((*tail)->list_tail);
  }
  return fdatum_make_ok(evaled_items);
}

bool ffi_type_init(ffi_type **type, datum *definition) {
  if (!datum_is_symbol(definition)) {
    return false;
  }
  if (!strcmp(definition->symbol_value, "string")) {
    *type = &ffi_type_pointer;
    return true;
  }
  if (!strcmp(definition->symbol_value, "pointer")) {
    *type = &ffi_type_pointer;
    return true;
  }
  if (!strcmp(definition->symbol_value, "sizet")) {
    *type = &ffi_type_uint64;
    return true;
  }
  if (!strcmp(definition->symbol_value, "int")) {
    *type = &ffi_type_sint;
    return true;
  }
  if (!strcmp(definition->symbol_value, "datum")) {
    *type = &ffi_type_pointer;
    return true;
  }
  if (!strcmp(definition->symbol_value, "val")) {
    *type = malloc(sizeof(ffi_type));
    (*type)->type = FFI_TYPE_STRUCT;
    (*type)->size = 0; // Lost 5 hours debugging non-deterministic failures on
                       // Mac before adding this line.
    (*type)->alignment = 0;
    ffi_type **elements = malloc(4 * sizeof(ffi_type *));
    elements[0] = &ffi_type_sint;
    elements[1] = &ffi_type_pointer;
    elements[2] = &ffi_type_pointer;
    elements[3] = NULL;
    (*type)->elements = elements;
    return type;
  }
  return false;
}

char *pointer_ffi_init_cif(datum *f, ffi_cif *cif) {
  datum *sig = f->pointer_descriptor;
  if (!datum_is_list(sig) || datum_is_nil(sig) ||
      datum_is_nil(sig->list_tail) ||
      !datum_is_nil(sig->list_tail->list_tail)) {
    return "the signature should be a two-item list";
  }
  ffi_type **arg_types = malloc(sizeof(ffi_type *) * 32);
  int arg_count = 0;
  datum *arg_def;
  for (arg_def = f->pointer_descriptor->list_head; !datum_is_nil(arg_def);
       arg_def = arg_def->list_tail) {
    if (!ffi_type_init(arg_types + arg_count, arg_def->list_head)) {
      return "something wrong with the argument type signature";
    }
    ++arg_count;
  }
  ffi_type *ret_type;
  if (!ffi_type_init(&ret_type, sig->list_tail->list_head)) {
    return "something wrong with the return type signature";
  }
  ffi_status status;
  if ((status = ffi_prep_cif(cif, FFI_DEFAULT_ABI, arg_count, ret_type,
                             arg_types)) != FFI_OK) {
    return "something went wrong during ffi_prep_cif";
  }
  return NULL;
}

char *pointer_ffi_serialize_args(datum *f, datum *args, void **cargs) {
  int arg_cnt = 0;
  datum *arg = args;
  for (datum *argt = f->pointer_descriptor->list_head; !datum_is_nil(argt);
       argt = argt->list_tail) {
    if (datum_is_nil(arg)) {
      return "too few arguments";
    }
    if (!strcmp(argt->list_head->symbol_value, "string")) {
      if (!datum_is_bytestring(arg->list_head)) {
        return "string expected, got something else";
      }
      cargs[arg_cnt] = &arg->list_head->bytestring_value;
    } else if (!strcmp(argt->list_head->symbol_value, "sizet")) {
      if (!datum_is_integer(arg->list_head)) {
        return "int expected, got something else";
      }
      cargs[arg_cnt] = &arg->list_head->integer_value;
    } else if (!strcmp(argt->list_head->symbol_value, "pointer")) {
      datum *sig;
      if (!datum_is_pointer(arg->list_head) ||
          !datum_is_symbol(sig = arg->list_head->pointer_descriptor) ||
          strcmp(sig->symbol_value, "pointer")) {
        return "pointer expected, got something else";
      }
      cargs[arg_cnt] = arg->list_head->pointer_value;
    } else if (!strcmp(argt->list_head->symbol_value, "datum")) {
      cargs[arg_cnt] = &arg->list_head;
    } else {
      return "cannot load an argument";
    }
    arg = arg->list_tail;
    ++arg_cnt;
  }
  if (!datum_is_nil(arg)) {
    return "too much arguments";
  }
  return NULL;
}

fdatum pointer_ffi_call(datum *f, ffi_cif *cif, void **cargs) {
  void (*fn_ptr)(void) = __extension__(void (*)(void))(f->pointer_value);
  char *rettype = f->pointer_descriptor->list_tail->list_head->symbol_value;

  if (!strcmp(rettype, "pointer")) {
    void *res = malloc(sizeof(void *));
    ffi_call(cif, fn_ptr, res, cargs);
    return fdatum_make_ok(datum_make_pointer_to_pointer(res));
  }
  if (!strcmp(rettype, "sizet")) {
    void *res = malloc(sizeof(size_t));
    ffi_call(cif, fn_ptr, res, cargs);
    return fdatum_make_ok(datum_make_int(*(int64_t *)res));
  }
  if (!strcmp(rettype, "int")) {
    void *res = malloc(sizeof(int));
    ffi_call(cif, fn_ptr, res, cargs);
    return fdatum_make_ok(datum_make_int(*(int64_t *)res));
  }
  if (!strcmp(rettype, "val")) {
    fdatum res;
    ffi_call(cif, fn_ptr, &res, cargs);
    if (fdatum_is_panic(res)) {
      return fdatum_make_panic(res.panic_message);
    }
    return fdatum_make_ok(res.ok_value);
  }
  return fdatum_make_panic("unknown return type for extern func");
}

fdatum pointer_call(datum *f, datum *args) {
  ffi_cif cif;
  char *err = NULL;
  err = pointer_ffi_init_cif(f, &cif);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  void *cargs[32];
  err = pointer_ffi_serialize_args(f, args, cargs);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  return pointer_ffi_call(f, &cif, cargs);
}

char* state_value_eval(state **ctxt, datum *v, fdatum (*module_source)(char *module)) {
  prog *s = prog_make();
  prog *pe = s;
  char *err = prog_append_statement(&pe, v, module_source);
  if (err != NULL) {
    return err;
  }
  routine c = routine_make(s, *ctxt);
  fstate res = routine_run(c);
  if (fstate_is_panic(res)) {
    return res.panic_message;
  }
  *ctxt = res.ok_value;
  return NULL;
}

void state_value_put(state **ctxt, datum *v) {
  state_stack_put(ctxt, v);
}

datum *state_value_pop(state **ctxt) {
  return state_stack_pop(ctxt);
}

fdatum builtin_concat_bytestrings(datum *x, datum *y) {
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

fdatum builtin_add(datum *x, datum *y) {
  if (!datum_is_integer(x) || !datum_is_integer(y)) {
    return fdatum_make_panic("expected integers");
  }
  return fdatum_make_ok(datum_make_int(x->integer_value + y->integer_value));
}

fdatum builtin_cons(datum *head, datum *tail) {
  if (!datum_is_list(tail)) {
    return fdatum_make_panic("cons requires a list as a second argument");
  }
  return fdatum_make_ok(datum_make_list(head, tail));
}

fdatum builtin_head(datum *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("car expects a nonempty list");
  }
  return fdatum_make_ok(list->list_head);
}

fdatum builtin_tail(datum *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("cdr expects a nonempty list");
  }
  return fdatum_make_ok(list->list_tail);
}

datum *state_list_vars(state *ns) {
  datum *result = datum_make_nil();
  datum **nil = &result;
  for (datum *cur = ns->vars; !datum_is_nil(cur); cur = cur->list_tail) {
    datum *entry = cur->list_head;
    datum *key = entry->list_head;
    datum *cell = entry->list_tail;
    datum *val = namespace_cell_get_value(cell, ns);
    datum *keyval = datum_make_list_2(key, val);
    *nil = datum_make_list_1(keyval);
    nil = &((*nil)->list_tail);
  }
  return result;
}

fdatum builtin_shared_library(datum *library_name) {
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

fdatum builtin_extern_pointer(datum *shared_library, datum *name,
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

fdatum builtin_repr(datum *v) {
  return fdatum_make_ok(datum_make_bytestring(datum_repr(v)));
}

bool datum_eq(datum *x, datum *y) {
  if (datum_is_symbol(x) && datum_is_symbol(y)) {
    if (!strcmp(x->symbol_value, y->symbol_value)) {
      return true;
    }
    return false;
  }
  if (datum_is_integer(x) && datum_is_integer(y)) {
    if (x->integer_value == y->integer_value) {
      return true;
    }
    return false;
  }
  if (datum_is_bytestring(x) && datum_is_bytestring(y)) {
    if (!strcmp(x->bytestring_value, y->bytestring_value)) {
      return true;
    }
    return false;
  }
  if (datum_is_list(x) && datum_is_list(y)) {
    if (datum_is_nil(x) && datum_is_nil(y)) {
      return true;
    }
    if (datum_is_nil(x) || datum_is_nil(y)) {
      return false;
    }
    return datum_eq(x->list_head, y->list_head) &&
           datum_eq(x->list_tail, y->list_tail);
  }
  return false;
}

fdatum builtin_eq(datum *x, datum *y) {
  datum *t = datum_make_list_1(datum_make_nil());
  datum *f = datum_make_nil();
  if (datum_eq(x, y)) {
    return fdatum_make_ok(t);
  }
  return fdatum_make_ok(f);
}

fdatum builtin_annotate(datum *arg_value) {
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

fdatum builtin_is_constant(datum *arg_value) {
  if (datum_is_constant(arg_value)) {
    return fdatum_make_ok(datum_make_list_1(datum_make_nil()));
  }
  return fdatum_make_ok(datum_make_nil());
}

fdatum builtin_panic(datum *arg_value) {
  if (!datum_is_bytestring(arg_value)) {
    return fdatum_make_panic("panic expects a bytestring");
  }
  return fdatum_make_panic(arg_value->bytestring_value);
}

void namespace_def_extern_fn(state **ctxt, char *name, fdatum (*fn)(),
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

state *state_make_builtins() {
  state *ns = state_make_fresh();

  namespace_def_extern_fn(&ns, "panic", builtin_panic, 1);
  namespace_def_extern_fn(&ns, "shared-library", builtin_shared_library, 1);
  namespace_def_extern_fn(&ns, "extern-pointer", builtin_extern_pointer, 3);
  namespace_def_extern_fn(&ns, "cons", builtin_cons, 2);
  namespace_def_extern_fn(&ns, "head", builtin_head, 1);
  namespace_def_extern_fn(&ns, "tail", builtin_tail, 1);
  namespace_def_extern_fn(&ns, "eq", builtin_eq, 2);
  namespace_def_extern_fn(&ns, "annotate", builtin_annotate, 1);
  namespace_def_extern_fn(&ns, "is-constant", builtin_is_constant, 1);
  namespace_def_extern_fn(&ns, "repr", builtin_repr, 1);
  namespace_def_extern_fn(&ns, "concat-bytestrings", builtin_concat_bytestrings,
                          2);
  namespace_def_extern_fn(&ns, "+", builtin_add, 2);

  return ns;
}
