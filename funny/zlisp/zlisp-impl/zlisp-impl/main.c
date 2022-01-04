// zlisp interpreter.
#include <zlisp-impl/main.h>

#include <ctype.h>
#include <dlfcn.h>
#include <ffi.h>
#include <inttypes.h>
#include <libgen.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

prog_t *prog_make() {
  prog_t *res = malloc(sizeof(prog_t));
  res->type = PROG_END;
  return res;
}

void prog_append_module_end(prog_t **begin) {
  (*begin)->type = PROG_MODULE_END;
  *begin = prog_make();
}

char *prog_append_statement(prog_t **begin, datum_t *stmt,
                   fdatum_t (*module_source)(char *module));

char *prog_init_module(prog_t *s, datum_t *source,
                            fdatum_t (*module_source)(char *module)) {
  for (datum_t *rest = source; !datum_is_nil(rest); rest = rest->list_tail) {
    datum_t *stmt = rest->list_head;
    char *err = prog_append_statement(&s, stmt, module_source);
    if (err != NULL) {
      return err;
    }
  }
  prog_append_module_end(&s);
  return NULL;
}

routine_t routine_make(prog_t *s, state_t *ctxt) {
  routine_t res = {.prog = s, .state = ctxt};
  return res;
}

routine_t routine_make_null() {
  routine_t res = {};
  return res;
}

bool routine_is_null(routine_t r) { return r.prog == NULL && r.state == NULL; }

routine_t state_get_parent(state_t *ns, bool hat) {
  if (hat) {
    return ns->hat_parent;
  }
  return ns->parent;
}

state_t *state_change_parent(state_t *ns, routine_t new_parent, bool hat) {
  if (hat) {
    return state_make(ns->vars, ns->stack, ns->parent, new_parent);
  }
  return state_make(ns->vars, ns->stack, new_parent, ns->hat_parent);
}

int list_length(datum_t *seq) {
  if (!datum_is_list(seq)) {
    return -1;
  }
  int res;
  for (res = 0; !datum_is_nil(seq); seq = seq->list_tail, ++res) {
  }
  return res;
}

static bool datum_is_the_symbol(datum_t *d, char *val) {
  return datum_is_symbol(d) && !strcmp(d->symbol_value, val);
}

static bool datum_is_the_symbol_pair(datum_t *d, char *val1, char *val2) {
  return datum_is_list(d) && list_length(d) == 2 &&
         datum_is_the_symbol(d->list_head, val1) &&
         datum_is_the_symbol(d->list_tail->list_head, val2);
}

void prog_join(prog_t *a, prog_t *b, prog_t *e) {
  if (a->type != PROG_END || b->type != PROG_END) {
    fprintf(stderr, "wrong usage\n");
    exit(1);
  }
  a->type = PROG_NOP;
  a->nop_next = e;
  b->type = PROG_NOP;
  b->nop_next = e;
}

void prog_append_put_const(prog_t **begin, datum_t *val) {
  (*begin)->type = PROG_PUT_CONST;
  (*begin)->put_const_value = val;
  (*begin)->put_const_next = prog_make();
  *begin = (*begin)->put_const_next;
}

void prog_append_put_var(prog_t **begin, datum_t *val) {
  (*begin)->type = PROG_PUT_VAR;
  (*begin)->put_var_value = val;
  (*begin)->put_var_next = prog_make();
  *begin = (*begin)->put_var_next;
}

void prog_append_args(prog_t **begin) {
  (*begin)->type = PROG_ARGS;
  (*begin)->args_next = prog_make();
  *begin = (*begin)->args_next;
}

void prog_append_call(prog_t **begin, bool hat) {
  (*begin)->type = PROG_CALL;
  (*begin)->call_hat = hat;
  (*begin)->call_next = prog_make();
  *begin = (*begin)->call_next;
}

void prog_append_pop(prog_t **begin, datum_t *var) {
  (*begin)->type = PROG_POP;
  (*begin)->pop_var = var;
  (*begin)->pop_next = prog_make();
  *begin = (*begin)->pop_next;
}

void prog_append_call_special(prog_t **begin,
                        fstate_t (*call_special_func)(datum_t *, state_t *)) {
  (*begin)->type = PROG_CALL_SPECIAL;
  (*begin)->call_special_func = call_special_func;
  (*begin)->call_special_next = prog_make();
  *begin = (*begin)->call_special_next;
}

void prog_append_return(prog_t **begin, bool hat) {
  (*begin)->type = PROG_RETURN;
  (*begin)->return_hat = hat;
  *begin = prog_make();
}

void prog_append_yield(prog_t **begin, bool hat) {
  (*begin)->type = PROG_YIELD;
  (*begin)->yield_hat = hat;
  (*begin)->yield_next = prog_make();
  *begin = (*begin)->yield_next;
}

fstate_t special_defn(datum_t *args, state_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail)) {
    return fstate_make_panic("defun expects exactly two arguments");
  }
  if (!datum_is_symbol(args->list_head)) {
    return fstate_make_panic("defun requires a symbol as a first argument");
  }
  ctxt = state_set_fn(ctxt, args->list_head, args->list_tail->list_head);
  ctxt = state_stack_put(ctxt, datum_make_void());
  return fstate_make_ok(ctxt);
}

fstate_t special_fn(datum_t *args, state_t *ctxt);

char *prog_init_routine(prog_t *s, datum_t *stmt);

char *prog_append_backquoted_statement(prog_t **begin, datum_t *stmt,
                              fdatum_t (*module_source)(char *module));

char *prog_append_require(prog_t **begin, datum_t *src,
                           fdatum_t (*module_source)(char *module)) {
  prog_t *pr = prog_make();
  char *err = prog_init_module(pr, src, module_source);
  if (err != NULL) {
    return err;
  }
  datum_t *r = datum_make_routine(pr, state_make_builtins());
  prog_append_put_const(begin, r);
  prog_append_args(begin);
  prog_append_call(begin, false); // TODO(harius): bare call
  return NULL;
}

char *prog_append_statement(prog_t **begin, datum_t *stmt,
                   fdatum_t (*module_source)(char *module)) {
  if ((*begin)->type != PROG_END) {
    return "expected an end state";
  }
  if (datum_is_symbol(stmt)) {
    prog_append_put_var(begin, stmt);
    return NULL;
  }
  if (datum_is_bytestring(stmt) || datum_is_integer(stmt)) {
    prog_append_put_const(begin, stmt);
    return NULL;
  }
  if (!datum_is_list(stmt)) {
    return "this datum cannot be a statement";
  }
  if (datum_is_nil(stmt)) {
    return "an empty list is not a statement";
  }
  datum_t *op = stmt->list_head;

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

    prog_t *true_end = prog_make(), *false_end = prog_make();
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
    for (datum_t *rest = stmt->list_tail; !datum_is_nil(rest);
         rest = rest->list_tail) {
      prog_append_pop(begin, NULL);
      datum_t *step = rest->list_head;
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
    prog_append_args(begin);
    prog_append_put_const(begin, stmt->list_tail->list_head);
    prog_append_put_const(begin, stmt->list_tail->list_tail->list_head);
    prog_append_call_special(begin, special_defn);
    return NULL;
  }
  if (datum_is_the_symbol(op, "builtin.fn")) {
    if (list_length(stmt->list_tail) != 1) {
      return "fn should have one arg";
    }

    prog_t *s = prog_make();
    char *err = prog_init_routine(s, stmt->list_tail->list_head);
    if (err != NULL) {
      return err;
    }
    datum_t *f = datum_make_routine(s, NULL); // The null state will be overriden at runtime.

    prog_append_args(begin);
    prog_append_put_const(begin, f);
    prog_append_call_special(begin, special_fn);
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
    fdatum_t pkg_src = module_source(pkg);
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

  datum_t *fn = stmt->list_head;
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
  char *err = prog_append_statement(begin, fn, module_source);
  if (err != NULL) {
    return err;
  }
  prog_append_args(begin);
  for (datum_t *rest_args = stmt->list_tail; !datum_is_nil(rest_args);
       rest_args = rest_args->list_tail) {
    datum_t *arg = rest_args->list_head;
    if (hash) {
      prog_append_put_const(begin, arg);
    } else {
      char *err = prog_append_statement(begin, arg, module_source);
      if (err != NULL) {
        return err;
      }
    }
  }
  prog_append_call(begin, hat);
  return NULL;
}

char *prog_append_backquoted_statement(prog_t **begin, datum_t *stmt,
                              fdatum_t (*module_source)(char *module)) {
  if (!datum_is_list(stmt)) {
    prog_append_put_const(begin, stmt);
    return NULL;
  }
  prog_append_put_var(begin, datum_make_symbol("list"));
  prog_append_args(begin);
  for (datum_t *rest_elems = stmt; !datum_is_nil(rest_elems);
       rest_elems = rest_elems->list_tail) {
    datum_t *elem = rest_elems->list_head;
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
  prog_append_call(begin, false);
  return NULL;
}

char *prog_init_routine(prog_t *s, datum_t *stmt) {
  prog_append_pop(&s, datum_make_symbol("args"));
  return prog_append_statement(&s, stmt, NULL);
}

fstate_t special_fn(datum_t *args, state_t *ctxt) {
  if (list_length(args) != 1) {
    return fstate_make_panic("fn expects a single argument");
  }
  if (!datum_is_routine(args->list_head)) {
    return fstate_make_panic("a routine was expected");
  }
  routine_t r = args->list_head->routine_value;
  r.state = state_make(ctxt->vars, datum_make_nil(),
                       routine_make_null(), routine_make_null());
  ctxt = state_stack_put(ctxt, datum_make_routine(r.prog, r.state));
  return fstate_make_ok(ctxt);
}

fdatum_t datum_eval_primitive(datum_t *e, state_t *ctxt) {
  if (datum_is_integer(e) || datum_is_bytestring(e)) {
    return fdatum_make_ok(e);
  }
  if (datum_is_symbol(e)) {
    if (e->symbol_value[0] == ':') {
      return fdatum_make_ok(e);
    }
    return state_get_var(ctxt, e);
  }
  return fdatum_make_panic("not a primitive");
}

fdatum_t pointer_call(datum_t *f, datum_t *args);

void switch_context(routine_t *c, routine_t b, datum_t *v) {
  *c = b;
  c->state = state_stack_put(c->state, v);
}

fstate_t routine_run(routine_t c) {
  for (;;) {
    // printf("%d\n", c.prog->type);
    switch (c.prog->type) {
    case PROG_END: {
      if (!routine_is_null(c.state->parent)) {
        return fstate_make_panic("reached the end state in a subroutine");
      }
      return fstate_make_ok(c.state);
    } break;
    case PROG_NOP: {
      c.prog = c.prog->nop_next;
    } break;
    case PROG_IF: {
      fdatum_t v = state_stack_peek(c.state);
      c.state = state_stack_pop(c.state);
      if (fdatum_is_panic(v)) {
        return fstate_make_panic(v.panic_message);
      }
      if (!datum_is_nil(v.ok_value)) {
        c.prog = c.prog->if_true;
      } else {
        c.prog = c.prog->if_false;
      }
    } break;
    case PROG_PUT_CONST: {
      c.state = state_stack_put(c.state, c.prog->put_const_value);
      c.prog = c.prog->put_const_next;
    } break;
    case PROG_PUT_VAR: {
      fdatum_t er = datum_eval_primitive(c.prog->put_var_value, c.state);
      if (fdatum_is_panic(er)) {
        return fstate_make_panic(er.panic_message);
      }
      c.state = state_stack_put(c.state, er.ok_value);
      c.prog = c.prog->put_var_next;
    } break;
    case PROG_POP: {
      fdatum_t v = state_stack_peek(c.state);
      if (fdatum_is_panic(v)) {
        return fstate_make_panic(v.panic_message);
      }
      c.state = state_stack_pop(c.state);
      if (c.prog->pop_var != NULL) {
        c.state = state_set_var(c.state, c.prog->pop_var, v.ok_value);
      }
      c.prog = c.prog->pop_next;
    } break;
    case PROG_ARGS: {
      c.state = state_stack_put(c.state, datum_make_symbol("__function_call"));
      c.prog = c.prog->args_next;
    } break;
    case PROG_CALL: {
      datum_t *args = datum_make_nil();
      fdatum_t arg;
      for (;;) {
        arg = state_stack_peek(c.state);
        c.state = state_stack_pop(c.state);
        if (fdatum_is_panic(arg)) {
          return fstate_make_panic(arg.panic_message);
        }
        if (datum_is_the_symbol(arg.ok_value, "__function_call")) {
          break;
        }
        args = datum_make_list(arg.ok_value, args);
      }
      fdatum_t fn = state_stack_peek(c.state);
      c.state = state_stack_pop(c.state);
      if (fdatum_is_panic(fn)) {
        return fstate_make_panic(fn.panic_message);
      }
      if (datum_is_routine(fn.ok_value)) {
        bool hat = c.prog->call_hat;
        routine_t parent_cont = routine_make(c.prog->call_next, c.state);
        switch_context(&c, fn.ok_value->routine_value, args);
        if (!routine_is_null(state_get_parent(c.state, hat))) {
          return fstate_make_panic(
              "attempt to call routine with existing parent");
        }
        c.state = state_change_parent(c.state, parent_cont, hat);
        if (!hat) {
          if (!routine_is_null(state_get_parent(c.state, true))) {
            return fstate_make_panic(
                "attempt to call routine with existing hat-parent");
          }
          c.state = state_change_parent(
              c.state, parent_cont.state->hat_parent, true);
        }
      } else if (datum_is_pointer(fn.ok_value)) {
        if (c.prog->call_hat) {
          return fstate_make_panic("hat-call makes no sense for native calls");
        }
        fdatum_t res = pointer_call(fn.ok_value, args);
        if (fdatum_is_panic(res)) {
          return fstate_make_panic(res.panic_message);
        }
        c.state = state_stack_put(c.state, res.ok_value);
        c.prog = c.prog->call_next;
      } else {
        return fstate_make_panic("non-callable datum");
      }
    } break;
    case PROG_RETURN: {
      routine_t hat_par = c.state->hat_parent;
      routine_t return_to;
      if (c.prog->return_hat) {
        return fstate_make_panic("^return not implemented yet");
      } else {
        return_to = c.state->parent;
      }
      if (routine_is_null(return_to)) {
        return fstate_make_panic("bad return");
      }
      fdatum_t res = state_stack_peek(c.state);
      if (fdatum_is_panic(res)) {
        return fstate_make_panic(res.panic_message);
      }
      switch_context(&c, return_to, res.ok_value);
      c.state->hat_parent =
          hat_par; /* Because the caller hat parent might be out-of-date.*/
    } break;
    case PROG_YIELD: {
      bool hat;
      routine_t yield_to;
      if (c.prog->yield_hat) {
        hat = true;
        yield_to = c.state->hat_parent;
      } else {
        hat = false;
        yield_to = c.state->parent;
      }
      if (routine_is_null(yield_to)) {
        return fstate_make_panic("bad yield");
      }
      c.state = state_change_parent(c.state, routine_make_null(), hat);
      fdatum_t res = state_stack_peek(c.state);
      if (fdatum_is_panic(res)) {
        return fstate_make_panic(res.panic_message);
      }
      c.state = state_stack_pop(c.state);
      datum_t *resume = datum_make_routine(c.prog->yield_next, c.state);
      datum_t *r = datum_make_list_2(res.ok_value, resume);
      switch_context(&c, yield_to, r);
    } break;
    case PROG_MODULE_END: {
      state_t *module_state = c.state;
      routine_t return_to = c.state->parent;
      if (routine_is_null(return_to)) {
        return fstate_make_ok(c.state);
      }
      fdatum_t res = state_stack_peek(c.state);
      if (fdatum_is_panic(res)) {
        return fstate_make_panic(res.panic_message);
      }
      switch_context(&c, return_to, datum_make_void());

      datum_t *imported_bindings = state_list_vars(module_state);
      for (; !datum_is_nil(imported_bindings);
           imported_bindings = imported_bindings->list_tail) {
        datum_t *sym = imported_bindings->list_head->list_head;
        datum_t *val = imported_bindings->list_head->list_tail->list_head;

        c.state = state_set_var(c.state, sym, val);
      }
    } break;
    case PROG_CALL_SPECIAL: {
      datum_t *sargs = datum_make_nil();
      fdatum_t sarg;
      while (sarg = state_stack_peek(c.state),
             c.state = state_stack_pop(c.state),
             !(fdatum_is_ok(sarg) && datum_is_symbol(sarg.ok_value) &&
               !strcmp(sarg.ok_value->symbol_value, "__function_call"))) {
        sargs = datum_make_list(sarg.ok_value, sargs);
      }
      fstate_t sres = c.prog->call_special_func(sargs, c.state);
      if (fstate_is_panic(sres)) {
        return sres;
      }
      c.state = sres.ok_value;
      c.prog = c.prog->call_special_next;
    } break;
    default: {
      return fstate_make_panic("unhandled state type");
    } break;
    }
  }
}

bool datum_is_nil(datum_t *e) { return e->type == DATUM_NIL; }

bool datum_is_list(datum_t *e) {
  return e->type == DATUM_NIL || e->type == DATUM_LIST;
}

bool datum_is_symbol(datum_t *e) { return e->type == DATUM_SYMBOL; }

bool datum_is_integer(datum_t *e) { return e->type == DATUM_INTEGER; }

bool datum_is_bytestring(datum_t *e) { return e->type == DATUM_BYTESTRING; }

bool datum_is_routine(datum_t *e) { return e->type == DATUM_ROUTINE; }

bool datum_is_pointer(datum_t *e) { return e->type == DATUM_POINTER; }

bool datum_is_void(datum_t *e) { return e->type == DATUM_VOID; }

datum_t *datum_make_nil() {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_NIL;
  return e;
}

datum_t *datum_make_list(datum_t *head, datum_t *tail) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_LIST;
  e->list_head = head;
  e->list_tail = tail;
  return e;
}

datum_t *datum_make_list_1(datum_t *head) {
  return datum_make_list(head, datum_make_nil());
}

datum_t *datum_make_list_2(datum_t *head, datum_t *second) {
  return datum_make_list(head, datum_make_list_1(second));
}

datum_t *datum_make_list_3(datum_t *head, datum_t *second, datum_t *third) {
  return datum_make_list(head, datum_make_list_2(second, third));
}

datum_t *datum_make_symbol(char *name) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_SYMBOL;
  size_t length = strlen(name);
  e->symbol_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->symbol_value[i] = name[i];
  }
  return e;
}

datum_t *datum_make_bytestring(char *text) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_BYTESTRING;
  size_t length = strlen(text);
  e->bytestring_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->bytestring_value[i] = text[i];
  }
  return e;
}

datum_t *datum_make_int(int64_t value) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_INTEGER;
  e->integer_value = value;
  return e;
}

datum_t *datum_make_routine(prog_t *s, state_t *lexical_bindings) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_ROUTINE;
  e->routine_value.prog = s;
  e->routine_value.state = lexical_bindings;
  return e;
}

datum_t *datum_make_pointer(void *data, datum_t *signature) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_POINTER;
  e->pointer_descriptor = signature;
  e->pointer_value = data;
  return e;
}

datum_t *datum_make_pointer_to_pointer(void **ptr) {
  return datum_make_pointer(ptr, datum_make_symbol("pointer"));
}

datum_t *datum_make_void() {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_VOID;
  return e;
}

bool read_result_is_ok(read_result_t x) { return x.type == READ_RESULT_OK; }

bool read_result_is_panic(read_result_t x) {
  return x.type == READ_RESULT_PANIC;
}

bool read_result_is_eof(read_result_t x) { return x.type == READ_RESULT_EOF; }

bool read_result_is_right_paren(read_result_t x) {
  return x.type == READ_RESULT_RIGHT_PAREN;
}

read_result_t read_result_make_ok(datum_t *e) {
  read_result_t result = {.type = READ_RESULT_OK, .ok_value = e};
  return result;
}

read_result_t read_result_make_panic(char *message) {
  read_result_t result = {.type = READ_RESULT_PANIC, .panic_message = message};
  return result;
}

read_result_t read_result_make_eof(void) {
  read_result_t result = {.type = READ_RESULT_EOF};
  return result;
}

read_result_t read_result_make_right_paren(void) {
  read_result_t result = {.type = READ_RESULT_RIGHT_PAREN};
  return result;
}

bool is_whitespace(char c) { return isspace(c) || c == ','; }

bool is_allowed_inside_symbol(char c) {
  return isalnum(c) || c == '.' || c == '-' || c == '_' || c == ':' || c == '+';
}

bool consume_control_sequence(char c, datum_t **form) {
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

read_result_t datum_read(FILE *strm) {
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
    read_result_t elem;
    datum_t *list = datum_make_nil();
    datum_t **end_marker = &list;
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
    datum_t *sym = datum_make_symbol(nm);
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
  datum_t *form;
  if (consume_control_sequence(c, &form)) {
    read_result_t v = datum_read(strm);
    if (read_result_is_panic(v)) {
      return v;
    }
    if (!read_result_is_ok(v)) {
      return read_result_make_panic(
          "expected an expression after a control character");
    }
    datum_t *res = datum_make_list_1(form);
    res->list_tail = datum_make_list_1(v.ok_value);
    return read_result_make_ok(res);
  }
  char *err = malloc(1024);
  sprintf(err, "unexpected symbol: 0x%x", c);
  return read_result_make_panic(err);
}

char *datum_repr(datum_t *e) {
  char *buf = malloc(1024 * sizeof(char));
  char *end = buf;
  if (datum_is_integer(e)) {
    sprintf(buf, "%" PRId64, e->integer_value);
  } else if (datum_is_list(e)) {
    end += sprintf(end, "(");
    for (datum_t *item = e; !datum_is_nil(item); item = item->list_tail) {
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

bool fdatum_is_ok(fdatum_t result) { return result.type == FDATUM_OK; }

bool fdatum_is_panic(fdatum_t result) { return result.type == FDATUM_PANIC; }

fdatum_t fdatum_make_ok(datum_t *v) {
  fdatum_t result = {.type = FDATUM_OK, .ok_value = v};
  return result;
}

fdatum_t fdatum_make_panic(char *message) {
  fdatum_t result = {.type = FDATUM_PANIC, .panic_message = message};
  return result;
}

bool fstate_is_ok(fstate_t result) { return result.type == FSTATE_OK; }

bool fstate_is_panic(fstate_t result) { return result.type == FSTATE_PANIC; }

fstate_t fstate_make_ok(state_t *v) {
  fstate_t result = {.type = FSTATE_OK, .ok_value = v};
  return result;
}

fstate_t fstate_make_panic(char *message) {
  fstate_t result = {.type = FSTATE_PANIC, .panic_message = message};
  return result;
}

state_t *state_make(datum_t *vars, datum_t *stack, routine_t parent,
                    routine_t hat_parent) {
  state_t *res = malloc(sizeof(state_t));
  res->vars = vars;
  res->stack = stack;
  res->parent = parent;
  res->hat_parent = hat_parent;
  return res;
}

state_t *state_make_fresh() {
  routine_t zero = routine_make_null();
  return state_make(datum_make_nil(), datum_make_nil(), zero, zero);
}

state_t *state_set_var(state_t *ns, datum_t *symbol, datum_t *value) {
  datum_t *kv = datum_make_list_3(symbol, datum_make_symbol(":value"), value);
  return state_make(datum_make_list(kv, ns->vars), ns->stack, ns->parent,
                    ns->hat_parent);
}

state_t *state_set_fn(state_t *ns, datum_t *symbol, datum_t *value) {
  prog_t *s = prog_make();
  char *err = prog_init_routine(s, value);
  if (err != NULL) {
    fprintf(stderr, "bad function def %s %s\n", err, datum_repr(value));
    exit(EXIT_FAILURE);
  }
  datum_t *fn = datum_make_routine(s, NULL);
  datum_t *kv = datum_make_list_3(symbol, datum_make_symbol(":fn"), fn);
  return state_make(datum_make_list(kv, ns->vars), ns->stack, ns->parent,
                    ns->hat_parent);
}

datum_t *namespace_cell_get_value(datum_t *cell, state_t *ns) {
  datum_t *raw_value = cell->list_tail->list_head;
  if (!strcmp(cell->list_head->symbol_value, ":value")) {
    return raw_value;
  } else if (!strcmp(cell->list_head->symbol_value, ":fn")) {
    if (!datum_is_routine(raw_value)) {
      fprintf(stderr, "namespace implementation error");
      exit(EXIT_FAILURE);
    }
    state_t *routine_ns = state_make(ns->vars, datum_make_nil(),
                                     routine_make_null(), routine_make_null());
    return datum_make_routine(raw_value->routine_value.prog, routine_ns);
  } else {
    fprintf(stderr, "namespace implementation error");
    exit(EXIT_FAILURE);
  }
}

fdatum_t state_get_var(state_t *ns, datum_t *symbol) {
  for (datum_t *cur = ns->vars; !datum_is_nil(cur); cur = cur->list_tail) {
    datum_t *entry = cur->list_head;
    if (!strcmp(entry->list_head->symbol_value, symbol->symbol_value)) {
      datum_t *cell = entry->list_tail;
      return fdatum_make_ok(namespace_cell_get_value(cell, ns));
    }
  }
  char *msg = malloc(1024);
  sprintf(msg, "unbound symbol: %s", symbol->symbol_value);
  return fdatum_make_panic(msg);
}

state_t *state_stack_put(state_t *ns, datum_t *value) {
  return state_make(ns->vars, datum_make_list(value, ns->stack), ns->parent,
                    ns->hat_parent);
}

fdatum_t state_stack_peek(state_t *ns) {
  if (datum_is_nil(ns->stack)) {
    return fdatum_make_panic("peek failed");
  }
  return fdatum_make_ok(ns->stack->list_head);
}

state_t *state_stack_pop(state_t *ns) {
  if (datum_is_nil(ns->stack)) {
    fprintf(stderr, "cannot pop from an empty stack\n");
    exit(EXIT_FAILURE);
  }
  return state_make(ns->vars, ns->stack->list_tail, ns->parent, ns->hat_parent);
}

fdatum_t list_map(fdatum_t (*fn)(datum_t *, state_t *), datum_t *items,
                  state_t *ctxt) {
  if (!datum_is_list(items)) {
    return fdatum_make_panic("expected a list");
  }
  datum_t *evaled_items = datum_make_nil();
  datum_t **tail = &evaled_items;
  for (datum_t *arg = items; !datum_is_nil(arg); arg = arg->list_tail) {
    fdatum_t evaled_arg = fn(arg->list_head, ctxt);
    if (fdatum_is_panic(evaled_arg)) {
      return evaled_arg;
    }
    *tail = datum_make_list_1(evaled_arg.ok_value);
    tail = &((*tail)->list_tail);
  }
  return fdatum_make_ok(evaled_items);
}

bool ffi_type_init(ffi_type **type, datum_t *definition) {
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

char *pointer_ffi_init_cif(datum_t *f, ffi_cif *cif) {
  datum_t *sig = f->pointer_descriptor;
  if (!datum_is_list(sig) || datum_is_nil(sig) ||
      datum_is_nil(sig->list_tail) ||
      !datum_is_nil(sig->list_tail->list_tail)) {
    return "the signature should be a two-item list";
  }
  ffi_type **arg_types = malloc(sizeof(ffi_type *) * 32);
  int arg_count = 0;
  datum_t *arg_def;
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

char *pointer_ffi_serialize_args(datum_t *f, datum_t *args, void **cargs) {
  int arg_cnt = 0;
  datum_t *arg = args;
  for (datum_t *argt = f->pointer_descriptor->list_head; !datum_is_nil(argt);
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
      datum_t *sig;
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

fdatum_t pointer_ffi_call(datum_t *f, ffi_cif *cif, void **cargs) {
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
    fdatum_t res;
    ffi_call(cif, fn_ptr, &res, cargs);
    if (fdatum_is_panic(res)) {
      return fdatum_make_panic(res.panic_message);
    }
    return fdatum_make_ok(res.ok_value);
  }
  return fdatum_make_panic("unknown return type for extern func");
}

fdatum_t pointer_call(datum_t *f, datum_t *args) {
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

fstate_t datum_eval(datum_t *e, state_t *ctxt,
                    fdatum_t (*module_source)(char *module)) {
  prog_t *s = prog_make();
  prog_t *pe = s;
  char *err = prog_append_statement(&pe, e, module_source);
  if (err != NULL) {
    return fstate_make_panic(err);
  }
  routine_t c = routine_make(s, ctxt);
  return routine_run(c);
}

fdatum_t builtin_concat_bytestrings(datum_t *x, datum_t *y) {
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

fdatum_t builtin_add(datum_t *x, datum_t *y) {
  if (!datum_is_integer(x) || !datum_is_integer(y)) {
    return fdatum_make_panic("expected integers");
  }
  return fdatum_make_ok(datum_make_int(x->integer_value + y->integer_value));
}

fdatum_t builtin_cons(datum_t *head, datum_t *tail) {
  if (!datum_is_list(tail)) {
    return fdatum_make_panic("cons requires a list as a second argument");
  }
  return fdatum_make_ok(datum_make_list(head, tail));
}

fdatum_t builtin_head(datum_t *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("car expects a nonempty list");
  }
  return fdatum_make_ok(list->list_head);
}

fdatum_t builtin_tail(datum_t *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("cdr expects a nonempty list");
  }
  return fdatum_make_ok(list->list_tail);
}

datum_t *state_list_vars(state_t *ns) {
  datum_t *result = datum_make_nil();
  datum_t **nil = &result;
  for (datum_t *cur = ns->vars; !datum_is_nil(cur); cur = cur->list_tail) {
    datum_t *entry = cur->list_head;
    datum_t *key = entry->list_head;
    datum_t *cell = entry->list_tail;
    datum_t *val = namespace_cell_get_value(cell, ns);
    datum_t *keyval = datum_make_list_2(key, val);
    *nil = datum_make_list_1(keyval);
    nil = &((*nil)->list_tail);
  }
  return result;
}

fdatum_t builtin_shared_library(datum_t *library_name) {
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

fdatum_t builtin_extern_pointer(datum_t *shared_library, datum_t *name,
                                datum_t *descriptor) {
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

fdatum_t builtin_repr(datum_t *v) {
  return fdatum_make_ok(datum_make_bytestring(datum_repr(v)));
}

bool datum_eq(datum_t *x, datum_t *y) {
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

fdatum_t builtin_eq(datum_t *x, datum_t *y) {
  datum_t *t = datum_make_list_1(datum_make_nil());
  datum_t *f = datum_make_nil();
  if (datum_eq(x, y)) {
    return fdatum_make_ok(t);
  }
  return fdatum_make_ok(f);
}

fdatum_t builtin_annotate(datum_t *arg_value) {
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

fdatum_t builtin_is_constant(datum_t *arg_value) {
  if (datum_is_integer(arg_value) || datum_is_bytestring(arg_value) ||
      (datum_is_symbol(arg_value) && arg_value->symbol_value[0] == ':')) {
    return fdatum_make_ok(datum_make_list_1(datum_make_nil()));
  }
  return fdatum_make_ok(datum_make_nil());
}

fdatum_t builtin_panic(datum_t *arg_value) {
  if (!datum_is_bytestring(arg_value)) {
    return fdatum_make_panic("panic expects a bytestring");
  }
  return fdatum_make_panic(arg_value->bytestring_value);
}

void namespace_def_extern_fn(state_t **ctxt, char *name, fdatum_t (*fn)(),
                             int cnt) {
  datum_t *sig = datum_make_nil();
  for (int i = 0; i < cnt; ++i) {
    sig = datum_make_list(datum_make_symbol("datum"), sig);
  }
  datum_t *wrapped_fn =
      datum_make_pointer(__extension__(void *) fn,
                         datum_make_list_2(sig, datum_make_symbol("val")));
  *ctxt = state_set_var(*ctxt, datum_make_symbol(name), wrapped_fn);
}

state_t *state_make_builtins() {
  state_t *ns = state_make_fresh();

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
