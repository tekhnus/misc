#include <zlisp-impl/compiling.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

prog *prog_make() {
  prog *res = malloc(sizeof(prog));
  res->type = PROG_END;
  return res;
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

void prog_append_call(prog **begin, int hat) { // TODO(harius): int to bool
  (*begin)->type = PROG_CALL;
  (*begin)->call_hat = hat;
  (*begin)->call_next = prog_make();
  *begin = (*begin)->call_next;
}
