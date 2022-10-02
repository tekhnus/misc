#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <extern.h>

EXPORT fdatum prog_init_submodule(prog_slice *sl, size_t *off, datum *source, datum **compdata) {
  fdatum res = prog_append_usages(sl, off, source->list_head, compdata);
  if (fdatum_is_panic(res)) {
    return res;
  }
  prog_append_put_const(sl, off, datum_make_void(), compdata);
  for (datum *rest = source->list_tail; !datum_is_nil(rest); rest = rest->list_tail) {
    datum *stmt = rest->list_head;
    if (datum_is_list(stmt) && !datum_is_nil(stmt) && datum_is_the_symbol(stmt->list_head, "export")) {
      if (!datum_is_nil(rest->list_tail)) {
        return fdatum_make_panic("export should be the last statement in module");
      }
      fdatum exp = prog_append_exports(sl, off, stmt, compdata);
      if (fdatum_is_panic(exp)) {
        return exp;
      }
      return fdatum_make_ok(datum_make_list_2(res.ok_value, exp.ok_value));
    }
    prog_append_pop(sl, off, datum_make_symbol(":void"), compdata);
    char *err = prog_append_statement(sl, off, stmt, compdata);
    if (err != NULL) {
      return fdatum_make_panic(err);
    }
  }
  return fdatum_make_ok(datum_make_list_2(res.ok_value, datum_make_nil()));
  // return fdatum_make_panic("export statement should terminate the module");
}

LOCAL fdatum prog_append_usages(prog_slice *sl, size_t *begin, datum *spec, datum **compdata) {
  fdatum res = prog_read_usages(spec);
  if (fdatum_is_panic(res)) {
    return res;
  }
  datum *re = res.ok_value;
  if (!datum_is_list(re) || list_length(re) != 2) {
    return fdatum_make_panic("not gonna happen");
  }
  datum *vars = re->list_head;
  prog_append_recieve(sl, begin, vars, compdata);
  return fdatum_make_ok(re->list_tail->list_head);
}

LOCAL fdatum prog_read_usages(datum *spec) {
  if (!datum_is_list(spec) || list_length(spec) == 0 || !datum_is_the_symbol(spec->list_head, "req")) {
    return fdatum_make_panic("wrong usage spec");
  }
  datum *items = spec->list_tail;
  datum *vars = datum_make_nil();
  datum **vars_tail = &vars;
  datum *specs = datum_make_nil();
  datum **specs_tail = &specs;
  for (datum *rest = items; !datum_is_nil(rest); rest=rest->list_tail) {
    datum *item = rest->list_head;
    if (!datum_is_list(item) || list_length(item) < 2 || list_length(item) > 3) {
      return fdatum_make_panic("wrong usage spec");
    }
    datum *item_var = item->list_head;
    if (!datum_is_symbol(item_var)) {
      return fdatum_make_panic("wrong usage spec");
    }
    datum *item_spec = item->list_tail;
    *vars_tail = datum_make_list_1(item_var);
    vars_tail = &((*vars_tail)->list_tail);
    *specs_tail = datum_make_list_1(item_spec);
    specs_tail = &((*specs_tail)->list_tail);
  }
  return fdatum_make_ok(datum_make_list_2(vars, specs));
}

LOCAL fdatum prog_append_exports(prog_slice *sl, size_t *begin, datum *spec, datum **compdata) {
  fdatum res = prog_read_exports(spec);
  if (fdatum_is_panic(res)) {
    return res;
  }
  datum *re = res.ok_value;
  if (!datum_is_list(re) || list_length(re) != 2) {
    return fdatum_make_panic("not gonna happen");
  }
  for (datum *rest_expressions=re->list_tail->list_head; !datum_is_nil(rest_expressions); rest_expressions=rest_expressions->list_tail) {
    datum *expr = rest_expressions->list_head;
    prog_append_statement(sl, begin, expr, compdata);
  }
  return fdatum_make_ok(re->list_head);
}

LOCAL fdatum prog_read_exports(datum *spec) {
  if (!datum_is_list(spec) || list_length(spec) == 0 || !datum_is_the_symbol(spec->list_head, "export")) {
    return fdatum_make_panic("wrong export spec");
  }
  datum *items = spec->list_tail;
  datum *names = datum_make_nil();
  datum **names_tail = &names;
  datum *expressions = datum_make_nil();
  datum **expressions_tail = &expressions;
  for (datum *rest = items; !datum_is_nil(rest); rest=rest->list_tail) {
    datum *item = rest->list_head;
    if (!datum_is_list(item) || list_length(item) != 2) {
      return fdatum_make_panic("wrong export spec");
    }
    datum *item_name = item->list_head;
    if (!datum_is_symbol(item_name)) {
      return fdatum_make_panic("wrong export spec");
    }
    datum *item_expression = item->list_tail->list_head;
    *names_tail = datum_make_list_1(item_name);
    names_tail = &((*names_tail)->list_tail);
    *expressions_tail = datum_make_list_1(item_expression);
    expressions_tail = &((*expressions_tail)->list_tail);
  }
  return fdatum_make_ok(datum_make_list_2(names, expressions));
}

LOCAL char *prog_append_statement(prog_slice *sl, size_t *begin, datum *stmt, datum **compdata) {
  if (!datum_is_nil(*compdata) && datum_is_the_symbol((*compdata)->list_head, "__different_if_branches")) {
    fprintf(stderr, "fatal: if branches had different compdata %s\n", datum_repr(stmt));
    exit(EXIT_FAILURE);
  }
  if (datum_is_constant(stmt)) {
    prog_append_put_const(sl, begin, stmt, compdata);
    return NULL;
  }
  if (datum_is_symbol(stmt)) {
    prog_append_put_var(sl, begin, stmt, compdata);
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
    err = prog_append_statement(sl, begin, stmt->list_tail->list_head, compdata);
    if (err != NULL) {
      return err;
    }

    size_t true_end = prog_slice_append_new(sl),
      false_end = prog_slice_append_new(sl);

    *prog_slice_datum_at(*sl, *begin) = *datum_make_list_3(datum_make_symbol(":if"), datum_make_int(true_end), datum_make_int(false_end));
    *begin = prog_slice_append_new(sl); // ???

    datum *false_compdata = *compdata;
    err = prog_append_statement(
                                sl, &true_end, stmt->list_tail->list_tail->list_head, compdata);
    if (err != NULL) {
      return err;
    }
    err = prog_append_statement(
                                sl, &false_end, stmt->list_tail->list_tail->list_tail->list_head, &false_compdata);
    if (err != NULL) {
      return err;
    }
    if (!datum_eq(*compdata, false_compdata)) {
      *compdata = compdata_pop_to_var(*compdata, datum_make_symbol("__different_if_branches"));
      // fprintf(stderr, "warning: if branches have different compdata\n");
      // fprintf(stderr, "%s\n", datum_repr(stmt->list_tail->list_head));
      // fprintf(stderr, "%s\n", datum_repr(*compdata));
      // fprintf(stderr, "%s\n", datum_repr(false_compdata));
    }

    prog_join(sl, true_end, false_end, *begin);
    return NULL;
  }
  if (datum_is_the_symbol(op, "progn")) {
    prog_append_put_const(sl, begin, datum_make_void(), compdata);
    for (datum *rest = stmt->list_tail; !datum_is_nil(rest);
         rest = rest->list_tail) {
      prog_append_pop(sl, begin, datum_make_symbol(":void"), compdata);
      datum *step = rest->list_head;
      char *err = prog_append_statement(sl, begin, step, compdata);
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
    prog_append_put_const(sl, begin, stmt->list_tail->list_head, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "def")) {
    if (list_length(stmt->list_tail) != 2) {
      return "def should have two args";
    }
    char *err = prog_append_statement(
                                      sl, begin, stmt->list_tail->list_tail->list_head, compdata);
    if (err != NULL) {
      return err;
    }
    prog_append_pop(sl, begin, datum_make_list_1(stmt->list_tail->list_head), compdata);
    prog_append_put_const(sl, begin, datum_make_void(), compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "builtin.defn") ||
      datum_is_the_symbol_pair(op, "hat", "builtin.defn")) {
    bool hat = datum_is_the_symbol_pair(op, "hat", "builtin.defn");
    if (list_length(stmt->list_tail) != 2) {
      return "defn should have two args";
    }
    size_t s_off = prog_slice_append_new(sl);
    *compdata = compdata_pop_to_var(*compdata, stmt->list_tail->list_head);
    char *err = prog_init_routine(sl, s_off, stmt->list_tail->list_tail->list_head, compdata);
    if (err != NULL) {
      return err;
    }
    prog_append_set_closures(sl, begin, s_off, stmt->list_tail->list_head, hat);
    prog_append_put_const(sl, begin, datum_make_void(), compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "builtin.fn") ||
      datum_is_the_symbol_pair(op, "hat", "builtin.fn")) {
    bool hat = datum_is_the_symbol_pair(op, "hat", "builtin.fn");
    if (list_length(stmt->list_tail) != 1) {
      return "fn should have one arg";
    }
    size_t s_off = prog_slice_append_new(sl);
    char *err =
      prog_init_routine(sl, s_off, stmt->list_tail->list_head, compdata);
    if (err != NULL) {
      return err;
    }
    prog_append_put_prog(sl, begin, s_off, hat ? 2 : 1);
    return NULL;
  }
  if (datum_is_the_symbol(op, "return") ||
      datum_is_the_symbol_pair(op, "hat", "return")) {
    bool hat = datum_is_the_symbol_pair(op, "hat", "return");
    if (list_length(stmt->list_tail) != 1) {
      return "return should have a single arg";
    }
    char *err = prog_append_statement(sl, begin, stmt->list_tail->list_head, compdata);
    if (err != NULL) {
      return err;
    }
    prog_append_return(sl, begin, hat, 1);
    return NULL;
  }
  if (datum_is_the_symbol(op, "yield") ||
      datum_is_the_symbol_pair(op, "hat", "yield")) {
    bool hat = datum_is_the_symbol_pair(op, "hat", "yield");
    if (list_length(stmt->list_tail) != 1) {
      return "yield should have a single arg";
    }
    char *err = prog_append_statement(sl, begin, stmt->list_tail->list_head, compdata);
    if (err != NULL) {
      return err;
    }
    prog_append_yield(sl, begin, hat);
    prog_append_recieve(sl, begin, datum_make_list_1(datum_make_symbol("__yield_result")), compdata);
    prog_append_put_const(sl, begin, datum_make_void(), compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "backquote")) {
    if (list_length(stmt->list_tail) != 1) {
      return "backquote should have a single arg";
    }
    return prog_append_backquoted_statement(
                                            sl, begin, stmt->list_tail->list_head, compdata);
  }
  if (datum_is_the_symbol(op, "host")) {
    if (list_length(stmt->list_tail) != 2) {
      return "host should have exactly two args";
    }
    datum *operation = stmt->list_tail->list_head;
    datum *arg = stmt->list_tail->list_tail->list_head;
    prog_append_statement(sl, begin, arg, compdata);
    prog_append_host(sl, begin, operation);
    return NULL;
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
  char *err = prog_append_statement(sl, begin, fn, compdata);
  if (err != NULL) {
    return err;
  }
  for (datum *rest_args = stmt->list_tail; !datum_is_nil(rest_args);
       rest_args = rest_args->list_tail) {
    datum *arg = rest_args->list_head;
    if (hash) {
      prog_append_put_const(sl, begin, arg, compdata);
    } else {
      char *err = prog_append_statement(sl, begin, arg, compdata);
      if (err != NULL) {
        return err;
      }
    }
  }
  prog_append_collect(sl, list_length(stmt) - 1, begin);
  prog_append_collect(sl, 2, begin);
  prog_append_call(sl, begin, hat);
  return NULL;
}

LOCAL void prog_join(prog_slice *sl, size_t a, size_t b, size_t e) {
  *prog_slice_datum_at(*sl, a) = *(datum_make_list_2(datum_make_symbol(":nop"),  datum_make_int(e)));
  *prog_slice_datum_at(*sl, b) = *(datum_make_list_2(datum_make_symbol(":nop"),  datum_make_int(e)));
}

EXPORT void prog_append_call(prog_slice *sl, size_t *begin, bool hat) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_3(datum_make_symbol(":call"), datum_make_int(hat), datum_make_int(next)));
  *begin = next;
}

LOCAL void prog_append_host(prog_slice *sl, size_t *begin, datum *name) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_3(datum_make_symbol(":host"), name, datum_make_int(next)));
  *begin = next;
}

EXPORT void prog_append_put_const(prog_slice *sl, size_t *begin, datum *val, datum **compdata) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_3(datum_make_symbol(":put-const"), val, datum_make_int(next)));
  *begin = next;
  compdata = compdata;
  // *compdata = compdata_pop_to_var(*compdata, datum_make_symbol(":anon"));
}

EXPORT void prog_append_put_var(prog_slice *sl, size_t *begin, datum *val, datum **compdata) {
  size_t next = prog_slice_append_new(sl);
  if (!datum_is_symbol(val)) {
    fprintf(stderr, "expected a symbol in put-var\n");
    exit(1);
  }
  int index = compdata_get_index(*compdata, val);
  if (index == -1) {
    fprintf(stderr, "undefined variable: %s\n", val->symbol_value);
    // fprintf(stderr, "%s\n", datum_repr(*compdata));
    exit(1);
  }
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_4(datum_make_symbol(":put-var"), val, datum_make_int(index), datum_make_int(next)));
  *begin = next;
}

EXPORT void prog_append_collect(prog_slice *sl, size_t count, size_t *begin) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_3(datum_make_symbol(":collect"), datum_make_int(count), datum_make_int(next)));
  *begin = next;
}

EXPORT void prog_append_pop(prog_slice *sl, size_t *begin, datum *var, datum **compdata) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_3(datum_make_symbol(":pop"), var, datum_make_int(next)));
  *begin = next;
  if (datum_is_list(var)) {
    for (datum *rest = var; !datum_is_nil(rest); rest = rest->list_tail) {
      *compdata = compdata_pop_to_var(*compdata, rest->list_head);
    }
  } else if (!datum_is_the_symbol(var, ":void")) {
    *compdata = compdata_del(*compdata);
  }
}

LOCAL void prog_append_set_closures(prog_slice *sl, size_t *begin, size_t p,
                                    datum *var, bool hat) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_5(datum_make_symbol(":set-closures"), datum_make_int(p), var, datum_make_int(hat), datum_make_int(next)));
  *begin = next;
}

EXPORT void prog_append_put_prog(prog_slice *sl, size_t *begin, size_t val, int capture) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_4(datum_make_symbol(":put-prog"), datum_make_int(val), datum_make_int(capture), datum_make_int(next)));
  *begin = next;
}

EXPORT void prog_append_return(prog_slice *sl, size_t *begin, bool hat, size_t count) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_3(datum_make_symbol(":return"), datum_make_int(hat), datum_make_int(count)));
  *begin = next;
}

EXPORT void prog_append_yield(prog_slice *sl, size_t *begin, bool hat) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_3(datum_make_symbol(":yield"), datum_make_int(hat), datum_make_int(next)));
  *begin = next;
}

LOCAL char *prog_append_backquoted_statement(
                                             prog_slice *sl, size_t *begin, datum *stmt, datum **compdata) {
  if (!datum_is_list(stmt)) {
    prog_append_put_const(sl, begin, stmt, compdata);
    return NULL;
  }
  for (datum *rest_elems = stmt; !datum_is_nil(rest_elems);
       rest_elems = rest_elems->list_tail) {
    datum *elem = rest_elems->list_head;
    char *err;
    if (datum_is_list(elem) && list_length(elem) == 2 &&
        datum_is_the_symbol(elem->list_head, "tilde")) {
      err = prog_append_statement(sl, begin, elem->list_tail->list_head, compdata);
    } else {
      err = prog_append_backquoted_statement(sl, begin, elem, compdata);
    }
    if (err != NULL) {
      return err;
    }
  }
  prog_append_collect(sl, list_length(stmt), begin);
  return NULL;
}

EXPORT void prog_append_nop(prog_slice *sl, size_t *begin) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_2(datum_make_symbol(":nop"), datum_make_int(next)));
  *begin = next;
}

EXPORT void prog_append_recieve(prog_slice *sl, size_t *begin, datum *args, datum **compdata) {
  prog_append_nop(sl, begin);
  prog_append_pop(sl, begin, args, compdata);
}

LOCAL char *prog_init_routine(prog_slice *sl, size_t s, datum *stmt, datum **compdata) {
  datum *routine_compdata = *compdata;
  prog_append_recieve(sl, &s, datum_make_list_1(datum_make_symbol("args")), &routine_compdata);
  return prog_append_statement(sl, &s, stmt, &routine_compdata);
}


EXPORT datum *compdata_make() {
  return datum_make_nil();
}

LOCAL datum *compdata_pop_to_var(datum *compdata, datum *var) {
  return datum_make_list(var, compdata);
}

LOCAL datum *compdata_del(datum *compdata) {
  if (datum_is_nil(compdata)) {
    fprintf(stderr, "compdata_del: empty compdata\n");
    exit(EXIT_FAILURE);
  }
  if (datum_is_the_symbol(compdata->list_head, "__different_if_branches")) {
    fprintf(stderr, "compdata_get_index: if branches had different compdata\n");
    exit(EXIT_FAILURE);
  }
  return compdata->list_tail;
}

LOCAL int compdata_get_index(datum *compdata, datum *var) {
  if (datum_is_nil(compdata)) {
    return -1;
  }
  if (datum_is_the_symbol(compdata->list_head, "__different_if_branches")) {
    fprintf(stderr, "compdata_get_index: if branches had different compdata\n");
    exit(EXIT_FAILURE);
  }
  if (datum_eq(compdata->list_head, var)) {
    return 0;
  }
  /* if (datum_is_the_symbol(compdata->list_head, ":anon")) { */
  /*   return compdata_get_index(compdata->list_tail, var); */
  /* } */
  int res = compdata_get_index(compdata->list_tail, var);
  if (res == -1) {
    return -1;
  }
  return res + 1;
}

LOCAL bool datum_is_the_symbol_pair(datum *d, char *val1, char *val2) {
  return datum_is_list(d) && list_length(d) == 2 &&
         datum_is_the_symbol(d->list_head, val1) &&
         datum_is_the_symbol(d->list_tail->list_head, val2);
}

EXPORT datum *datum_make_void() { return datum_make_symbol(":void-value"); }
