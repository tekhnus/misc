#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <extern.h>

EXPORT fdatum prog_compile(datum *source, datum **compdata, datum *info) {
  prog_slice sl = prog_slice_make(16 * 1024);
  size_t p = prog_slice_append_new(&sl);
  char *err = prog_append_statements(&sl, &p, source, compdata, info);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  return fdatum_make_ok(prog_slice_to_datum(sl));
}

EXPORT void prog_append_call(prog_slice *sl, size_t *begin, int fn_index, bool pop_one, datum *type, int arg_count, int return_count, datum **compdata) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_7(datum_make_symbol(":call"), datum_make_int(fn_index), datum_make_int(pop_one), type, datum_make_int(arg_count), datum_make_int(return_count), datum_make_int(next)));
  for (int i = 0; i < arg_count; ++i) {
    *compdata = compdata_del(*compdata, 0);
  }
  if (pop_one) {
    *compdata = compdata_del(*compdata, 0);
  }
  for (int i = 0; i < return_count; ++i) {
    *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
  }  
  *begin = next;
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
    exit(1);
  }
  index = list_length(*compdata) - 1 - index;
  prog_append_nop(sl, begin, datum_make_list_2(datum_make_symbol("putting-var"), val));
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_3(datum_make_symbol(":put-var"), datum_make_int(index), datum_make_int(next)));
  *begin = next;
  compdata = compdata;
  *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
}

EXPORT void compdata_give_names(datum *var, datum **compdata) {
  if (!datum_is_list(var)) {
    fprintf(stderr, "error: compdata_give_names\n");
    exit(EXIT_FAILURE);
  }
  for (datum *rest = var; !datum_is_nil(rest); rest = rest->list_tail) {
    *compdata = compdata_del(*compdata, 0);
  }
  for (datum *rest = var; !datum_is_nil(rest); rest = rest->list_tail) {
    *compdata = compdata_put(*compdata, rest->list_head);
  }
}

EXPORT void prog_append_put_prog(prog_slice *sl, size_t *begin, size_t val, int capture, datum **compdata) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_4(datum_make_symbol(":put-prog"), datum_make_int(val), datum_make_int(capture), datum_make_int(next)));
  *begin = next;
  compdata = compdata;
  *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
}

EXPORT void prog_append_yield(prog_slice *sl, size_t *begin, datum *type, size_t count, size_t recieve_count, datum *meta, datum **compdata) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_6(datum_make_symbol(":yield"), type, datum_make_int(count), datum_make_int(recieve_count), meta, datum_make_int(next)));
  *begin = next;
  for (size_t i = 0; i < count; ++i) {
    *compdata = compdata_del(*compdata, 0);
  }
  for (size_t i = 0; i < recieve_count; ++i) {
    *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
  }
}

EXPORT datum *compdata_make() {
  return datum_make_nil();
}

EXPORT bool compdata_has_value(datum *compdata) {
  compdata_validate(compdata);
  return !datum_is_nil(compdata) && datum_is_the_symbol(compdata->list_head, ":anon");
}

LOCAL char *prog_append_statements(prog_slice *sl, size_t *off, datum *source, datum **compdata, datum *info) {
  for (datum *rest = source; !datum_is_nil(rest); rest = rest->list_tail) {
    datum *stmt = rest->list_head;
    if (rest != source) {
      prog_append_nop(sl, off, datum_make_list_2(datum_make_symbol("info"), datum_make_list(stmt, info)));
    }
    char *err = prog_append_statement(sl, off, stmt, compdata, info);
    if (err != NULL) {
      return err;
    }
  }
  return NULL;
}

LOCAL char *prog_append_statement(prog_slice *sl, size_t *begin, datum *stmt, datum **compdata, datum *info) {
  if (datum_is_constant(stmt)) {
    prog_append_put_const(sl, begin, stmt, compdata);
    return NULL;
  }
  if (datum_is_symbol(stmt)) {
    prog_append_nop(sl, begin, datum_make_list_2(datum_make_symbol("compdata"), *compdata));
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

  if (datum_is_the_symbol(op, "req")) {
    return prog_append_usages(sl, begin, stmt, compdata);
  }
  if (datum_is_the_symbol(op, "export")) {
    return prog_append_exports(sl, begin, stmt, compdata);
  }
  if (datum_is_the_symbol(op, "if")) {
    if (list_length(stmt->list_tail) != 3) {
      return "if should have three args";
    }
    char *err;
    err = prog_append_statement(sl, begin, stmt->list_tail->list_head, compdata, info);
    if (err != NULL) {
      return err;
    }

    size_t true_end = prog_slice_append_new(sl),
      false_end = prog_slice_append_new(sl);

    *prog_slice_datum_at(*sl, *begin) = *datum_make_list_3(datum_make_symbol(":if"), datum_make_int(true_end), datum_make_int(false_end));
    *begin = prog_slice_append_new(sl); // ???

    *compdata = compdata_del(*compdata, 0);
    datum *false_compdata = *compdata;
    err = prog_append_statement(
                                sl, &true_end, stmt->list_tail->list_tail->list_head, compdata, info);
    if (err != NULL) {
      return err;
    }
    err = prog_append_statement(
                                sl, &false_end, stmt->list_tail->list_tail->list_tail->list_head, &false_compdata, info);
    if (err != NULL) {
      return err;
    }
    if (!datum_eq(*compdata, false_compdata)) {
      *compdata = compdata_put(*compdata, datum_make_symbol("__different_if_branches"));
    }

    prog_join(sl, true_end, false_end, *begin);
    return NULL;
  }
  if (datum_is_the_symbol(op, "progn")) {
    for (datum *rest = stmt->list_tail; !datum_is_nil(rest);
         rest = rest->list_tail) {
      datum *step = rest->list_head;
      prog_append_nop(sl, begin, datum_make_list_2(datum_make_symbol("info"), datum_make_list(step, info)));
      char *err = prog_append_statement(sl, begin, step, compdata, info);
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
                                      sl, begin, stmt->list_tail->list_tail->list_head, compdata, datum_make_nil());
    if (err != NULL) {
      return err;
    }
    datum *names;
    if (datum_is_list(stmt->list_tail->list_head)) {
      names = stmt->list_tail->list_head;
    } else {
      names = datum_make_list_1(stmt->list_tail->list_head);
    }
    compdata_give_names(names, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "builtin.defn") || datum_is_the_symbol(op, "builtin.defun")) {
    datum *name = list_at(stmt, 1);
    datum *args;
    datum *body;
    if (list_length(stmt->list_tail) != 3) {
      return "wrong defn";
    }
    args = list_at(stmt, 2);
    body = list_at(stmt, 3);
    size_t s_off = prog_slice_append_new(sl);
    datum *routine_compdata = compdata_put(*compdata, name);
    char *err = prog_init_routine(sl, s_off, args, body, &routine_compdata, datum_make_list(name, info));
    if (err != NULL) {
      return err;
    }
    prog_append_put_prog(sl, begin, s_off, 2, compdata);
    if (datum_is_the_symbol(op, "builtin.defn")) {
      prog_append_resolve(sl, begin);
    }
    compdata_give_names(datum_make_list_1(name), compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "builtin.fn")) {
    datum *args;
    datum *body;
    if (list_length(stmt->list_tail) != 2) {
      return "wrong fn";
    }
    args = list_at(stmt, 1);
    body = list_at(stmt, 2);
    size_t s_off = prog_slice_append_new(sl);
    datum *routine_compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
    char *err =
      prog_init_routine(sl, s_off, args, body, &routine_compdata, datum_make_list(datum_make_symbol("lambda"), info));
    if (err != NULL) {
      return err;
    }
    prog_append_put_prog(sl, begin, s_off, 2, compdata);
    prog_append_resolve(sl, begin);
    return NULL;
  }
  if (datum_is_the_symbol(op, "resolve")) {
    if (list_length(stmt->list_tail) != 1) {
      return "resolve takes a single argument";
    }
    datum *arg = list_at(stmt, 1);
    prog_append_statement(sl, begin, arg, compdata, datum_make_nil());
    prog_append_resolve(sl, begin);
    return NULL;
  }
  if (datum_is_the_symbol(op, "return") ||
      datum_is_the_symbol_pair(op, "hat", "return")) {
    bool hat = datum_is_the_symbol_pair(op, "hat", "return");
    for (datum *rest = stmt->list_tail; !datum_is_nil(rest); rest=rest->list_tail) {
      datum *component = rest->list_head;
      char *err = prog_append_statement(sl, begin, component, compdata, datum_make_nil());
      if (err != NULL) {
        return err;
      }
    }
    prog_append_yield(sl, begin, hat ? datum_make_symbol("hat") : datum_make_symbol("plain"), list_length(stmt->list_tail), 1, datum_make_nil(), compdata);
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
    if (list_length(stmt->list_tail) < 1) {
      return "host should have at least one arg";
    }
    datum *name = stmt->list_tail->list_head;
    datum *args = stmt->list_tail->list_tail;
    size_t nargs = list_length(args);
    for (datum *rest = args; !datum_is_nil(rest); rest=rest->list_tail) {
      datum *arg = rest->list_head;
      prog_append_statement(sl, begin, arg, compdata, datum_make_nil());
    }
    prog_append_yield(sl, begin, datum_make_list_2(datum_make_symbol("host"), name), nargs, 1, datum_make_nil(), compdata);
    return NULL;
  }

  datum *fn = stmt->list_head;
  bool hash = false;
  bool hat = false;
  bool at = false;
  size_t ret_count = 1;
  for (; datum_is_list(fn) && list_length(fn) == 2 &&
         datum_is_symbol(fn->list_head);
       fn = fn->list_tail->list_head) {
    char *tag = fn->list_head->symbol_value;
    if (!strcmp(tag, "hash")) {
      hash = true;
    } else if (!strcmp(tag, "hat")) {
      hat = true;
    } else if (!strcmp(tag, "at")) {
      at = true;
    } else {
      break;
    }
  }
  int fn_index;
  if (at) {
    if (!datum_is_symbol(fn)) {
      return "expected an lvalue";
    }
    fn_index = list_length(*compdata) - 1 - compdata_get_index(*compdata, fn);
    if (fn_index == -1) {
      return datum_repr(*compdata);
      return "function not found";
    }
  } else {
    char *err = prog_append_statement(sl, begin, fn, compdata, datum_make_nil());
    if (err != NULL) {
      return err;
    }
    fn_index = list_length(*compdata) - 1;
  }
  datum *rest_args = stmt->list_tail;
  if (!datum_is_nil(rest_args)) {
    datum *tag = rest_args->list_head;
    if (datum_is_list(tag) && list_length(tag) == 2 && datum_is_the_symbol(tag->list_head, "at")) {
      datum *content = list_at(tag, 1);
      if (!datum_is_integer(content)) {
        return "unknown tag";
      }
      ret_count = content->integer_value;
      rest_args = rest_args->list_tail;
    }
  }
  size_t arg_count = list_length(rest_args);
  for (; !datum_is_nil(rest_args);
       rest_args = rest_args->list_tail) {
    datum *arg = rest_args->list_head;
    if (hash) {
      prog_append_put_const(sl, begin, arg, compdata);
    } else {
      char *err = prog_append_statement(sl, begin, arg, compdata, datum_make_nil());
      if (err != NULL) {
        return err;
      }
    }
  }
  prog_append_call(sl, begin, fn_index, !at, hat ? datum_make_symbol("hat") : datum_make_symbol("plain"), arg_count, ret_count, compdata);
  return NULL;
}

LOCAL char *prog_append_usages(prog_slice *sl, size_t *begin, datum *spec, datum **compdata) {
  fdatum res = prog_read_usages(spec);
  if (fdatum_is_panic(res)) {
    return res.panic_message;
  }
  datum *re = res.ok_value;
  if (!datum_is_list(re) || list_length(re) != 2) {
    return "not gonna happen";
  }
  datum *vars = re->list_head;
  prog_append_recieve(sl, begin, vars, re->list_tail->list_head, compdata);
  return NULL;
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

LOCAL char *prog_append_exports(prog_slice *sl, size_t *begin, datum *spec, datum **compdata) {
  fdatum res = prog_read_exports(spec);
  if (fdatum_is_panic(res)) {
    return res.panic_message;
  }
  datum *re = res.ok_value;
  if (!datum_is_list(re) || list_length(re) != 2) {
    return "not gonna happen";
  }
  for (datum *rest_expressions=re->list_tail->list_head; !datum_is_nil(rest_expressions); rest_expressions=rest_expressions->list_tail) {
    datum *expr = rest_expressions->list_head;
    prog_append_statement(sl, begin, expr, compdata, datum_make_nil());
  }
  /* This nop is appended as a hack so that the yield becomes the last statement on the slice. */
  prog_append_nop(sl, begin, datum_make_nil());
  // probably should change hat=false to true.
  prog_append_yield(sl, begin, datum_make_symbol("plain"), list_length(re->list_head), 1, re->list_head, compdata);
  return NULL;
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
      err = prog_append_statement(sl, begin, elem->list_tail->list_head, compdata, datum_make_nil());
    } else {
      err = prog_append_backquoted_statement(sl, begin, elem, compdata);
    }
    if (err != NULL) {
      return err;
    }
  }
  prog_append_collect(sl, list_length(stmt), begin, compdata);
  return NULL;
}

LOCAL void prog_append_recieve(prog_slice *sl, size_t *begin, datum *args, datum *meta, datum **compdata) {
  // fix hat=false; sometimes it should be true.
  prog_append_yield(sl, begin, datum_make_symbol("plain"), 0, list_length(args), meta, compdata);
  compdata_give_names(args, compdata);
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

LOCAL char *prog_init_routine(prog_slice *sl, size_t s, datum *args, datum *stmt, datum **routine_compdata, datum *info) {
  if (args == NULL) {
    return "args can't be null";
  } else {
    prog_append_recieve(sl, &s, args, datum_make_nil(), routine_compdata);
  }
  prog_append_nop(sl, &s, datum_make_list_2(datum_make_symbol("info"), info));
  return prog_append_statement(sl, &s, stmt, routine_compdata, info);
}

LOCAL void prog_append_put_const(prog_slice *sl, size_t *begin, datum *val, datum **compdata) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_3(datum_make_symbol(":put-const"), val, datum_make_int(next)));
  *begin = next;
  compdata = compdata;
  *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
}


EXPORT void prog_append_resolve(prog_slice *sl, size_t *begin) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_2(datum_make_symbol(":resolve"), datum_make_int(next)));
  *begin = next;
}

EXPORT void prog_append_nop(prog_slice *sl, size_t *begin, datum *info) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_3(datum_make_symbol(":nop"), info, datum_make_int(next)));
  *begin = next;
}

LOCAL void prog_append_collect(prog_slice *sl, size_t count, size_t *begin, datum **compdata) {
  size_t next = prog_slice_append_new(sl);
  *prog_slice_datum_at(*sl, *begin) = *(datum_make_list_3(datum_make_symbol(":collect"), datum_make_int(count), datum_make_int(next)));
  *begin = next;
  compdata = compdata;
  for (size_t i = 0; i < count; ++i) {
    *compdata = compdata_del(*compdata, 0);
  }
  *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
}

LOCAL void prog_join(prog_slice *sl, size_t a, size_t b, size_t e) {
  *prog_slice_datum_at(*sl, a) = *(datum_make_list_3(datum_make_symbol(":nop"), datum_make_nil(), datum_make_int(e)));
  *prog_slice_datum_at(*sl, b) = *(datum_make_list_3(datum_make_symbol(":nop"), datum_make_nil(), datum_make_int(e)));
}

LOCAL void compdata_validate(datum *compdata) {
  if (!datum_is_nil(compdata) && datum_is_the_symbol(compdata->list_head, "__different_if_branches")) {
    fprintf(stderr, "compdata_del: if branches had different compdata\n");
    fprintf(stderr, "%s\n", datum_repr(compdata));
    exit(EXIT_FAILURE);
  }
}

LOCAL datum *compdata_put(datum *compdata, datum *var) {
  return datum_make_list(var, compdata);
}

LOCAL datum *compdata_del(datum *compdata, int index) {
  compdata_validate(compdata);
  if (datum_is_nil(compdata)) {
    fprintf(stderr, "compdata_del: empty compdata\n");
    exit(EXIT_FAILURE);
  }
  if (index == 0) {
    return compdata->list_tail;
  }
  return datum_make_list(compdata->list_head, compdata_del(compdata->list_tail, index - 1));
}

LOCAL int compdata_get_index(datum *compdata, datum *var) {
  return list_index_of(compdata, var);
}

LOCAL bool datum_is_the_symbol_pair(datum *d, char *val1, char *val2) {
  return datum_is_list(d) && list_length(d) == 2 &&
         datum_is_the_symbol(d->list_head, val1) &&
         datum_is_the_symbol(d->list_tail->list_head, val2);
}
