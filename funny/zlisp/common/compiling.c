#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <extern.h>

EXPORT fdatum prog_init_submodule(prog_slice *sl, size_t *off, datum *source) {
  prog *p = prog_slice_at(*sl, *off);
  fdatum res = prog_append_usages(sl, &p, source->list_head);
  if (fdatum_is_panic(res)) {
    return res;
  }
  for (datum *rest = source->list_tail; !datum_is_nil(rest); rest = rest->list_tail) {
    prog_append_pop(sl, &p, datum_make_symbol(":void"));
    datum *stmt = rest->list_head;
    char *err = prog_append_statement(sl, &p, stmt);
    if (err != NULL) {
      return fdatum_make_panic(err);
    }
  }
  *off = prog_to_offset_int(*sl, p);
  return res;
}

LOCAL fdatum prog_append_usages(prog_slice *sl, prog **begin, datum *spec) {
  fdatum res = prog_read_usages(spec);
  if (fdatum_is_panic(res)) {
    return res;
  }
  datum *re = res.ok_value;
  if (!datum_is_list(re) || list_length(re) != 2) {
    return fdatum_make_panic("not gonna happen");
  }
  for (datum *rest_deps=re->list_head; !datum_is_nil(rest_deps); rest_deps=rest_deps->list_tail) {
    datum *dep_var = rest_deps->list_head;
    prog_append_uncollect(sl, begin);
    prog_append_pop(sl, begin, dep_var);
  }
  prog_append_pop(sl, begin, datum_make_symbol(":void"));
  prog_append_put_const(sl, begin, datum_make_void());
  return res;
}

LOCAL fdatum prog_read_usages(datum *spec) {
  if (!datum_is_list(spec) || list_length(spec) == 0 || !datum_is_the_symbol(spec->list_head, "req")) {
    return fdatum_make_panic(datum_repr(spec));
    return fdatum_make_panic("wrong usage spec");
  }
  datum *items = spec->list_tail;
  datum *vars = datum_make_nil();
  datum **vars_tail = &vars;
  datum *specs = datum_make_nil();
  datum **specs_tail = &specs;
  for (datum *rest = items; !datum_is_nil(rest); rest=rest->list_tail) {
    datum *item = rest->list_head;
    if (!datum_is_list(item) || list_length(item) != 2) {
      return fdatum_make_panic("wrong usage spec");
    }
    datum *item_var = item->list_head;
    if (!datum_is_symbol(item_var)) {
      return fdatum_make_panic("wrong usage spec");
    }
    datum *item_spec = item->list_tail->list_head;
    *vars_tail = datum_make_list_1(item_var);
    vars_tail = &((*vars_tail)->list_tail);
    *specs_tail = datum_make_list_1(item_spec);
    specs_tail = &((*specs_tail)->list_tail);
  }
  return fdatum_make_ok(datum_make_list_2(vars, specs));
}

LOCAL char *prog_append_statement(prog_slice *sl, prog **begin, datum *stmt) {
  if (datum_is_constant(stmt)) {
    prog_append_put_const(sl, begin, stmt);
    return NULL;
  }
  if (datum_is_symbol(stmt)) {
    prog_append_put_var(sl, begin, stmt);
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
    err = prog_append_statement(sl, begin, stmt->list_tail->list_head);
    if (err != NULL) {
      return err;
    }


    size_t true_end = prog_slice_append_new(sl),
      false_end = prog_slice_append_new(sl);

    **begin = datum_to_prog(datum_make_list_3(datum_make_symbol(":if"), datum_make_int(true_end), datum_make_int(false_end)));

    prog *te = prog_slice_at(*sl, true_end);
    prog *fe = prog_slice_at(*sl, false_end);
    err = prog_append_statement(
                                sl, &te, stmt->list_tail->list_tail->list_head);
    if (err != NULL) {
      return err;
    }
    err = prog_append_statement(
        sl, &fe, stmt->list_tail->list_tail->list_tail->list_head);
    if (err != NULL) {
      return err;
    }
    size_t begin_off = prog_slice_append_new(sl);
    *begin = prog_slice_at(*sl, begin_off);
    prog_join(sl, te, fe, *begin);
    return NULL;
  }
  if (datum_is_the_symbol(op, "progn")) {
    prog_append_put_const(sl, begin, datum_make_void());
    for (datum *rest = stmt->list_tail; !datum_is_nil(rest);
         rest = rest->list_tail) {
      prog_append_pop(sl, begin, datum_make_symbol(":void"));
      datum *step = rest->list_head;
      char *err = prog_append_statement(sl, begin, step);
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
    prog_append_put_const(sl, begin, stmt->list_tail->list_head);
    return NULL;
  }
  if (datum_is_the_symbol(op, "def")) {
    if (list_length(stmt->list_tail) != 2) {
      return "def should have two args";
    }
    char *err = prog_append_statement(
        sl, begin, stmt->list_tail->list_tail->list_head);
    if (err != NULL) {
      return err;
    }
    prog_append_pop(sl, begin, stmt->list_tail->list_head);
    prog_append_put_const(sl, begin, datum_make_void());
    return NULL;
  }
  if (datum_is_the_symbol(op, "builtin.defn") ||
      datum_is_the_symbol_pair(op, "hat", "builtin.defn")) {
    bool hat = datum_is_the_symbol_pair(op, "hat", "builtin.defn");
    if (list_length(stmt->list_tail) != 2) {
      return "defn should have two args";
    }
    size_t s_off = prog_slice_append_new(sl);
    prog *s = prog_slice_at(*sl, s_off);
    char *err = prog_init_routine(sl, s, stmt->list_tail->list_tail->list_head);
    if (err != NULL) {
      return err;
    }
    prog_append_set_closures(sl, begin, s, stmt->list_tail->list_head, hat);
    prog_append_put_const(sl, begin, datum_make_void());
    return NULL;
  }
  if (datum_is_the_symbol(op, "builtin.fn") ||
      datum_is_the_symbol_pair(op, "hat", "builtin.fn")) {
    bool hat = datum_is_the_symbol_pair(op, "hat", "builtin.fn");
    if (list_length(stmt->list_tail) != 1) {
      return "fn should have one arg";
    }
    size_t s_off = prog_slice_append_new(sl);
    prog *s = prog_slice_at(*sl, s_off);
    char *err =
        prog_init_routine(sl, s, stmt->list_tail->list_head);
    if (err != NULL) {
      return err;
    }
    prog_append_set_closures(sl, begin, s, datum_make_symbol("__lambda"), hat);
    prog_append_put_var(sl, begin, datum_make_symbol("__lambda"));
    return NULL;
  }
  if (datum_is_the_symbol(op, "importall")) {
    if (list_length(stmt->list_tail) != 1) {
      return "importall should have one arg";
    }
    char *err = prog_append_statement(
        sl, begin, stmt->list_tail->list_head);
    if (err != NULL) {
      return err;
    }
    prog_append_import(sl, begin);
    prog_append_put_const(sl, begin, datum_make_void());
    return NULL;
  }
  if (datum_is_the_symbol(op, "return") ||
      datum_is_the_symbol_pair(op, "hat", "return")) {
    bool hat = datum_is_the_symbol_pair(op, "hat", "return");
    if (list_length(stmt->list_tail) != 1) {
      return "return should have a single arg";
    }
    char *err = prog_append_statement(sl, begin, stmt->list_tail->list_head);
    if (err != NULL) {
      return err;
    }
    prog_append_return(sl, begin, hat);
    return NULL;
  }
  if (datum_is_the_symbol(op, "yield") ||
      datum_is_the_symbol_pair(op, "hat", "yield")) {
    bool hat = datum_is_the_symbol_pair(op, "hat", "yield");
    if (list_length(stmt->list_tail) != 1) {
      return "yield should have a single arg";
    }
    char *err = prog_append_statement(sl, begin, stmt->list_tail->list_head);
    if (err != NULL) {
      return err;
    }
    prog_append_yield(sl, begin, hat);
    return NULL;
  }
  if (datum_is_the_symbol(op, "backquote")) {
    if (list_length(stmt->list_tail) != 1) {
      return "backquote should have a single arg";
    }
    return prog_append_backquoted_statement(
        sl, begin, stmt->list_tail->list_head);
  }
  if (datum_is_the_symbol(op, "host")) {
    if (list_length(stmt->list_tail) != 2) {
      return "host should have exactly two args";
    }
    datum *operation = stmt->list_tail->list_head;
    datum *arg = stmt->list_tail->list_tail->list_head;
    prog_append_statement(sl, begin, arg);
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
  prog_append_args(sl, begin);
  char *err = prog_append_statement(sl, begin, fn);
  if (err != NULL) {
    return err;
  }
  for (datum *rest_args = stmt->list_tail; !datum_is_nil(rest_args);
       rest_args = rest_args->list_tail) {
    datum *arg = rest_args->list_head;
    if (hash) {
      prog_append_put_const(sl, begin, arg);
    } else {
      char *err = prog_append_statement(sl, begin, arg);
      if (err != NULL) {
        return err;
      }
    }
  }
  prog_append_collect(sl, begin);
  prog_append_call(sl, begin, hat);
  return NULL;
}

EXPORT void prog_append_call(prog_slice *sl, prog **begin, bool hat) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_3(datum_make_symbol(":call"), datum_make_int(hat), datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

LOCAL void prog_append_host(prog_slice *sl, prog **begin, datum *name) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_3(datum_make_symbol(":host"), name, datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

LOCAL void prog_join(prog_slice *sl, prog *a, prog *b, prog *e) {
  *a = datum_to_prog(datum_make_list_2(datum_make_symbol(":nop"),  datum_make_int(prog_to_offset_int(*sl, e))));
  *b = datum_to_prog(datum_make_list_2(datum_make_symbol(":nop"),  datum_make_int(prog_to_offset_int(*sl, e))));
}

EXPORT void prog_append_put_const(prog_slice *sl, prog **begin, datum *val) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_3(datum_make_symbol(":put-const"), val, datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

EXPORT void prog_append_put_var(prog_slice *sl, prog **begin, datum *val) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_3(datum_make_symbol(":put-var"), val, datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

EXPORT void prog_append_args(prog_slice *sl, prog **begin) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_2(datum_make_symbol(":args"), datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

EXPORT void prog_append_collect(prog_slice *sl, prog **begin) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_2(datum_make_symbol(":collect"), datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

EXPORT void prog_append_uncollect(prog_slice *sl, prog **begin) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_2(datum_make_symbol(":uncollect"), datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

EXPORT void prog_append_pop(prog_slice *sl, prog **begin, datum *var) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_3(datum_make_symbol(":pop"), var, datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

LOCAL void prog_append_set_closures(prog_slice *sl, prog **begin, prog *p,
                                    datum *var, bool hat) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_5(datum_make_symbol(":set-closures"), prog_to_offset(*sl, p), var, datum_make_int(hat), datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

EXPORT void prog_append_put_prog(prog_slice *sl, prog **begin, prog *val, int capture) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_4(datum_make_symbol(":put-prog"), prog_to_offset(*sl, val), datum_make_int(capture), datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

EXPORT void prog_append_return(prog_slice *sl, prog **begin, bool hat) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_3(datum_make_symbol(":return"), datum_make_int(hat), datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

EXPORT void prog_append_yield(prog_slice *sl, prog **begin, bool hat) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_3(datum_make_symbol(":yield"), datum_make_int(hat), datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

LOCAL char *prog_append_backquoted_statement(
    prog_slice *sl, prog **begin, datum *stmt) {
  if (!datum_is_list(stmt)) {
    prog_append_put_const(sl, begin, stmt);
    return NULL;
  }
  prog_append_args(sl, begin);
  for (datum *rest_elems = stmt; !datum_is_nil(rest_elems);
       rest_elems = rest_elems->list_tail) {
    datum *elem = rest_elems->list_head;
    char *err;
    if (datum_is_list(elem) && list_length(elem) == 2 &&
        datum_is_the_symbol(elem->list_head, "tilde")) {
      err = prog_append_statement(sl, begin, elem->list_tail->list_head);
    } else {
      err = prog_append_backquoted_statement(sl, begin, elem);
    }
    if (err != NULL) {
      return err;
    }
  }
  prog_append_collect(sl, begin);
  return NULL;
}

LOCAL char *prog_init_routine(prog_slice *sl, prog *s, datum *stmt) {
  prog_append_pop(sl, &s, datum_make_symbol("args"));
  return prog_append_statement(sl, &s, stmt);
}

LOCAL void prog_append_import(prog_slice *sl, prog **begin) {
  size_t next = prog_slice_append_new(sl);
  **begin = datum_to_prog(datum_make_list_2(datum_make_symbol(":import"), datum_make_int(next)));
  *begin = prog_slice_at(*sl, next);
}

LOCAL bool datum_is_the_symbol_pair(datum *d, char *val1, char *val2) {
  return datum_is_list(d) && list_length(d) == 2 &&
         datum_is_the_symbol(d->list_head, val1) &&
         datum_is_the_symbol(d->list_tail->list_head, val2);
}

EXPORT datum *datum_make_void() { return datum_make_symbol(":void-value"); }
