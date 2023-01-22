#include <assert.h>
#include <extern.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

EXPORT fdatum prog_compile(datum *source, datum **compdata) {
  vec sl = vec_make(16 * 1024);
  size_t p = vec_append_new(&sl);
  char *err = prog_append_statements(&sl, &p, source, compdata);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  return fdatum_make_ok(*vec_to_datum(&sl));
}

LOCAL char *prog_append_statements(vec *sl, size_t *off, datum *source,
                                   datum **compdata) {
  for (int i = 0; i < list_length(source); ++i) {
    datum *stmt = list_at(source, i);
    if (i > 0) {
      prog_append_yield(sl, off, datum_make_list_of(3, datum_make_symbol("debugger"), datum_make_symbol("statement"), stmt), 0, 0, datum_make_nil(), compdata);
    }
    char *err = prog_append_statement(sl, off, stmt, compdata);
    if (err != NULL) {
      return err;
    }
  }
  return NULL;
}

LOCAL char *prog_append_statement(vec *sl, size_t *begin, datum *stmt,
                                  datum **compdata) {
  if (datum_is_constant(stmt)) {
    prog_append_put_const(sl, begin, stmt, compdata);
    return NULL;
  }
  if (datum_is_symbol(stmt)) {
    prog_append_yield(
                      sl, begin, datum_make_list_of(3, datum_make_symbol("debugger"), datum_make_symbol("compdata"), *compdata), 0, 0, datum_make_nil(), compdata);
    prog_append_put_var(sl, begin, stmt, compdata);
    return NULL;
  }
  if (!datum_is_list(stmt)) {
    return "this datum cannot be a statement";
  }
  if (datum_is_nil(stmt)) {
    return "an empty list is not a statement";
  }
  datum *op = list_at(stmt, 0);

  if (datum_is_the_symbol(op, "req")) {
    return prog_append_usages(sl, begin, stmt, compdata);
  }
  if (datum_is_the_symbol(op, "export")) {
    return prog_append_exports(sl, begin, stmt, compdata);
  }
  if (datum_is_the_symbol(op, "if")) {
    if (list_length(stmt) != 4) {
      return "if should have three args";
    }
    char *err;
    err = prog_append_statement(sl, begin, list_at(stmt, 1), compdata);
    if (err != NULL) {
      return err;
    }

    size_t true_end = vec_append_new(sl),
           false_end = vec_append_new(sl);

    *vec_at(sl, *begin) =
        *datum_make_list_of(3, datum_make_symbol(":if"), datum_make_int(true_end),
                           datum_make_int(false_end));
    *begin = vec_append_new(sl); // ???

    *compdata = compdata_del(*compdata);
    datum *false_compdata = *compdata;
    err = prog_append_statement(
        sl, &true_end, list_at(stmt, 2), compdata);
    if (err != NULL) {
      return err;
    }
    err = prog_append_statement(
        sl, &false_end, list_at(stmt, 3),
        &false_compdata);
    if (err != NULL) {
      return err;
    }
    if (!datum_eq(*compdata, false_compdata)) {
      *compdata =
          compdata_put(*compdata, datum_make_symbol("__different_if_branches"));
    }

    prog_join(sl, true_end, false_end, *begin);
    return NULL;
  }
  if (datum_is_the_symbol(op, "progn")) {
    char *err = prog_append_statements(sl, begin, list_get_tail(stmt), compdata);
    if (err != NULL) {
      return err;
    }
    return NULL;
  }
  if (datum_is_the_symbol(op, "quote")) {
    if (list_length(stmt) != 2) {
      return "quote should have a single arg";
    }
    prog_append_put_const(sl, begin, list_at(stmt, 1), compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "def")) {
    if (list_length(stmt) != 3) {
      return "def should have two args";
    }
    char *err =
        prog_append_statement(sl, begin, list_at(stmt, 2),
                              compdata);
    if (err != NULL) {
      return err;
    }
    datum *names;
    if (datum_is_list(list_at(stmt, 1))) {
      names = list_at(stmt, 1);
    } else {
      names = datum_make_list_of(1, list_at(stmt, 1));
    }
    compdata_give_names(names, compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "defn")) {
    datum *name = list_at(stmt, 1);
    datum *args;
    datum *body;
    if (list_length(stmt) != 4) {
      return "wrong defn";
    }
    args = list_at(stmt, 2);
    body = list_at(stmt, 3);
    size_t s_off = vec_append_new(sl);
    datum *routine_compdata = compdata_put(*compdata, name);
    routine_compdata = compdata_start_new_section(routine_compdata);
    char *err = prog_init_routine(sl, s_off, args, body, &routine_compdata);
    if (err != NULL) {
      return err;
    }
    prog_append_put_prog(sl, begin, s_off, 2, compdata);
    compdata_give_names(datum_make_list_of(1, name), compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "return")) {
    datum *target = NULL;
    size_t recieve_count = 1;
    int index = 1;
    while (index < list_length(stmt)) {
      datum *tag = list_at(stmt, index);
      if (!datum_is_list(tag) || list_length(tag) != 2 ||
          !datum_is_the_symbol(list_at(tag, 0), "at")) {
        break;
      }
      datum *content = list_at(tag, 1);
      if (datum_is_integer(content)) {
        recieve_count = content->integer_value;
        ++index;
      } else if (target == NULL) {
        target = content;
        ++index;
      } else {
        return "unknown return tag";
      }
    }
    size_t argcnt = list_length(stmt) - index;
    for (; index < list_length(stmt); ++index) {
      datum *component = list_at(stmt, index);
      char *err = prog_append_statement(sl, begin, component, compdata);
      if (err != NULL) {
        return err;
      }
    }
    if (target == NULL) {
      target = datum_make_symbol("plain");
    }
    prog_append_yield(
        sl, begin, target,
        argcnt, recieve_count, datum_make_nil(), compdata);
    return NULL;
  }
  if (datum_is_the_symbol(op, "backquote")) {
    if (list_length(stmt) != 2) {
      return "backquote should have a single arg";
    }
    return prog_append_backquoted_statement(
        sl, begin, list_at(stmt, 1), compdata);
  }
  if (datum_is_the_symbol(op, "host")) {
    if (list_length(stmt) < 2) {
      return "host should have at least one arg";
    }
    datum *name = list_at(stmt, 1);
    size_t nargs = list_length(stmt) - 2;
    for (int i = 2; i < list_length(stmt); ++i) {
      datum *arg = list_at(stmt, i);
      prog_append_statement(sl, begin, arg, compdata);
    }
    prog_append_yield(sl, begin,
                      datum_make_list_of(2, datum_make_symbol("host"), name), nargs,
                      1, datum_make_nil(), compdata);
    return NULL;
  }

  datum *fn = list_at(stmt, 0);
  bool hash = false;
  datum *target = NULL;
  bool at = false;
  size_t ret_count = 1;
  for (; datum_is_list(fn) && list_length(fn) == 2 &&
           datum_is_symbol(list_at(fn, 0));
       fn = list_at(fn, 1)) {
    char *tag = list_at(fn, 0)->symbol_value;
    if (!strcmp(tag, "hash")) {
      hash = true;
    } else if (!strcmp(tag, "at")) {
      at = true;
    } else {
      break;
    }
  }
  datum *mainname;
  datum *subname;
  if (datum_is_list(fn) && !datum_is_nil(fn) &&
      datum_is_the_symbol(list_at(fn, 0), "polysym")) {
    mainname = list_at(fn, 1);
    subname = list_at(fn, 2);
  } else {
    mainname = fn;
    subname = NULL;
  }
  int index = 1;
  while (index < list_length(stmt)) {
    datum *tag = list_at(stmt, index);
    if (!datum_is_list(tag) || list_length(tag) != 2 ||
        !datum_is_the_symbol(list_at(tag, 0), "at")) {
      break;
    }
    datum *content = list_at(tag, 1);
    if (datum_is_integer(content)) {
      ret_count = content->integer_value;
      ++index;
    } else if (target == NULL) {
      target = content;
      ++index;
    } else {
      return "unknown tag";
    }
  }
  datum *fn_index;
  datum *subfn_index = datum_make_nil();
  if (at || subname != NULL) {
    if (!datum_is_symbol(mainname)) {
      return "expected an lvalue";
    }
    fn_index = compdata_get_polyindex(*compdata, mainname);
    if (datum_is_nil(fn_index)) {
      char *err = malloc(256);
      *err = 0;
      sprintf(err, "function not found: %s", datum_repr(mainname));
      return err;
    }
  } else {
    char *err =
        prog_append_statement(sl, begin, mainname, compdata);
    if (err != NULL) {
      return err;
    }
    fn_index = compdata_get_top_polyindex(*compdata);
  }
  if (subname != NULL) {
    char *err =
        prog_append_statement(sl, begin, subname, compdata);
    if (err != NULL) {
      return err;
    }
    subfn_index = compdata_get_top_polyindex(*compdata);
  }
  size_t arg_count = list_length(stmt) - index;
  for (; index < list_length(stmt); ++index) {
    datum *arg = list_at(stmt, index);
    if (hash) {
      prog_append_put_const(sl, begin, arg, compdata);
    } else {
      char *err =
          prog_append_statement(sl, begin, arg, compdata);
      if (err != NULL) {
        return err;
      }
    }
  }
  if (target == NULL) {
    target = datum_make_symbol("plain");
  }
  prog_append_call(sl, begin, fn_index, subfn_index, !at,
                   target,
                   arg_count, ret_count, compdata);
  return NULL;
}

LOCAL char *prog_append_usages(vec *sl, size_t *begin, datum *spec,
                               datum **compdata) {
  fdatum res = prog_read_usages(spec);
  if (fdatum_is_panic(res)) {
    return res.panic_message;
  }
  datum *re = &res.ok_value;
  if (!datum_is_list(re) || list_length(re) != 2) {
    return "not gonna happen";
  }
  datum *vars = list_at(re, 0);
  prog_append_recieve(sl, begin, vars, list_at(re, 1), compdata);
  return NULL;
}

LOCAL fdatum prog_read_usages(datum *spec) {
  if (!datum_is_list(spec) || list_length(spec) == 0 ||
      !datum_is_the_symbol(list_at(spec, 0), "req")) {
    return fdatum_make_panic("wrong usage spec");
  }
  int index = 1;
  datum *vars = datum_make_nil();
  datum *specs = datum_make_nil();
  for (; index < list_length(spec); ++index) {
      datum *item = list_at(spec, index);
    if (!datum_is_list(item) || list_length(item) < 2 ||
        list_length(item) > 3) {
      return fdatum_make_panic("wrong usage spec");
    }
    datum *item_var = list_at(item, 0);
    if (!datum_is_symbol(item_var)) {
      return fdatum_make_panic("wrong usage spec");
    }
    
    datum *item_spec;
    if (list_length(item) == 2) {
      item_spec = datum_make_list_of(1, list_at(item, 1));
    } else if (list_length(item) == 3) {
      item_spec = datum_make_list_of(2, list_at(item, 1), list_at(item, 2));
    } else {
      return fdatum_make_panic("wrong usage spec");
    }
    list_append(vars, item_var);
    list_append(specs, item_spec);
  }
  return fdatum_make_ok(*datum_make_list_of(2, vars, specs));
}

LOCAL char *prog_append_exports(vec *sl, size_t *begin, datum *spec,
                                datum **compdata) {
  fdatum res = prog_read_exports(spec);
  if (fdatum_is_panic(res)) {
    return res.panic_message;
  }
  datum *re = &res.ok_value;
  if (!datum_is_list(re) || list_length(re) != 2) {
    return "not gonna happen";
  }
  datum *exprs = list_at(re, 1);
  for (int i = 0; i < list_length(exprs); ++i) {
    datum *expr = list_at(exprs, i);
    prog_append_statement(sl, begin, expr, compdata);
  }
  /* This nop is appended as a hack so that the yield becomes the last statement
   * on the slice. */
  prog_append_nop(sl, begin);
  prog_append_yield(sl, begin, datum_make_symbol("plain"),
                    list_length(list_at(re, 0)), 1, list_at(re, 0), compdata);
  return NULL;
}

EXPORT void prog_append_call(vec *sl, size_t *begin, datum *fn_index,
                             datum *subfn_index, bool pop_one, datum *type,
                             int arg_count, int return_count,
                             datum **compdata) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) = *(datum_make_list_of(8, 
      datum_make_symbol(":call"), fn_index, subfn_index,
      datum_make_int(pop_one), type, datum_make_int(arg_count),
      datum_make_int(return_count), datum_make_int(next)));
  for (int i = 0; i < arg_count; ++i) {
    *compdata = compdata_del(*compdata);
  }
  if (pop_one) {
    *compdata = compdata_del(*compdata);
  }
  for (int i = 0; i < return_count; ++i) {
    *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
  }
  *begin = next;
}

EXPORT void prog_append_put_var(vec *sl, size_t *begin, datum *val,
                                datum **compdata) {
  size_t next = vec_append_new(sl);
  if (!datum_is_symbol(val)) {
    fprintf(stderr, "expected a symbol in put-var\n");
    exit(1);
  }
  datum *polyindex = compdata_get_polyindex(*compdata, val);
  if (datum_is_nil(polyindex)) {
    fprintf(stderr, "undefined variable: %s\n", val->symbol_value);
    exit(1);
  }
  *vec_at(sl, *begin) = *(datum_make_list_of(3, 
      datum_make_symbol(":put-var"), polyindex, datum_make_int(next)));
  *begin = next;
  *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
}

EXPORT void prog_append_put_prog(vec *sl, size_t *begin, size_t val,
                                 int capture, datum **compdata) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) =
      *(datum_make_list_of(4, datum_make_symbol(":put-prog"), datum_make_int(val),
                          datum_make_int(capture), datum_make_int(next)));
  *begin = next;
  *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
}

EXPORT void prog_append_yield(vec *sl, size_t *begin, datum *type,
                              size_t count, size_t recieve_count, datum *meta,
                              datum **compdata) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) = *(datum_make_list_of(6, 
      datum_make_symbol(":yield"), type, datum_make_int(count),
      datum_make_int(recieve_count), meta, datum_make_int(next)));
  *begin = next;
  for (size_t i = 0; i < count; ++i) {
    *compdata = compdata_del(*compdata);
  }
  for (size_t i = 0; i < recieve_count; ++i) {
    *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
  }
}

LOCAL char *prog_append_backquoted_statement(vec *sl, size_t *begin,
                                             datum *stmt, datum **compdata) {
  if (!datum_is_list(stmt)) {
    prog_append_put_const(sl, begin, stmt, compdata);
    return NULL;
  }
  for (int i = 0; i < list_length(stmt); ++i) {
    datum *elem = list_at(stmt, i);
    char *err;
    if (datum_is_list(elem) && list_length(elem) == 2 &&
        datum_is_the_symbol(list_at(elem, 0), "tilde")) {
      err = prog_append_statement(sl, begin, list_at(elem, 1),
                                  compdata);
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

LOCAL void prog_append_recieve(vec *sl, size_t *begin, datum *args,
                               datum *meta, datum **compdata) {
  prog_append_yield(sl, begin, datum_make_symbol("plain"), 0, list_length(args),
                    meta, compdata);
  compdata_give_names(args, compdata);
}

LOCAL fdatum prog_read_exports(datum *spec) {
  if (!datum_is_list(spec) || list_length(spec) == 0 ||
      !datum_is_the_symbol(list_at(spec, 0), "export")) {
    return fdatum_make_panic("wrong export spec");
  }
  int index = 1;
  datum *names = datum_make_nil();
  datum *expressions = datum_make_nil();
  for (; index < list_length(spec); ++index) {
    datum *item = list_at(spec, index);
    if (!datum_is_list(item) || list_length(item) != 2) {
      return fdatum_make_panic("wrong export spec");
    }
    datum *item_name = list_at(item, 0);
    if (!datum_is_symbol(item_name)) {
      return fdatum_make_panic("wrong export spec");
    }
    datum *item_expression = list_at(item, 1);
    list_append(names, item_name);
    list_append(expressions, item_expression);
  }
  return fdatum_make_ok(*datum_make_list_of(2, names, expressions));
}

LOCAL char *prog_init_routine(vec *sl, size_t s, datum *args,
                              datum *stmt, datum **routine_compdata) {
  if (args == NULL) {
    return "args can't be null";
  } else {
    prog_append_recieve(sl, &s, args, datum_make_nil(), routine_compdata);
  }
  return prog_append_statement(sl, &s, stmt, routine_compdata);
}

LOCAL void prog_append_put_const(vec *sl, size_t *begin, datum *val,
                                 datum **compdata) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) = *(datum_make_list_of(3, 
      datum_make_symbol(":put-const"), val, datum_make_int(next)));
  *begin = next;
  *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
}

EXPORT void prog_append_nop(vec *sl, size_t *begin) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) = *(
                          datum_make_list_of(2, datum_make_symbol(":nop"), datum_make_int(next)));
  *begin = next;
}

LOCAL void prog_append_collect(vec *sl, size_t count, size_t *begin,
                               datum **compdata) {
  size_t next = vec_append_new(sl);
  *vec_at(sl, *begin) =
      *(datum_make_list_of(3, datum_make_symbol(":collect"), datum_make_int(count),
                          datum_make_int(next)));
  *begin = next;
  for (size_t i = 0; i < count; ++i) {
    *compdata = compdata_del(*compdata);
  }
  *compdata = compdata_put(*compdata, datum_make_symbol(":anon"));
}

LOCAL void prog_join(vec *sl, size_t a, size_t b, size_t e) {
  *vec_at(sl, a) = *(datum_make_list_of(2, 
      datum_make_symbol(":nop"), datum_make_int(e)));
  *vec_at(sl, b) = *(datum_make_list_of(2, 
      datum_make_symbol(":nop"), datum_make_int(e)));
}

EXPORT datum *compdata_make() { return datum_make_list_of(1, datum_make_nil()); }

EXPORT bool compdata_has_value(datum *compdata) {
  compdata_validate(compdata);
  datum *outer_frame = list_get_last(compdata);
  return !datum_is_nil(outer_frame) && datum_is_the_symbol(list_get_last(outer_frame), ":anon");
}

LOCAL void compdata_validate(datum *compdata) {
  datum *outer_frame = list_get_last(compdata);
  if (!datum_is_nil(outer_frame) &&
      datum_is_the_symbol(list_get_last(outer_frame), "__different_if_branches")) {
    fprintf(stderr, "compdata_del: if branches had different compdata\n");
    fprintf(stderr, "%s\n", datum_repr(compdata));
    exit(EXIT_FAILURE);
  }
}

LOCAL datum *compdata_put(datum *compdata, datum *var) {
  compdata = datum_copy(compdata);
  datum *last_frame = list_get_last(compdata);
  list_append(last_frame, var);
  return compdata;
}

LOCAL datum *compdata_del(datum *compdata) {
  compdata_validate(compdata);
  compdata = datum_copy(compdata);
  datum *last_frame = list_get_last(compdata);
  list_pop(last_frame);
  return compdata;
}

EXPORT datum *compdata_get_polyindex(datum *compdata, datum *var) {
  int frames = list_length(compdata);
  assert(frames > 0);
  for (int frame = frames - 1; frame >= 0; --frame) {
    datum *comp = list_at(compdata, frame);
    int idx = list_index_of(comp, var);
    if (idx != -1) {
      return datum_make_list_of(2, datum_make_int(frame),
                               datum_make_int(idx));
    }
  }
  return datum_make_nil();
}

LOCAL datum *compdata_start_new_section(datum *compdata) {
  return list_copy_and_append(compdata, datum_make_nil());
}

EXPORT datum *compdata_get_top_polyindex(datum *compdata) {
  size_t frames = list_length(compdata);
  size_t indices = list_length(list_get_last(compdata));
  assert(frames > 0 && indices > 0);
  return datum_make_list_of(2, datum_make_int(frames - 1),
                           datum_make_int(indices - 1));
}

EXPORT datum *compdata_get_shape(datum *compdata) {
  datum *res = datum_make_nil();
  for (int i = 0; i < list_length(compdata); ++i) {
    res = list_copy_and_append(res, datum_make_int(list_length(list_at(compdata, i))));
  }
  return res;
}

EXPORT void compdata_give_names(datum *var, datum **compdata) {
  if (!datum_is_list(var)) {
    fprintf(stderr, "error: compdata_give_names\n");
    exit(EXIT_FAILURE);
  }
  for (int i = 0; i < list_length(var); ++i) {
    *compdata = compdata_del(*compdata);
  }
  for (int i = 0; i < list_length(var); ++i) {
    *compdata = compdata_put(*compdata, list_at(var, i));
  }
}

LOCAL datum *list_copy_and_append(datum *list, datum *value) {
  assert(datum_is_list(list));
  
  datum *e = datum_copy(list);
  list_append(e, value);
  return e;
}
