#include <assert.h>
#include <extern.h>
#include <string.h>

#if EXPORT_INTERFACE
typedef struct lisp_extension lisp_extension;

struct lisp_extension {
  extension base;
  vec program;
  size_t instruction;
  datum routine_;
  datum compdata;
  fdatum (*yield_handler)(datum *, datum *);
};
#endif

EXPORT lisp_extension lisp_extension_make(vec program, size_t instruction,
                                          datum routine_, datum compdata,
                                          fdatum (*yield_handler)(datum *,
                                                                  datum *)) {
  lisp_extension e = {{.call = lisp_extension_call},
                      program,
                      instruction,
                      routine_,
                      compdata,
                      yield_handler};
  return e;
}

EXPORT extension null_extension_make() {
  return (extension){null_extension_call};
}

LOCAL char *lisp_extension_call(extension *self_, vec *sl, size_t *begin,
                                datum *source, int *i, datum *compdata) {
  extension nu = null_extension_make();
  char *err = null_extension_call(&nu, sl, begin, source, i, compdata);
  if (err == NULL || strcmp(err, "<not an extension>")) {
    return err;
  }

  lisp_extension *self = (lisp_extension *)self_;
  datum *op = list_at(source, *i);
  if (!datum_is_symbol(op)) {
    return "<not an extension>";
  }
  char nm[128] = {0};
  sprintf(nm, ".%s", op->symbol_value);
  datum name = datum_make_symbol(nm);
  datum pi = compdata_get_polyindex(&self->compdata, &name);
  if (datum_is_nil(&pi)) {
     return "<not an extension>";
  }
  datum invokation_statement;
  if (datum_is_the_symbol(op, "defn2")) {
    *i += 4;
    invokation_statement = list_copy(source, *i - 4, *i);
  } else if (datum_is_the_symbol(op, "switch")) {
    *i += 3;
    invokation_statement = list_copy(source, *i - 3, *i);
  } else if (datum_is_the_symbol(op, "fntest")) {
    *i += 3;
    invokation_statement = list_copy(source, *i - 3, *i);
  } else if (datum_is_the_symbol(op, "backquote")) {
    *i += 2;
    invokation_statement = list_copy(source, *i - 2, *i);
  } else {
    return "<not an extension>";
  }
    *list_at(&invokation_statement, 0) =
        datum_make_list_of(datum_make_symbol("hash"), name);
    fdatum res = lisp_extension_run(&invokation_statement, self);
    if (fdatum_is_panic(res)) {
      return res.panic_message;
    }
    assert(datum_is_list(&res.ok_value));
    assert(list_length(&res.ok_value) == 1);
    // datum exprs = datum_make_list_of(*list_at(&res.ok_value, 0));
    return
        prog_append_expressions(sl, begin, &res.ok_value, compdata, self_);
}

LOCAL fdatum lisp_extension_run(datum *e, lisp_extension *est) {
  datum mod = datum_make_list_of(
      datum_make_symbol("return"),
      datum_make_list_of(datum_make_symbol("at"), datum_make_int(0)),
      datum_make_list_of(datum_make_symbol("at"), datum_make_symbol("halt")),
      datum_copy(e));

  extension ext = null_extension_make();
  // this is a hack in order to make the relocation possible.
  prog_append_nop(&est->program, &est->instruction);
  char *err = prog_compile_and_relocate(&est->program, &est->instruction, &mod,
                                        &est->compdata, &ext);
  if (err != NULL) {
    char err2[256];
    err2[0] = 0;
    strcat(err2, "error while invoking an extension: ");
    strcat(err2, err);
    return fdatum_make_panic(err2);
  }
  result res = routine_run_with_handler(est->program, &est->routine_,
                                        est->yield_handler);
  if (!datum_is_the_symbol(&res.type, "halt")) {
    return fdatum_make_panic(datum_repr(&res.value));
  }
  return fdatum_make_ok(res.value);
}

LOCAL char *null_extension_call(extension *self, vec *sl, size_t *begin,
                                datum *source, int *i, datum *compdata) {
  datum *op = list_at(source, *i);
  datum stmt;
  if (datum_is_the_symbol(op, "req")) {
    *i += 2;
    stmt = list_copy(source, *i - 2, *i);
    return prog_append_usages(sl, begin, &stmt, compdata, self);
  }
  if (datum_is_the_symbol(op, "export")) {
    *i += 2;
    stmt = list_copy(source, *i - 2, *i);
    return prog_append_exports(sl, begin, &stmt, compdata, self);
  }
  return "<not an extension>";
}

LOCAL char *prog_append_usages(vec *sl, size_t *begin, datum *spec,
                               datum *compdata, extension *ext) {
  fdatum res = prog_read_usages(spec);
  if (fdatum_is_panic(res)) {
    return res.panic_message;
  }
  datum re = res.ok_value;
  if (!datum_is_list(&re) || list_length(&re) != 2) {
    return "not gonna happen";
  }
  datum *vars = list_at(&re, 0);
  datum *meta = list_at(&re, 1);
  datum stmt = datum_make_list_of(
      datum_make_symbol("brackets"), datum_copy(vars), datum_make_symbol("="),
      datum_make_list_of(
          datum_make_symbol("brackets"), datum_make_symbol("return"),
          datum_make_list_of(datum_make_symbol("at"),
                             datum_make_int(list_length(vars))),
          datum_make_list_of(
              datum_make_symbol("at"),
              datum_make_list_of(datum_make_symbol("meta"), datum_copy(meta))),
          datum_make_list_of(datum_make_symbol("brackets"))));
  datum code = datum_make_list_of(stmt);
  prog_append_expressions(sl, begin, &code, compdata, ext);
  return NULL;
}

LOCAL fdatum prog_read_usages(datum *spec) {
  if (!datum_is_list(spec) || list_length(spec) == 0 ||
      !datum_is_the_symbol(list_at(spec, 0), "req")) {
    return fdatum_make_panic("wrong usage spec");
  }
  int index = 1;
  datum vars = datum_make_nil();
  datum specs = datum_make_nil();
  datum *items = list_at(spec, 1);
  for (; index < list_length(items); ++index) {
    datum *item = list_at(items, index);
    if (!datum_is_list(item) || list_length(item) < 2 ||
        list_length(item) > 3) {
      return fdatum_make_panic("wrong usage spec: wrong length");
    }
    datum *item_var = list_at(item, 0);
    if (!datum_is_symbol(item_var)) {
      return fdatum_make_panic("wrong usage spec: not a symbol");
    }

    datum item_spec;
    if (list_length(item) == 2) {
      item_spec = datum_make_list_of(datum_copy(list_at(item, 1)));
    } else if (list_length(item) == 3) {
      item_spec = datum_make_list_of(datum_copy(list_at(item, 1)),
                                     datum_copy(list_at(item, 2)));
    } else {
      return fdatum_make_panic("wrong usage spec: wrong item length");
    }
    list_append(&vars, datum_copy(item_var));
    list_append(&specs, item_spec);
  }
  return fdatum_make_ok(datum_make_list_of(vars, specs));
}

LOCAL char *prog_append_exports(vec *sl, size_t *begin, datum *spec,
                                datum *compdata, extension *ext) {
  fdatum res = prog_read_exports(spec);
  if (fdatum_is_panic(res)) {
    return res.panic_message;
  }
  datum re = res.ok_value;
  if (!datum_is_list(&re) || list_length(&re) != 2) {
    return "not gonna happen";
  }
  datum *meta = list_at(&re, 0);
  datum *exprs = list_at(&re, 1);

  datum return_expr = datum_make_list_of(
      datum_make_symbol("brackets"), datum_make_symbol("return"),
      datum_make_list_of(
          datum_make_symbol("at"),
          datum_make_list_of(datum_make_symbol("meta"), datum_copy(meta))));
  datum vals = datum_make_list_of(datum_make_symbol("brackets"));
  for (int i = 0; i < list_length(exprs); ++i) {
    list_append(&vals, datum_copy(list_at(exprs, i)));
  }
  list_append(&return_expr, vals);
  datum stmt =
      datum_make_list_of(datum_make_symbol("brackets"), datum_make_nil(),
                         datum_make_symbol("="), return_expr);
  datum code = datum_make_list_of(stmt);
  prog_append_expressions(sl, begin, &code, compdata, ext);

  return NULL;
}

LOCAL fdatum prog_read_exports(datum *spec) {
  if (!datum_is_list(spec) || list_length(spec) == 0 ||
      !datum_is_the_symbol(list_at(spec, 0), "export")) {
    return fdatum_make_panic("wrong export spec");
  }
  int index = 1;
  datum names = datum_make_nil();
  datum expressions = datum_make_nil();
  datum *items = list_at(spec, 1);
  for (; index < list_length(items); ++index) {
    datum *item = list_at(items, index);
    if (!datum_is_list(item) || list_length(item) != 2) {
      return fdatum_make_panic("wrong export spec");
    }
    datum *item_name = list_at(item, 0);
    if (!datum_is_symbol(item_name)) {
      return fdatum_make_panic("wrong export spec");
    }
    datum *item_expression = list_at(item, 1);
    list_append(&names, datum_copy(item_name));
    list_append(&expressions, datum_copy(item_expression));
  }
  return fdatum_make_ok(datum_make_list_of(names, expressions));
}
