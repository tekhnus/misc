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
                                datum *stmt, datum *compdata) {
  extension nu = null_extension_make();
  char *res = null_extension_call(&nu, sl, begin, stmt, compdata);
  if (res == NULL || strcmp(res, "<not an extension>")) {
    return res;
  }

  lisp_extension *self = (lisp_extension *)self_;
  datum *op = list_at(stmt, 0);
  datum pi = compdata_get_polyindex(&self->compdata, op);
  if (!datum_is_nil(&pi)) {
    datum invokation_statement = datum_copy(stmt);
    *list_at(&invokation_statement, 0) =
        datum_make_list_of(datum_make_symbol("hash"), datum_copy(op));
    fdatum res = lisp_extension_run(&invokation_statement, self);
    if (fdatum_is_panic(res)) {
      return res.panic_message;
    }
    assert(datum_is_list(&res.ok_value));
    for (int i = 0; i < list_length(&res.ok_value); ++i) {
      char *err = prog_append_expression(sl, begin, list_at(&res.ok_value, i),
                                         compdata, self_);
      if (err) {
        return err;
      }
    }
    return NULL;
  }
  return "<not an extension>";
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
                                datum *stmt, datum *compdata) {
  if (datum_is_list(stmt) && !datum_is_nil(stmt) && datum_is_the_symbol(list_at(stmt, 0), "req")) {
    return prog_append_usages(sl, begin, stmt, compdata, self);
  }
  if (datum_is_list(stmt) && !datum_is_nil(stmt) && datum_is_the_symbol(list_at(stmt, 0), "export")) {
    return prog_append_exports(sl, begin, stmt, compdata, self);
  }
  if (self || sl || begin || stmt || compdata) {
  };
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
      datum_copy(vars), datum_make_symbol("="),
      datum_make_list_of(
          datum_make_symbol("return"),
          datum_make_list_of(datum_make_symbol("at"),
                             datum_make_int(list_length(vars))),
          datum_make_list_of(datum_make_symbol("at"),
                             datum_make_list_of(datum_make_symbol("meta"),
                                                datum_copy(meta)))));
  prog_append_statement(sl, begin, &stmt, compdata, ext);
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

    datum item_spec;
    if (list_length(item) == 2) {
      item_spec = datum_make_list_of(datum_copy(list_at(item, 1)));
    } else if (list_length(item) == 3) {
      item_spec = datum_make_list_of(datum_copy(list_at(item, 1)),
                                     datum_copy(list_at(item, 2)));
    } else {
      return fdatum_make_panic("wrong usage spec");
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
          datum_make_symbol("return"),
          datum_make_list_of(datum_make_symbol("at"),
                             datum_make_list_of(datum_make_symbol("meta"),
                                                datum_copy(meta))));
  for (int i = 0; i < list_length(exprs); ++i) {
    list_append(&return_expr, datum_copy(list_at(exprs, i)));
  }
  datum stmt = datum_make_list_of(
      datum_make_nil(), datum_make_symbol("="),
      return_expr);
  prog_append_statement(sl, begin, &stmt, compdata, ext);

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
    list_append(&names, datum_copy(item_name));
    list_append(&expressions, datum_copy(item_expression));
  }
  return fdatum_make_ok(datum_make_list_of(names, expressions));
}
