#include <assert.h>
#include <extern.h>
#include <string.h>

#if EXPORT_INTERFACE
typedef struct lisp_extension lisp_extension;

struct lisp_extension {
  extension base;
  vec program;
  datum routine_;
  datum compdata;
  fdatum (*yield_handler)(datum *, datum *);
};
#endif

EXPORT lisp_extension lisp_extension_make(vec program, datum routine_,
                                          datum compdata,
                                          fdatum (*yield_handler)(datum *,
                                                                  datum *)) {
  lisp_extension e = {{.call = lisp_extension_call},
                      program,
                      routine_,
                      compdata,
                      yield_handler};
  return e;
}

EXPORT extension null_extension_make() {
  return (extension){null_extension_call};
}

LOCAL char *lisp_extension_call(extension *self_, vec *sl, datum *source,
                                int *i, datum *compdata, context *ctxt) {
  extension nu = null_extension_make();
  int i_val = *i;
  char *err = null_extension_call(&nu, sl, source, i, compdata, ctxt);
  if (err != NULL) {
    return err;
  }
  if (i_val != *i) {
    return NULL;
  }

  lisp_extension *self = (lisp_extension *)self_;
  datum *op = list_at(source, *i);
  if (!datum_is_symbol(op)) {
    return NULL;
  }
  char nm[128] = {0};
  sprintf(nm, ".%s", op->symbol_value);
  datum name = datum_make_symbol(nm);
  datum pi = compdata_get_polyindex(&self->compdata, &name);
  if (datum_is_nil(&pi)) {
    return NULL;
  }
  char aritynm[128] = {0};
  sprintf(aritynm, ".%s.arity", op->symbol_value);
  datum arity_statement = datum_make_symbol(aritynm);
  fdatum arityc = lisp_extension_run(&arity_statement, self, ctxt);
  if (fdatum_is_panic(arityc)) {
    return arityc.panic_message;
  }
  assert(datum_is_list(&arityc.ok_value) && list_length(&arityc.ok_value) == 1);
  datum *arityd = list_at(&arityc.ok_value, 0);
  assert(datum_is_integer(arityd));
  int arity = arityd->integer_value;
  *i += arity;
  datum invokation_statement_ = list_copy(source, *i - arity, *i);
  vec invokation_statement = vec_make(0);
  vec_append(&invokation_statement, name);
  for (int i = 1; i < list_length(&invokation_statement_); ++i) {
    datum orig = datum_copy(list_at(&invokation_statement_, i));
    vec_append(&invokation_statement, datum_make_symbol("quote"));
    vec_append(&invokation_statement, datum_make_list_of(orig));
  }
  datum call_statement = datum_make_list_of(
      datum_make_symbol("call"), datum_make_list(invokation_statement));
  // fprintf(stderr, "call: %s\n", datum_repr(&call_statement));
  fdatum res = lisp_extension_run(&call_statement, self, ctxt);
  if (fdatum_is_panic(res)) {
    // fprintf(stderr, "call panic\n");
    return res.panic_message;
  }
  // fprintf(stderr, "call ok\n");
  assert(datum_is_list(&res.ok_value));
  assert(list_length(&res.ok_value) == 1);
  // datum src = list_copy(source, *i - arity, *i);
  // fprintf(stderr, "macro: %s -> %s\n", datum_repr(&src),
  // datum_repr(list_at(&res.ok_value, 0))); datum exprs =
  // datum_make_list_of(*list_at(&res.ok_value, 0));
  return prog_compile(sl, list_at(&res.ok_value, 0), compdata,
                                 self_);
}

LOCAL fdatum lisp_extension_run(datum *e, lisp_extension *est, context *ctxt) {
  datum mod = datum_make_list_of(
      datum_make_symbol("return"), datum_make_symbol("at"),
      datum_make_list_of(datum_make_int(0)), datum_make_symbol("at"),
      datum_make_list_of(datum_make_symbol("halt")), datum_copy(e));

  extension ext = null_extension_make();
  char *err =
      prog_compile(&est->program, &mod, &est->compdata, &ext);
  if (err != NULL) {
    char err2[256];
    err2[0] = 0;
    strcat(err2, "error while invoking an extension: ");
    strcat(err2, err);
    abortf(ctxt, "error");
    return fdatum_make_panic(err2);
  }
  result res = routine_run_with_handler(est->program, &est->routine_,
                                        est->yield_handler);
  if (!datum_is_the_symbol(&res.type, "halt")) {
    return fdatum_make_panic(datum_repr(&res.value));
  }
  return fdatum_make_ok(res.value);
}

LOCAL char *null_extension_call(extension *self, vec *sl, datum *source, int *i,
                                datum *compdata, context *ctxt) {
  datum *op = list_at(source, *i);
  datum stmt;
  if (datum_is_the_symbol(op, "req")) {
    *i += 2;
    stmt = list_copy(source, *i - 2, *i);
    return prog_append_usages(sl, &stmt, compdata, self, ctxt);
  }
  if (datum_is_the_symbol(op, "export")) {
    *i += 2;
    stmt = list_copy(source, *i - 2, *i);
    return prog_append_exports(sl, &stmt, compdata, self, ctxt);
  }
  return NULL;
}

LOCAL char *prog_append_usages(vec *sl, datum *spec, datum *compdata,
                               extension *ext, context *ctxt) {
  fdatum res = prog_read_usages(spec, ctxt);
  if (fdatum_is_panic(res)) {
    abortf(ctxt, "error");
    return res.panic_message;
  }
  datum re = res.ok_value;
  if (!datum_is_list(&re) || list_length(&re) != 2) {
    return "not gonna happen";
  }
  datum *vars = list_at(&re, 0);
  datum *meta = list_at(&re, 1);
  datum stmt = datum_make_list_of(
      datum_copy(vars), datum_make_symbol(":="), datum_make_symbol("return"),
      datum_make_symbol("at"),
      datum_make_list_of(datum_make_int(list_length(vars))),
      datum_make_symbol("at"),
      datum_make_list_of(
          datum_make_list_of(datum_make_symbol("meta"), datum_copy(meta))),
      datum_make_symbol("flat"), datum_make_list_of(datum_make_nil()));
  return prog_compile(sl, &stmt, compdata, ext);
}

LOCAL fdatum prog_read_usages(datum *spec, context *ctxt) {
  if (!datum_is_list(spec) || list_length(spec) == 0 ||
      !datum_is_the_symbol(list_at(spec, 0), "req")) {
    return fdatum_make_panic("wrong usage spec");
  }
  int index = 0;
  vec vars = vec_make(0);
  vec specs = vec_make(0);
  datum *items = list_at(spec, 1);
  for (; index < list_length(items); ++index) {
    datum *item_ = list_at(items, index);
    datum item_val = datum_copy(item_);
    datum *item = &item_val;
    if (!datum_is_list(item) || list_length(item) < 2 ||
        list_length(item) > 3) {
      abortf(ctxt, "error");
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
    vec_append(&vars, datum_copy(item_var));
    vec_append(&specs, item_spec);
  }
  return fdatum_make_ok(
      datum_make_list_of(datum_make_list(vars), datum_make_list(specs)));
}

LOCAL char *prog_append_exports(vec *sl, datum *spec, datum *compdata,
                                extension *ext, context *ctxt) {
  fdatum res = prog_read_exports(spec, ctxt);
  if (fdatum_is_panic(res)) {
    abortf(ctxt, "error");
    return res.panic_message;
  }
  datum re = res.ok_value;
  if (!datum_is_list(&re) || list_length(&re) != 2) {
    return "not gonna happen";
  }
  datum *meta = list_at(&re, 0);
  datum *exprs = list_at(&re, 1);

  vec return_expr =
      vec_make_of(datum_make_nil(), datum_make_symbol(":="),
                  datum_make_symbol("return"), datum_make_symbol("at"),
                  datum_make_list_of(datum_make_list_of(
                      datum_make_symbol("meta"), datum_copy(meta))));
  vec vals = vec_make(0);
  for (int i = 0; i < list_length(exprs); ++i) {
    vec_append(&vals, datum_copy(list_at(exprs, i)));
  }
  vec_append(&return_expr, datum_make_symbol("flat"));
  vec_append(&return_expr, datum_make_list_of(datum_make_list(vals)));
  datum return_expr_ = datum_make_list(return_expr);
  return prog_compile(sl, &return_expr_, compdata, ext);
}

LOCAL fdatum prog_read_exports(datum *spec, context *ctxt) {
  if (!datum_is_list(spec) || list_length(spec) == 0 ||
      !datum_is_the_symbol(list_at(spec, 0), "export")) {
    abortf(ctxt, "error");
    return fdatum_make_panic("wrong export spec");
  }
  int index = 0;
  vec names = vec_make(0);
  vec expressions = vec_make(0);
  datum *items = list_at(spec, 1);
  for (; index < list_length(items); ++index) {
    datum *item_ = list_at(items, index);
    datum item_val = datum_copy(item_);
    datum *item = &item_val;
    if (!datum_is_list(item) || list_length(item) != 2) {
      return fdatum_make_panic("wrong export spec");
    }
    datum *item_name = list_at(item, 0);
    if (!datum_is_symbol(item_name)) {
      return fdatum_make_panic("wrong export spec");
    }
    datum *item_expression = list_at(item, 1);
    vec_append(&names, datum_copy(item_name));
    vec_append(&expressions, datum_copy(item_expression));
  }
  return fdatum_make_ok(
      datum_make_list_of(datum_make_list(names), datum_make_list(expressions)));
}
