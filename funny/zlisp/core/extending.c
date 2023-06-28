#include <assert.h>
#include <types.h>
#include <extending.h>
#include <string.h>
#include <zlisp/common.h>

EXPORT lisp_extension lisp_extension_make(vec program, datum routine_,
                                          datum compdata,
                                          result (*runner)(vec *, datum *, datum, context *)) {
  lisp_extension e = {{.call = lisp_extension_call},
                      program,
                      routine_,
                      compdata,
                      runner};
  return e;
}

EXPORT extension null_extension_make() {
  return (extension){null_extension_call};
}

LOCAL void lisp_extension_call(extension *self_, vec *sl, datum *source, int *i,
                               datum *compdata, context *ctxt) {
  extension nu = null_extension_make();
  int i_val = *i;
  null_extension_call(&nu, sl, source, i, compdata, ctxt);

  if (ctxt->aborted) {
    return;
  }
  if (i_val != *i) {
    return;
  }

  lisp_extension *self = (lisp_extension *)self_;
  datum *op = list_at(source, *i);
  if (!datum_is_symbol(op)) {
    return;
  }
  char nm[128] = {0};
  snprintf(nm, 128, ".%s", op->symbol_value);
  datum name = datum_make_symbol(nm);
  datum pi = compdata_get_polyindex(&self->compdata, &name);
  if (datum_is_nil(&pi)) {
    return;
  }
  char aritynm[128] = {0};
  snprintf(aritynm, 128, ".%s.arity", op->symbol_value);
  datum arity_statement = datum_make_symbol(aritynm);
  datum arityc = lisp_extension_run(&arity_statement, self, ctxt);
  if (ctxt->aborted) {
    return;
  }
  assert(datum_is_list(&arityc) && list_length(&arityc) == 1);
  datum *arityd = list_at(&arityc, 0);
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
  datum res = lisp_extension_run(&call_statement, self, ctxt);
  if (ctxt->aborted) {
    return;
  }
  assert(datum_is_list(&res));
  assert(list_length(&res) == 1);
  prog_compile(sl, list_at(&res, 0), compdata, self_, ctxt);
  if (ctxt->aborted) {
    return;
  }
  return;
}

LOCAL datum lisp_extension_run(datum *e, lisp_extension *est, context *ctxt) {
  datum mod = datum_make_list_of(
      datum_make_symbol("return"), datum_make_symbol("at"),
      datum_make_list_of(datum_make_int(0)), datum_make_symbol("at"),
      datum_make_list_of(datum_make_symbol("halt")), datum_copy(e));

  extension ext = null_extension_make();
  prog_compile(&est->program, &mod, &est->compdata, &ext, ctxt);

  if (ctxt->aborted) {
    return (datum){};
  }
  result res = est->runner(&est->program, &est->routine_,
    datum_make_nil(), ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  if (!datum_is_the_symbol(&res.type, "halt")) {
    abortf(ctxt, "%s", datum_repr(&res.value));
    return (datum){};
  }
  return res.value;
}

LOCAL void null_extension_call(extension *self, vec *sl, datum *source, int *i,
                               datum *compdata, context *ctxt) {
  datum *op = list_at(source, *i);
  datum stmt;
  if (datum_is_the_symbol(op, "req")) {
    *i += 2;
    stmt = list_copy(source, *i - 2, *i);
    prog_append_usages(sl, &stmt, compdata, self, ctxt);

    if (ctxt->aborted) {
      return;
    }
    return;
  }
  if (datum_is_the_symbol(op, "export")) {
    *i += 2;
    stmt = list_copy(source, *i - 2, *i);
    prog_append_exports(sl, &stmt, compdata, self, ctxt);

    if (ctxt->aborted) {
      return;
    }
    return;
  }
  return;
}

LOCAL void prog_append_usages(vec *sl, datum *spec, datum *compdata,
                              extension *ext, context *ctxt) {
  datum re = prog_read_usages(spec, ctxt);
  if (ctxt->aborted) {
    return;
  }
  if (!datum_is_list(&re) || list_length(&re) != 2) {
    abortf(ctxt, "not gonna happen");
    return;
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
  prog_compile(sl, &stmt, compdata, ext, ctxt);

  if (ctxt->aborted) {
    return;
  }
}

LOCAL datum prog_read_usages(datum *spec, context *ctxt) {
  if (!datum_is_list(spec) || list_length(spec) == 0 ||
      !datum_is_the_symbol(list_at(spec, 0), "req")) {
    abortf(ctxt, "wrong usage spec");
    return (datum){};
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
      abortf(ctxt, "wrong usage spec: wrong length");
      return (datum){};
    }
    datum *item_var = list_at(item, 0);
    if (!datum_is_symbol(item_var)) {
      abortf(ctxt, "wrong usage spec: not a symbol");
      return (datum){};
    }

    datum item_spec;
    if (list_length(item) == 2) {
      item_spec = datum_make_list_of(datum_copy(list_at(item, 1)));
    } else if (list_length(item) == 3) {
      item_spec = datum_make_list_of(datum_copy(list_at(item, 1)),
                                     datum_copy(list_at(item, 2)));
    } else {
      abortf(ctxt, "wrong usage spec: wrong item length");
      return (datum){};
    }
    vec_append(&vars, datum_copy(item_var));
    vec_append(&specs, item_spec);
  }
  return datum_make_list_of(datum_make_list(vars), datum_make_list(specs));
}

LOCAL void prog_append_exports(vec *sl, datum *spec, datum *compdata,
                               extension *ext, context *ctxt) {
  datum re = prog_read_exports(spec, ctxt);
  if (ctxt->aborted) {
    return;
  }
  if (!datum_is_list(&re) || list_length(&re) != 2) {
    abortf(ctxt, "not gonna happen");
    return;
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
  prog_compile(sl, &return_expr_, compdata, ext, ctxt);

  if (ctxt->aborted) {
    return;
  }
}

LOCAL datum prog_read_exports(datum *spec, context *ctxt) {
  if (!datum_is_list(spec) || list_length(spec) == 0 ||
      !datum_is_the_symbol(list_at(spec, 0), "export")) {
    abortf(ctxt, "wrong export spec");
    return (datum){};
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
      abortf(ctxt, "wrong export spec");
      return (datum){};
    }
    datum *item_name = list_at(item, 0);
    if (!datum_is_symbol(item_name)) {
      abortf(ctxt, "wrong export spec");
      return (datum){};
    }
    datum *item_expression = list_at(item, 1);
    vec_append(&names, datum_copy(item_name));
    vec_append(&expressions, datum_copy(item_expression));
  }
  return datum_make_list_of(datum_make_list(names),
                            datum_make_list(expressions));
}
