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

EXPORT extension trivial_extension_make() {
  return (extension){trivial_extension_call};
}

LOCAL extension null_extension_make() {
  return (extension){null_extension_call};
}

LOCAL char *lisp_extension_call(extension *self_, vec *sl, size_t *begin,
                                datum *stmt, datum *compdata) {
  lisp_extension *self = (lisp_extension *)self_;
  datum *op = list_at(stmt, 0);
  if (datum_is_the_symbol(op, "backquote")) {
    if (list_length(stmt) != 2) {
      return "backquote should have a single arg";
    }
    return prog_append_backquoted_statement(sl, begin, list_at(stmt, 1),
                                            compdata, self_);
  }
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
      char *err = prog_append_statement(sl, begin, list_at(&res.ok_value, i),
                                        compdata, self_);
      if (err) {
        return err;
      }
    }
    return NULL;
  }
  return "<not an extension>";
}

LOCAL char *prog_append_backquoted_statement(vec *sl, size_t *begin,
                                             datum *stmt, datum *compdata,
                                             extension *ext) {
  if (!datum_is_list(stmt)) {
    prog_append_put_const(sl, begin, stmt, compdata);
    return NULL;
  }
  for (int i = 0; i < list_length(stmt); ++i) {
    datum *elem = list_at(stmt, i);
    char *err;
    if (datum_is_list(elem) && list_length(elem) == 2 &&
        datum_is_the_symbol(list_at(elem, 0), "tilde")) {
      err = prog_append_statement(sl, begin, list_at(elem, 1), compdata, ext);
    } else {
      err = prog_append_backquoted_statement(sl, begin, elem, compdata, ext);
    }
    if (err != NULL) {
      return err;
    }
  }
  prog_append_collect(sl, list_length(stmt), begin, compdata);
  return NULL;
}

LOCAL fdatum lisp_extension_run(datum *e, lisp_extension *est) {
  datum mod = datum_make_list_of(datum_copy(e));
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
  int yield_count = compdata_has_value(&est->compdata) ? 1 : 0;
  datum nil = datum_make_nil();
  prog_append_yield(&est->program, &est->instruction, datum_make_symbol("halt"),
                    yield_count, 0, nil, &est->compdata);
  result res = routine_run_with_handler(est->program, &est->routine_,
                                        est->yield_handler);
  if (!datum_is_the_symbol(&res.type, "halt")) {
    return fdatum_make_panic(datum_repr(&res.value));
  }
  return fdatum_make_ok(res.value);
}

LOCAL char *trivial_extension_call(extension *self, vec *sl, size_t *begin,
                                   datum *stmt, datum *compdata) {
  datum *op = list_at(stmt, 0);
  if (datum_is_the_symbol(op, "backquote")) {
    if (list_length(stmt) != 2) {
      return "backquote should have a single arg";
    }
    return prog_append_backquoted_statement(sl, begin, list_at(stmt, 1),
                                            compdata, self);
  }
  return "<not an extension>";
}

LOCAL char *null_extension_call(extension *self, vec *sl, size_t *begin,
                                datum *stmt, datum *compdata) {
  if (self || sl || begin || stmt || compdata) {
  };
  return "<not an extension>";
}
