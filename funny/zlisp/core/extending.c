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

LOCAL fdatum lisp_extension_run(datum *e, lisp_extension *est) {
  datum exp = datum_make_list_of(
      datum_make_symbol("return"),
      datum_make_list_of(datum_make_symbol("at"), datum_make_int(0)),
      datum_make_list_of(datum_make_symbol("at"), datum_make_symbol("halt")),
      datum_copy(e));

  datum mod = datum_make_list_of(exp);
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
  if (self || sl || begin || stmt || compdata) {
  };
  return "<not an extension>";
}
