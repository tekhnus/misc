#include <assert.h>
#include <extensions.h>
#if EXPORT_INTERFACE
#include <zlisp/common.h>
#endif

#if EXPORT_INTERFACE
typedef struct lisp_extension lisp_extension;

struct lisp_extension {
  struct extension base;
  vec program;
  size_t instruction;
  datum routine_;
  datum compdata;
};
#endif

EXPORT struct lisp_extension standard_extension_make() {
  vec program;
  size_t instruction;
  datum routine_;
  datum compdata;
  char *err =
      standard_extension_init(&program, &instruction, &routine_, &compdata);
  if (err != NULL) {
    fprintf(stderr, "%s\n", err);
    exit(EXIT_FAILURE);
  }
  return lisp_extension_make(program, instruction, routine_, compdata);
}

EXPORT struct lisp_extension lisp_extension_make(vec program,
                                                 size_t instruction,
                                                 datum routine_,
                                                 datum compdata) {
  struct lisp_extension e = {
      {.call = lisp_extension_call}, program, instruction, routine_, compdata};
  return e;
}

LOCAL char *standard_extension_init(vec *program, size_t *instruction,
                                    datum *routine_, datum *compdata) {
  *program = vec_make(16 * 1024);
  *instruction = vec_append_new(program);
  size_t lisp_extension_builder_prg = vec_append_new(program);
  *routine_ = routine_make(lisp_extension_builder_prg, NULL);
  *compdata = compdata_make();
  datum lisp_extension_builder_compdata = compdata_make();
  prog_build_init(program, instruction, &lisp_extension_builder_prg, compdata,
                  &lisp_extension_builder_compdata);
  struct extension lisp_extension_ext = trivial_extension_make();
  datum initialization_statements = datum_make_list_of(
      datum_make_list_of(datum_make_symbol("req"),
                         datum_make_list_of(datum_make_symbol("stdmacro"),
                                            datum_make_bytestring("stdmacro")),
                         datum_make_list_of(datum_make_symbol("fntest"),
                                            datum_make_bytestring("stdmacro"),
                                            datum_make_symbol("fntest")),
                         datum_make_list_of(datum_make_symbol("switch"),
                                            datum_make_bytestring("stdmacro"),
                                            datum_make_symbol("switch"))));
  datum set = datum_make_bytestring("c-prelude");
  char *res =
      prog_build(program, instruction, &lisp_extension_builder_prg,
                 &initialization_statements, compdata,
                 &lisp_extension_builder_compdata, &set, &lisp_extension_ext);
  if (res) {
    fprintf(stderr, "while building extensions: %s\n", res);
    exit(EXIT_FAILURE);
  }
  result init_res = routine_run_with_handler(*program, routine_, host_ffi);
  if (!datum_is_the_symbol(&init_res.type, "halt")) {
    fprintf(stderr, "while initializing extensions: %s\n",
            datum_repr(&init_res.value));
    exit(EXIT_FAILURE);
  }
  return NULL;
}

EXPORT extension *standard_extension_alloc_make() {
  // For Lisp.
  lisp_extension *res = malloc(sizeof(lisp_extension));
  *res = standard_extension_make();
  return &res->base;
}

LOCAL char *lisp_extension_call(struct extension *self_, vec *sl, size_t *begin,
                                datum *stmt, datum *compdata) {
  struct lisp_extension *self = (lisp_extension *)self_;
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
    *list_at(&invokation_statement, 0) = datum_make_list_of(
        datum_make_symbol("hash"),
        datum_make_list_of(datum_make_symbol("polysym"), datum_make_symbol(""),
                           datum_make_symbol("stdmacro"), datum_copy(op)));
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

LOCAL fdatum lisp_extension_run(datum *e, struct lisp_extension *est) {
  datum mod = datum_make_list_of(datum_copy(e));
  datum set = datum_make_bytestring("c-prelude");
  extension ext = null_extension_make();
  char *err = prog_build(&est->program, &est->instruction, NULL, &mod,
                         &est->compdata, NULL, &set, &ext);
  if (err != NULL) {
    char err2[256];
    err2[0] = 0;
    strcat(err2, "error while invoking an extension: ");
    strcat(err2, err);
    return fdatum_make_panic(err2);
  }
  result res = routine_run_with_handler(est->program, &est->routine_, host_ffi);
  if (!datum_is_the_symbol(&res.type, "halt")) {
    return fdatum_make_panic(datum_repr(&res.value));
  }
  return fdatum_make_ok(res.value);
}

LOCAL extension trivial_extension_make() {
  return (extension){trivial_extension_call};
}

LOCAL char *trivial_extension_call(struct extension *self, vec *sl,
                                   size_t *begin, datum *stmt,
                                   datum *compdata) {
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

LOCAL extension null_extension_make() {
  return (extension){null_extension_call};
}

LOCAL char *null_extension_call(struct extension *self, vec *sl, size_t *begin,
                                datum *stmt, datum *compdata) {
  if (self || sl || begin || stmt || compdata) {
  };
  return "<not an extension>";
}
