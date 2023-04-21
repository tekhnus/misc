#include <assert.h>
#include <preprocessing.h>

#if EXPORT_INTERFACE
#include <zlisp/common.h>
struct expander_state {
  vec expander_sl;
  size_t expander_prg;
  datum expander_routine;
  datum expander_compdata;
  extension_fn expander_ext;
};
#endif

EXPORT struct expander_state expander_state_make() {
  struct expander_state e;
  e.expander_sl = vec_make(16 * 1024);
  e.expander_prg = vec_append_new(&e.expander_sl);
  size_t expander_builder_prg = vec_append_new(&e.expander_sl);
  e.expander_routine = routine_make(expander_builder_prg, NULL);
  e.expander_compdata = compdata_make();
  datum expander_builder_compdata = compdata_make();
  prog_build_init(&e.expander_sl, &e.expander_prg, &expander_builder_prg,
                  &e.expander_compdata, &expander_builder_compdata);
  e.expander_ext = (extension_fn){call_ext_for_macros, NULL};
  datum macro_init = datum_make_list_of(
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
  char *res = prog_build(&e.expander_sl, &e.expander_prg, &expander_builder_prg,
                         &macro_init, &e.expander_compdata,
                         &expander_builder_compdata, &set, &e.expander_ext);
  if (res) {
    fprintf(stderr, "while building macros: %s\n", res);
    exit(EXIT_FAILURE);
  }
  result init_res =
      routine_run_with_handler(e.expander_sl, &e.expander_routine, host_ffi);
  if (!datum_is_the_symbol(&init_res.type, "halt")) {
    fprintf(stderr, "while initializing macros: %s\n",
            datum_repr(&init_res.value));
    exit(EXIT_FAILURE);
  }
  return e;
}

EXPORT fdatum datum_expand(datum *e, struct expander_state *est) {
  datum mod = datum_make_list_of(datum_copy(e));
  datum set = datum_make_bytestring("c-prelude");
  char *err =
      prog_build(&est->expander_sl, &est->expander_prg, NULL, &mod,
                 &est->expander_compdata, NULL, &set, &est->expander_ext);
  if (err != NULL) {
    char err2[256];
    err2[0] = 0;
    strcat(err2, "error while compiling a macro: ");
    strcat(err2, err);
    return fdatum_make_panic(err2);
  }
  result res = routine_run_with_handler(est->expander_sl,
                                        &est->expander_routine, host_ffi);
  if (!datum_is_the_symbol(&res.type, "halt")) {
    return fdatum_make_panic(datum_repr(&res.value));
  }
  return fdatum_make_ok(res.value);
}
