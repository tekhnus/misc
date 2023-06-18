#include <assert.h>
#include <extensions.h>
#include <stdlib.h>
#if EXPORT_INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
#endif

EXPORT struct lisp_extension standard_extension_make() {
  context ctxt = {};
  vec program;
  datum routine_;
  datum compdata;
  standard_extension_init(&program, &routine_, &compdata, &ctxt);
  if (ctxt.aborted) {
    fprintf(stderr, "%s\n", "ext init fail");
    exit(EXIT_FAILURE);
  }
  return lisp_extension_make(program, routine_, compdata, host_ffi);
}

EXPORT extension *standard_extension_alloc_make() {
  // For Lisp.
  lisp_extension *res = malloc(sizeof(lisp_extension));
  *res = standard_extension_make();
  return &res->base;
}

LOCAL void standard_extension_init(vec *program, datum *routine_,
                                    datum *compdata, context *ctxt) {
  *program = vec_create_slice();
  size_t lisp_extension_builder_prg = 0;
  *routine_ = routine_make(lisp_extension_builder_prg, NULL);
  *compdata = compdata_make();
  datum lisp_extension_builder_compdata = compdata_make();
  lisp_extension_builder_prg =
      prog_build_init(program, compdata, &lisp_extension_builder_compdata, ctxt);
  if (ctxt->aborted) {
    return;
  }
  struct extension lisp_extension_ext = null_extension_make();
  char fname[256] = {0};
  module_to_filename(fname, "extensions");
  fdatum src = file_source(fname);
  if (fdatum_is_panic(src)) {
    abortf(ctxt, "%s", src.panic_message);
    return;
  }
  datum initialization_statements = src.ok_value;
  datum set = datum_make_bytestring("c-prelude");
  char *res = prog_build(
      program, &lisp_extension_builder_prg, &initialization_statements,
      compdata, &lisp_extension_builder_compdata, &set, &lisp_extension_ext);
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
  return;
}
