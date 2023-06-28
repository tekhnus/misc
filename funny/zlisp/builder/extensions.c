#include <assert.h>
#include <extensions.h>
#include <stdlib.h>
#if INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
#endif

EXPORT struct lisp_extension standard_extension_make(context *ctxt) {
  vec program;
  datum routine_;
  datum compdata;
  standard_extension_init(&program, &routine_, &compdata, ctxt);
  if (ctxt->aborted) {
    return (struct lisp_extension){};
  }
  return lisp_extension_make(program, routine_, compdata, host_ffi_run);
}

LOCAL void standard_extension_init(vec *program, datum *routine_,
                                    datum *compdata, context *ctxt) {
  *program = vec_create_slice();
  size_t lisp_extension_builder_prg = 0;
  *routine_ = routine_make_topmost(lisp_extension_builder_prg);
  *compdata = compdata_make();
  datum lisp_extension_builder_compdata = compdata_make();
  lisp_extension_builder_prg =
      prog_build_init(program, compdata, &lisp_extension_builder_compdata, ctxt);
  if (ctxt->aborted) {
    return;
  }
  struct extension lisp_extension_ext = null_extension_make();
  char fname[256] = {0};
  module_to_filename(fname, "extensions", ctxt);
  if (ctxt->aborted) {
    return;
  }
  datum initialization_statements = file_source(fname, ctxt);
  if (ctxt->aborted) {
    return;
  }
  datum set = datum_make_bytestring("c-prelude");
  prog_build(
      program, &lisp_extension_builder_prg, &initialization_statements,
      compdata, &lisp_extension_builder_compdata, &set, &lisp_extension_ext, ctxt);
  if (ctxt->aborted) {
    return;
  }
  result init_res = host_ffi_run(program, routine_, datum_make_nil(), ctxt);
  if (ctxt->aborted) {
    return;
  }
  if (!datum_is_the_symbol(&init_res.type, "halt")) {
    abortf(ctxt, "while initializing extensions: %s\n",
            datum_repr(&init_res.value));
    return;
  }
  return;
}
