// a basic CLI for zlisp interpreter.
#if INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
#endif
#include <main.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
  if (argc != 3) {
    printf("usage: %s <prelude> <script>\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  char filename_copy[1024] = {0};

  if (argv[2][0] != '/') {
    getcwd(filename_copy, sizeof(filename_copy));
    strcat(filename_copy, "/");
  }
  strcat(filename_copy, argv[2]);
  fdatum src = file_source(filename_copy);
  if (fdatum_is_panic(src)) {
    fprintf(stderr, "file error: %s\n", src.panic_message);
    return EXIT_FAILURE;
  }
  vec sl = vec_make(16 * 1024);
  // the interpreter will start from the first instruction,
  // so the first call of append_new must be for the starting point.
  size_t bp = vec_append_new(&sl);
  size_t p = vec_append_new(&sl);
  datum compdata = *compdata_make();
  datum builder_compdata = *compdata_make();
  datum *compdata_ptr = &compdata;
  datum *builder_compdata_ptr = &builder_compdata;
  prog_build_init(&sl, &p, &bp, &compdata_ptr, &builder_compdata_ptr);
  char *err = prog_build(&sl, &p, &bp, &src.ok_value, &compdata_ptr,
                         &builder_compdata_ptr, datum_make_bytestring(argv[1]));
  if (err != NULL) {
    fprintf(stderr, "compilation error: %s\n", err);
    return EXIT_FAILURE;
  }
  datum d = vec_to_datum(&sl);
  printf("%s\n", datum_repr_bounded(&d, 128));
  return EXIT_SUCCESS;
}

EXPORT datum *get_host_ffi_settings() { // used in lisp
  return datum_make_bytestring("c-prelude");
}

EXPORT char *prog_build(vec *sl, size_t *p, size_t *bp, datum *source,
                        datum **compdata, datum **builder_compdata,
                        datum *settings) {
  fdatum bytecode = prog_compile(source, compdata);
  if (fdatum_is_panic(bytecode)) {
    return bytecode.panic_message;
  }
  // this is a hack in order to make the relocation possible.
  prog_append_nop(sl, p);
  size_t start_p = *p;
  char *res = vec_relocate(sl, p, &bytecode.ok_value);
  if (res != NULL) {
    return res;
  }
  int yield_count = compdata_has_value(*compdata) ? 1 : 0;
  datum nil = *datum_make_nil();
  prog_append_yield(sl, p, datum_make_symbol("halt"), yield_count, 0,
                    nil, compdata);
  return prog_link_deps(sl, bp, builder_compdata, start_p, compile_module,
                        settings);
}

LOCAL fdatum compile_module(char *module, datum *settings) {
  if (!datum_is_bytestring(settings)) {
    return fdatum_make_panic("settings should be a string");
  }
  if (!strcmp(module, "prelude")) {
    module = settings->bytestring_value;
  }
  char fname[1024] = {'\0'};
  module_to_filename(fname, module);
  fdatum src = file_source(fname);
  if (fdatum_is_panic(src)) {
    return src;
  }
  datum compdata = *compdata_make();
  datum *compdata_ptr = &compdata;
  return prog_compile(&src.ok_value, &compdata_ptr);
}

LOCAL void module_to_filename(char *fname, char *module) {
  char *zlisp_home = getenv("ZLISP");
  if (zlisp_home == NULL) {
    fprintf(stderr, "ZLISP variable not defined");
    exit(EXIT_FAILURE);
  }
  strcat(fname, zlisp_home);
  strcat(fname, "/");
  strcat(fname, module);
  strcat(fname, "/main.lisp");
}
