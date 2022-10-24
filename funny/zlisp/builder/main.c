// a basic CLI for zlisp interpreter.
#if INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
#endif
#include <main.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char *prelude_module_name;

int main(int argc, char **argv) {
  if (argc != 3) {
    printf("usage: %s <prelude> <script>\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  prelude_module_name = argv[1];
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
  prog_slice sl = prog_slice_make(16 * 1024);
  // the interpreter will start from the first instruction,
  // so the first call of append_new must be for the starting point.
  size_t bp = prog_slice_append_new(&sl);
  size_t p = prog_slice_append_new(&sl);
  // datum *s = routine_2_make(bp);
  datum *compdata = compdata_make();
  datum *builder_compdata = compdata_make();
  prog_build_init(&sl, &p, &bp, &compdata, &builder_compdata);
  fdatum bytecode = prog_compile(src.ok_value, &compdata, datum_make_list_1(datum_make_symbol("main")));
  if (fdatum_is_panic(bytecode)) {
    fprintf(stderr, "%s\n", bytecode.panic_message);
    return EXIT_FAILURE;
  }
  char *err = prog_build(&sl, &p, &bp, bytecode.ok_value, compile_module, &builder_compdata);

  if (err != NULL) {
    fprintf(stderr, "compilation error: %s\n", err);
    return EXIT_FAILURE;
  }
  printf("%s\n", datum_repr_bounded(prog_slice_to_datum(sl), 128));
  return EXIT_SUCCESS;
}

LOCAL fdatum compile_module(char *module) {
  fdatum src = preprocessed_module_source(module);
  if (fdatum_is_panic(src)) {
    return src;
  }
  datum *compdata = compdata_make();
  return prog_compile(src.ok_value, &compdata, datum_make_list_1(datum_make_symbol(module)));
}

LOCAL fdatum preprocessed_module_source(char *module) {
  if (!strcmp(module, "prelude")) {
    module = prelude_module_name;
  }
  char *fname = module_to_filename(module);
  return file_source(fname);
}
