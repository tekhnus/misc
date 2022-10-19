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

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("usage: %s <script>\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  char filename_copy[1024] = {0};

  if (argv[1][0] != '/') {
    getcwd(filename_copy, sizeof(filename_copy));
    strcat(filename_copy, "/");
  }
  strcat(filename_copy, argv[1]);
  fdatum src = file_source(filename_copy);
  if (fdatum_is_panic(src)) {
    fprintf(stderr, "file error: %s\n", src.panic_message);
    return EXIT_FAILURE;
  }
  prog_slice sl = prog_slice_make(16 * 1024);
  // python will start from the first instruction,
  // so the first call of append_new must be for the starting point.
  size_t bp = prog_slice_append_new(&sl);
  size_t p = prog_slice_append_new(&sl);
  // datum *s = routine_2_make(bp);
  datum *compdata = compdata_make();
  datum *builder_compdata = compdata_make();
  prog_build_init(&sl, &p, &bp, &compdata, &builder_compdata);
  char *err = prog_build_2(&sl, &p, &bp, src.ok_value, python_module_routine, &compdata, &builder_compdata);

  if (err != NULL) {
    fprintf(stderr, "compilation error: %s\n", err);
    return EXIT_FAILURE;
  }
  printf("%s\n", datum_repr(prog_slice_to_datum(sl)));
  return EXIT_SUCCESS;
}

LOCAL char *python_module_routine(prog_slice *sl, size_t *p, char *module) {
  fdatum src = python_module_source(module);
  if (fdatum_is_panic(src)) {
    return src.panic_message;
  }
  datum *compdata = compdata_make();
  return prog_append_statements(sl, p, src.ok_value, &compdata, datum_make_list_1(datum_make_symbol(module)));
}

LOCAL fdatum python_module_source(char *module) {
  if (!strcmp(module, "prelude")) {
    module = "py-prelude";
  }
  char *fname = module_to_filename(module);
  return file_source(fname);
}
