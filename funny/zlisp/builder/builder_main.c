// a basic CLI for zlisp interpreter.
#if INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
#endif
#include <assert.h>
#include <builder_main.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
  context ctxt = {};
  if (argc != 3) {
    printf("usage: %s <prelude> <script>\n", argv[0]);
    return (EXIT_FAILURE);
  }
  char filename_copy[1024] = {0};

  if (argv[2][0] != '/') {
    char *res = getcwd(filename_copy, sizeof(filename_copy));
    if (res == NULL) {
      perror("while getting cwd");
      return EXIT_FAILURE;
    }
    strcat(filename_copy, "/");
  }
  strcat(filename_copy, argv[2]);
  datum src = file_source(filename_copy, &ctxt);
  if (ctxt.aborted) {
    fprintf(stderr, "while obtaining source: %s", ctxt.error);
    return EXIT_FAILURE;
  }
  vec sl = vec_create_slice();
  // the interpreter will start from the first instruction,
  // so the first call of append_new must be for the starting point.
  size_t bp;
  datum compdata = compdata_make();
  datum builder_compdata = compdata_make();
  bp = prog_build_init(&sl, &compdata, &builder_compdata, &ctxt);
  if (ctxt.aborted) {
    fprintf(stderr, "while build_init: %s", ctxt.error);
    return EXIT_FAILURE;
  }
  datum set = datum_make_bytestring(argv[1]);
  struct lisp_extension extension = standard_extension_make(&ctxt);
  if (ctxt.aborted) {
    fprintf(stderr, "while initializing extension: %s", ctxt.error);
    return EXIT_FAILURE;
  }
  prog_build(&sl, &bp, &src, &compdata, &builder_compdata, &set,
             &extension.base, &ctxt);
  if (ctxt.aborted) {
    fprintf(stderr, "while building: %s", ctxt.error);
    return EXIT_FAILURE;
  }
  datum d = datum_make_list(sl);
  printf("%s\n", datum_repr(&d));
  return EXIT_SUCCESS;
}
