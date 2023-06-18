// a basic CLI for zlisp interpreter.
#if INTERFACE
#include <zlisp/common.h>
#include <zlisp/host-ffi.h>
#endif
#include <assert.h>
#include <main.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

int main(int argc, char **argv) {
  context ctxt = {};
  if (argc != 3) {
    printf("usage: %s <prelude> <script>\n", argv[0]);
    exit(EXIT_FAILURE);
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
  fdatum src = file_source(filename_copy);
  if (fdatum_is_panic(src)) {
    fprintf(stderr, "file error: %s\n", src.panic_message);
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
    fprintf(stderr, "build_init error");
    return EXIT_FAILURE;
  }
  datum set = datum_make_bytestring(argv[1]);
  struct lisp_extension extension = standard_extension_make(&ctxt);
  if (ctxt.aborted) {
    fprintf(stderr, "ext make error");
    return EXIT_FAILURE;
  }
  prog_build(&sl, &bp, &src.ok_value, &compdata, &builder_compdata,
                         &set, &extension.base, &ctxt);
  if (ctxt.aborted) {
    fprintf(stderr, "compilation error: %s\n", "error");
    return EXIT_FAILURE;
  }
  datum d = datum_make_list(sl);
  printf("%s\n", datum_repr(&d));
  return EXIT_SUCCESS;
}

EXPORT extension *standard_extension_alloc_make() {
  // For Lisp.
  context ctxt = {};
  lisp_extension *res = malloc(sizeof(lisp_extension));
  *res = standard_extension_make(&ctxt);
  if (ctxt.aborted) {
    fprintf(stderr, "%s\n", "ext init fail");
    exit(EXIT_FAILURE);
  }
  return &res->base;
}

EXPORT char *prog_build_or_exit(vec *sl, size_t *bp, datum *source, datum *compdata,
                        datum *builder_compdata, datum *settings,
                        extension *ext) {
  // For Lisp.
  context ctxt = {};
  prog_build(sl, bp, source, compdata, builder_compdata, settings, ext, &ctxt);
  if (ctxt.aborted) {
    fprintf(stderr, "%s\n", "build fail");
    exit(EXIT_FAILURE);
  }
  return NULL;
}
