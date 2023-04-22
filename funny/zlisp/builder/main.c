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
  vec sl = vec_make(16 * 1024);
  // the interpreter will start from the first instruction,
  // so the first call of append_new must be for the starting point.
  size_t bp = vec_append_new(&sl);
  size_t p = vec_append_new(&sl);
  datum compdata = compdata_make();
  datum builder_compdata = compdata_make();
  prog_build_init(&sl, &p, &bp, &compdata, &builder_compdata);
  datum set = datum_make_bytestring(argv[1]);
  struct lisp_extension extension = standard_extension_make();
  char *err = prog_build(&sl, &p, &bp, &src.ok_value, &compdata,
                         &builder_compdata, &set, &extension.base);
  if (err != NULL) {
    fprintf(stderr, "compilation error: %s\n", err);
    return EXIT_FAILURE;
  }
  datum d = vec_to_datum(&sl);
  printf("%s\n", datum_repr_bounded(&d, 128));
  return EXIT_SUCCESS;
}

EXPORT datum *get_host_ffi_settings() { // used in lisp
  datum *res = malloc(sizeof(datum));
  *res = datum_make_bytestring("c-prelude");
  return res;
}

EXPORT char *prog_build(vec *sl, size_t *p, size_t *bp, datum *source,
                        datum *compdata, datum *builder_compdata,
                        datum *settings, extension *ext) {
  // this is a hack in order to make the relocation possible.
  prog_append_nop(sl, p);
  size_t start_p = *p;
  char *res = prog_compile_and_relocate(sl, p, source, compdata, ext);
  if (res != NULL) {
    return res;
  }
  if (!bp) {
    return NULL;
  }
  return prog_link_deps(sl, bp, builder_compdata, start_p, compile_module,
                        settings, ext);
}

LOCAL fdatum compile_module(char *module, datum *settings,
                            extension *extension) {
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
  datum compdata = compdata_make();
  return prog_compile(&src.ok_value, &compdata, extension);
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

LOCAL fdatum file_source(char *fname) {
  FILE *stre = fopen(fname, "r");
  if (stre == NULL) {
    perror("file_source");
    char err[1024];
    err[0] = 0;
    strcat(err, "Module not found: ");
    strcat(err, fname);
    return fdatum_make_panic(err);
  }

  read_result rr = datum_read_all(stre);
  if (read_result_is_panic(rr)) {
    return fdatum_make_panic(rr.panic_message);
  }
  assert(read_result_is_ok(rr));
  return fdatum_make_ok(rr.ok_value);
}
