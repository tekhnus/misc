// a basic CLI for zlisp interpreter.
#include <main.h>
#include <compiling.h>


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
  size_t p = prog_slice_append_new(&sl);
  size_t bp = prog_slice_append_new(&sl);
  datum *s = routine_2_make(bp);
  datum *compdata = compdata_make();
  datum *builder_compdata = compdata_make();
  prog_build_init(&sl, &p, &bp, &compdata, &builder_compdata);
  char *err = prog_build_c_host(&sl, &p, &bp, src.ok_value, &compdata, &builder_compdata);
  if (err != NULL) {
    fprintf(stderr, "compilation error (C host): %s\n", err);
    return EXIT_FAILURE;
  }
  fprintf(stderr, "compiled, %zu instructions\n", prog_slice_length(sl));
  // fprintf(stderr, "%s\n", datum_repr(prog_slice_to_datum(sl)));
  fdatum res = routine_run_and_get_value_c_host_new(sl, &s);
  if (fdatum_is_panic(res)) {
    fprintf(stderr, "runtime error: %s\n", res.panic_message);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
