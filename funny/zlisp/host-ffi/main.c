// a basic CLI for zlisp interpreter.
#include <main.h>
#if INTERFACE
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp-impl/zlisp-impl.h>
#endif

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
  prog *p = prog_slice_append_new(&sl);
  char *err = prog_init_module_c_host(&sl, p, src.ok_value);
  if (err != NULL) {
    fprintf(stderr, "compilation error: %s\n", err);
    return EXIT_FAILURE;
  }
  fprintf(stderr, "compiled, %zu instructions\n", prog_slice_length(sl));
  // fprintf(stderr, "%s\n", datum_repr(prog_slice_to_datum(sl)));
  state *s = state_make_builtins();
  fdatum res = routine_run_and_get_value_c_host(sl, &s, p);
  if (fdatum_is_panic(res)) {
    fprintf(stderr, "runtime error: %s\n", res.panic_message);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
