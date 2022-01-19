// a basic CLI for zlisp interpreter.
#include <zlisp-cli-bootstrap/main.h>
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
  prog *p = prog_make();
  char *err = prog_init_module_c_host(p, src.ok_value);
  if (err != NULL) {
    fprintf(stderr, "compilation error: %s\n", err);
    return EXIT_FAILURE;
  }
  state *s = state_make_builtins();
  fdatum res = routine_run_and_get_value_c_host(&s, p);
  if (fdatum_is_panic(res)) {
    fprintf(stderr, "runtime error: %s\n", res.panic_message);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
