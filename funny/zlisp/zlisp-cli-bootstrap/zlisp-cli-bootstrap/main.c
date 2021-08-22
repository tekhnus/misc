// a basic CLI for zlisp interpreter.
#include <zlisp-impl/main.h>

#include <libgen.h>
#include <unistd.h>

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
  prog_t *p = prog_make();
  char *err = prog_init_from_file(p, filename_copy, true);
  if (err != NULL) {
    fprintf(stderr, "compilation error: %s\n", err);
    return EXIT_FAILURE;
  }
  state_t *s = state_make_fresh();
  fstate_t ns = state_eval(routine_make(p, s));
  if (fstate_is_panic(ns)) {
    fprintf(stderr, "runtime error: %s\n", ns.panic_message);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
