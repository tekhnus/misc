// a basic CLI for zlisp interpreter.
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp-impl/zlisp-impl.h>

state *state_make_builtins();
fdatum file_source(char *file);
routine module_routine(char *module);

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
  char *err = prog_init_module(p, src.ok_value, module_routine);
  if (err != NULL) {
    fprintf(stderr, "compilation error: %s\n", err);
    return EXIT_FAILURE;
  }
  routine r = routine_make(p, state_make_builtins());
  fstate ns = routine_run(r);
  if (fstate_is_panic(ns)) {
    fprintf(stderr, "runtime error: %s\n", ns.panic_message);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
