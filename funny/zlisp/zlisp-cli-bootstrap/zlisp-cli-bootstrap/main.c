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
  ctx_t ns = namespace_make_eval_file(filename_copy);
  if (ctx_is_panic(ns)) {
    fprintf(stderr, "%s\n", ns.panic_message);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
