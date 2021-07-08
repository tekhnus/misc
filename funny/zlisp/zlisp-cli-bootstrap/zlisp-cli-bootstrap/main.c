// a basic CLI for zlisp interpreter.
#include <zlisp-impl/main.h>

#include <libgen.h>
#include <unistd.h>

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("usage: %s <script>\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  eval_result_t prelude = namespace_make_prelude();
  if (eval_result_is_panic(prelude)) {
    fprintf(stderr, "%s\n", prelude.panic_message);
    exit(EXIT_FAILURE);
  }
  if (!eval_result_is_context(prelude)) {
    fprintf(stderr, "prelude expected to be a context, not a value");
    exit(EXIT_FAILURE);
  }
  namespace_t *ns = prelude.context_value;

  char filename_copy[1024] = {0};

  if (argv[1][0] != '/') {
    getcwd(filename_copy, sizeof(filename_copy));
    strcat(filename_copy, "/");
  }
  char arg_copy[1024];
  strcpy(arg_copy, argv[1]);
  strcat(filename_copy, dirname(arg_copy));
  datum_t *new_this_directory = datum_make_bytestring(filename_copy);
  ns = namespace_set(ns, datum_make_symbol("this-directory"), new_this_directory);

  FILE *f = fopen(argv[1], "r");
  if (f == NULL) {
    perror("error while opening the script file");
    exit(EXIT_FAILURE);
  }

  read_result_t rr;

  for (; read_result_is_ok(rr = datum_read(f));) {
    eval_result_t val = datum_eval(rr.ok_value, ns);
    if (eval_result_is_panic(val)) {
      fprintf(stderr, "%s\n", val.panic_message);
      exit(EXIT_FAILURE);
    }
    if (eval_result_is_ok(val)) {
      printf("the program should consist of statements\n");
      exit(EXIT_FAILURE);
    }
    ns = val.context_value;
  }
  if (read_result_is_right_paren(rr)) {
    fprintf(stderr, "unmatched closing bracket\n");
    exit(EXIT_FAILURE);
  } else if (read_result_is_panic(rr)) {
    fprintf(stderr, "%s\n", rr.panic_message);
    exit(EXIT_FAILURE);
  }

  return EXIT_SUCCESS;
}
