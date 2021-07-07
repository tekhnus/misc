// a basic CLI for zlisp interpreter.
#include <zlisp-impl/main.h>

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("usage: %s <script>\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  char *files[] = {argv[1]};

  namespace_t *ns = namespace_make_prelude();
  for (int i = 0; i < 2; ++i) {
    FILE *f = fopen(files[i], "r");
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
  }
  return EXIT_SUCCESS;
}
