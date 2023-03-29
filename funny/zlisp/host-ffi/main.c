// a basic CLI for zlisp interpreter.
#include <main.h>

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("usage: %s <file>\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  char *filename = argv[1];
  FILE *f = fopen(filename, "r");
  if (f == NULL) {
    perror("while opening file (C host)");
    return EXIT_FAILURE;
  }
  read_result rr = datum_read(f);
  fclose(f);
  if (read_result_is_panic(rr)) {
    fprintf(stderr, "zlisp-run couldn't parse bytecode: %s\n", rr.panic_message);
    return EXIT_FAILURE;
  }
  if (!read_result_is_ok(rr)) {
    fprintf(stderr, "zlisp-run couldn't read bytecode: %d\n", rr.type);
    return EXIT_FAILURE;
  }
  vec sl = vec_make(16 * 1024);
  vec_extend(&sl, &rr.ok_value);
  datum s = routine_make(0, NULL); // running starts from the first instruction.
  result res = routine_run_with_handler(sl, &s, host_ffi);
  if (!datum_is_the_symbol(&res.type, "halt")) {
    fprintf(stderr, "runtime error: %s %s\n", datum_repr(&res.type), datum_repr(&res.value));
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
