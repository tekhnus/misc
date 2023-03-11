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
    fprintf(stderr, "parsing error (C host): %s\n", rr.panic_message);
    return EXIT_FAILURE;
  }
  vec sl = vec_make(16 * 1024);
  vec_extend(&sl, rr.ok_value);
  datum s = *routine_make(0, NULL); // running starts from the first instruction.
  fdatum res = routine_run_in_ffi_host(sl, &s);
  if (fdatum_is_panic(res)) {
    fprintf(stderr, "runtime error: %s\n", res.panic_message);
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
