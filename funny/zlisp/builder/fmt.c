#include <assert.h>
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
  read_result rr = datum_read_all(f);
  fclose(f);
  if (read_result_is_panic(rr)) {
    fprintf(stderr, "fmt couldn't parse source: %s\n",
            rr.panic_message);
    return EXIT_FAILURE;
  }
  assert(read_result_is_ok(rr));
  assert(datum_is_list(&rr.ok_value));
  f = fopen(filename, "w");
  if (f == NULL) {
    perror("while opening file (C host)");
    return EXIT_FAILURE;
  }
  fprintf(f, "%s\n", datum_format_bounded(&rr.ok_value, 128, 0, true, false));
  fclose(f);
  return EXIT_SUCCESS;
}
