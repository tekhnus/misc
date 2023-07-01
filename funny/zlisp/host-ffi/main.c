// a basic CLI for zlisp interpreter.
#include <assert.h>
#include <main.h>

int main(int argc, char **argv) {
  context ctxt = {};
  if (argc != 2) {
    printf("usage: %s <file>\n", argv[0]);
    return (EXIT_FAILURE);
  }
  char *filename = argv[1];
  FILE *f = fopen(filename, "r");
  if (f == NULL) {
    perror("while opening file (C host)");
    return EXIT_FAILURE;
  }
  vec rr = datum_read_all(f, &ctxt);
  fclose(f);
  if (ctxt.aborted) {
    fprintf(stderr, "%s", ctxt.error);
    return EXIT_FAILURE;
  }
  assert(vec_length(&rr) == 1);
  vec sl = list_to_vec(vec_at(&rr, 0));
  datum s =
      routine_make_topmost(0); // running starts from the first instruction.
  result res = host_ffi_run(&sl, &s, datum_make_nil(), &ctxt);
  if (ctxt.aborted) {
    fprintf(stderr, "%s", ctxt.error);
    return EXIT_FAILURE;
  }
  if (!datum_is_the_symbol(&res.type, "halt")) {
    fprintf(stderr, "runtime error: %s %s\n", datum_repr(&res.type),
            datum_repr(&res.value));
    return EXIT_FAILURE;
  }
  return EXIT_SUCCESS;
}
