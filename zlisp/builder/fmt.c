#ifndef THIS_IS_JUST_A_WORKAROUND
#include <zlisp/common.h>
#endif
#include <assert.h>
#include <fmt.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  context ctxt = {};
  if (argc != 2) {
    printf("usage: %s <file>\n", argv[0]);
    return (EXIT_FAILURE);
  }
  lisp_extension ext;
  char *filename = argv[1];
  fprintf(stderr, "formatting %s\n", filename);
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
  f = fopen(filename, "w");
  if (f == NULL) {
    perror("while opening file (C host)");
    return EXIT_FAILURE;
  }
  datum src = datum_make_list_vec(rr);
  datum source = rewrite(&src);
  char *res = datum_repr_pretty(&source, &ext.base);
  res[strlen(res) - 1] = 0;
  res += 1;
  fprintf(f, "%s\n", res);
  fclose(f);
  return EXIT_SUCCESS;
}

LOCAL datum rewrite(datum *source) {
  if (!datum_is_list(source)) {
    return datum_copy(source);
  }
  vec res = vec_make(0);
  for (int i = 0; i < list_length(source); ++i) {
    datum *elem = list_at(source, i);
    vec_append(&res, rewrite(elem));
  }
  return datum_make_list_vec(res);
}
