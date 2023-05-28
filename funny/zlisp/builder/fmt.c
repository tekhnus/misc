#ifndef THIS_IS_JUST_A_WORKAROUND
#include <zlisp/common.h>
#endif
#include "extensions.h"
#include <assert.h>
#include <fmt.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("usage: %s <file>\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  lisp_extension ext;
  // ext = standard_extension_make();
  char *filename = argv[1];
  fprintf(stderr, "formatting %s\n", filename);
  FILE *f = fopen(filename, "r");
  if (f == NULL) {
    perror("while opening file (C host)");
    return EXIT_FAILURE;
  }
  read_result rr = datum_read_all(f);
  fclose(f);
  if (read_result_is_panic(rr)) {
    fprintf(stderr, "fmt couldn't parse source: %s\n", rr.panic_message);
    return EXIT_FAILURE;
  }
  assert(read_result_is_ok(rr));
  assert(datum_is_list(&rr.ok_value));
  f = fopen(filename, "w");
  if (f == NULL) {
    perror("while opening file (C host)");
    return EXIT_FAILURE;
  }
  datum source = rewrite(&rr.ok_value);
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
  datum res = datum_make_nil();
  for (int i = 0; i < list_length(source); ++i) {
    datum *elem = list_at(source, i);

    if (i + 3 < list_length(source) &&
        (datum_is_the_symbol(list_at(source, i), "if"))) {
      datum *head = list_at(source, i++);
      datum *cond = list_at(source, i++);
      datum *tb = list_at(source, i++);
      datum *fb = list_at(source, i++);
      --i;
      list_append(&res, rewrite(head));
      list_append(&res, rewrite(cond));
      list_append(&res, datum_make_list_of(datum_make_symbol("list"), rewrite(tb)));
      list_append(&res, datum_make_list_of(datum_make_symbol("list"), rewrite(fb)));
      continue;
    }
    list_append(&res, rewrite(elem));
  }
  return res;
}
