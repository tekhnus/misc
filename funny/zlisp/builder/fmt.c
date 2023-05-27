#ifndef THIS_IS_JUST_A_WORKAROUND
#include <zlisp/common.h>
#endif
#include <assert.h>
#include <fmt.h>
#include <stdlib.h>
#include <string.h>

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
  char *res = datum_repr_pretty(&source);
  res[strlen(res) - 1] = 0;
  res += 1;
  fprintf(f, "%s\n", res);
  fclose(f);
  return EXIT_SUCCESS;
}

LOCAL datum rewrite(datum *source) {
  if (datum_is_the_symbol(source, "=")) {
    return datum_make_symbol(":=");
  }
  if (!datum_is_list(source)) {
    return datum_copy(source);
  }
  datum res = datum_make_nil();
  for (int i = 0; i < list_length(source); ++i) {
    datum *elem = list_at(source, i);
    /* if (i + 3 < list_length(source) && */
    /*     (datum_is_the_symbol(list_at(source, i), "defn") || */
    /*      datum_is_the_symbol(list_at(source, i), "defnx"))) { */
    /*   list_append(&res, rewrite(list_at(source, i++))); */
    /*   list_append(&res, rewrite(list_at(source, i++))); */
    /*   list_append(&res, rewrite(list_at(source, i++))); */
    /*   list_append(&res, rewrite(list_at(source, i++))); */
    /*   --i; */
    /*   continue; */
    /* } */
    list_append(&res, rewrite(elem));
  }
  return res;
}
