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
  char *res = datum_format_bounded(&source, 128, 0, true, false, "\n\n");
  res[strlen(res) - 1] = 0;
  res += 1;
  fprintf(f, "%s\n", res);
  fclose(f);
  return EXIT_SUCCESS;
}

LOCAL datum list_to_brackets(datum *list) {
  datum res = datum_make_list_of(datum_make_symbol("brackets"));
  for (int k = 0; k < list_length(list); ++k) {
    list_append(&res, datum_copy(list_at(list, k)));
  }
  return res;
}

LOCAL datum rewrite(datum *source) {
  if (!datum_is_list(source)) {
    return datum_copy(source);
  }
  /* if (datum_is_list(source) && list_length(source) == 2 && */
  /*     datum_is_the_symbol(list_at(source, 0), "brackets")) { */
  /*   return datum_copy(list_at(source, 1)); */
  /* } */
  if (datum_is_list(source) && list_length(source) == 3 &&
      datum_is_the_symbol(list_at(source, 0), "brackets") &&
      datum_is_the_symbol(list_at(source, 1), "quote")) {
    datum *val_ = list_at(source, 2);
    assert(datum_is_list(val_) && list_length(val_) > 0 && datum_is_the_symbol(list_at(val_, 0), "brackets"));
    datum vals = list_get_tail(val_);
    if (datum_is_list(&vals) && list_length(&vals) == 1) {
      datum *val = list_at(&vals, 0);
      if (datum_is_list(val) && list_length(val) > 0 && !datum_is_the_symbol(list_at(val, 0), "brackets")) {
        datum res = datum_make_list_of(datum_make_symbol("brackets"));
        for (int i = 0; i < list_length(val); ++i) {
          list_append(&res, datum_copy(list_at(val, i)));
        }
        return datum_make_list_of(datum_make_symbol("brackets"), datum_make_symbol("list"), datum_make_list_of(datum_make_symbol("brackets"), datum_make_symbol("quote"), res));
      }
    }
  }
  if (datum_is_list(source) && list_length(source) > 0 &&
      datum_is_the_symbol(list_at(source, 0), "tilde")) {
    datum vals = datum_make_list_of(datum_make_symbol("brackets"));
    for (int i = 0; i < list_length(source); ++i) {
      datum *elem = list_at(source, i);
      list_append(&vals, *elem);
    }
    return vals;
  }
  datum res = datum_make_nil();
  for (int i = 0; i < list_length(source); ++i) {
    datum *elem = list_at(source, i);
    if ((datum_is_list(elem) && list_length(elem) == 3 &&
         datum_is_the_symbol(list_at(elem, 1), "=")) ||
        (datum_is_list(elem) && list_length(elem) == 2 &&
         datum_is_the_symbol(list_at(elem, 0), "req")) ||
        (datum_is_list(elem) && list_length(elem) == 2 &&
         datum_is_the_symbol(list_at(elem, 0), "export")) ||
        (datum_is_list(elem) && list_length(elem) == 3 &&
         datum_is_the_symbol(list_at(elem, 0), "fntest"))) {
      for (int j = 0; j < list_length(elem); ++j) {
        list_append(&res, rewrite(list_at(elem, j)));
      }
      continue;
    }
    if (datum_is_list(elem) && i + 1 < list_length(source) &&
        datum_is_the_symbol(list_at(source, i + 1), "__xxx__") &&
        i + 2 < list_length(source)) {
      list_append(&res, list_to_brackets(list_at(source, i++)));
      list_append(&res, datum_copy(list_at(source, i++)));
      list_append(&res, datum_copy(list_at(source, i++)));
      --i;
      continue;
    }
    list_append(&res, rewrite(elem));
  }
  return res;
}
