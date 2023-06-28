// zlisp interpreter.
#include <assert.h>
#include <types.h>
#if INTERFACE
#include <types.h>
#endif
#include <datum.h>
#if INTERFACE
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>

#endif
#include <stdlib.h>
#include <string.h>

#if EXPORT_INTERFACE
extern const int NON_FLAT;
extern const int FLAT_CHILDREN;
extern const int FLAT;

#define datum_make_list_of(...)                                                \
  datum_make_list_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),      \
                          (datum[]){__VA_ARGS__})

#define vec_make_of(...)                                                       \
  vec_make_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),             \
                   (datum[]){__VA_ARGS__})

#endif

const int NON_FLAT = 0;
const int FLAT_CHILDREN = 1;
const int FLAT = 2;

EXPORT bool datum_is_symbol(datum *e) { return e->type == DATUM_SYMBOL; }

EXPORT bool datum_is_integer(datum *e) { return e->type == DATUM_INTEGER; }

EXPORT bool datum_is_bytestring(datum *e) {
  return e->type == DATUM_BYTESTRING;
}

EXPORT datum datum_make_symbol(char *name) {
  datum e;
  e.type = DATUM_SYMBOL;
  size_t length = strlen(name);
  e.symbol_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e.symbol_value[i] = name[i];
  }
  return e;
}

EXPORT datum datum_make_bytestring(char *text) {
  datum e;
  e.type = DATUM_BYTESTRING;
  size_t length = strlen(text);
  e.bytestring_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e.bytestring_value[i] = text[i];
  }
  return e;
}

EXPORT datum datum_make_int(int64_t value) {
  datum e;
  e.type = DATUM_INTEGER;
  e.integer_value = value;
  return e;
}

EXPORT char *datum_repr(datum *e) {
  char *buf;
  size_t sz;
  FILE *mem = open_memstream(&buf, &sz);
  datum_repr_impl(mem, e, 128, 0, false, FLAT, " ");
  fclose(mem);
  return buf;
}

EXPORT char *datum_repr_pretty(datum *e, extension *ext) {
  char *buf;
  size_t sz;
  FILE *mem = open_memstream(&buf, &sz);
  ext = ext ? ext : ext;
  datum_repr_impl(mem, e, 128, 0, true, NON_FLAT, "\n\n");
  fclose(mem);
  return buf;
}

LOCAL char *escape_string(char *s) {
  size_t len = strlen(s) * 2;
  char *quoted = malloc(len);
  size_t i = 0;
  for (size_t j = 0; j < strlen(s); ++j) {
    if (s[j] == '\n') {
      quoted[i++] = '\\';
      quoted[i++] = 'n';
      continue;
    }
    quoted[i++] = s[j];
  }
  quoted[i++] = '\0';
  return quoted;
}

LOCAL size_t datum_repr_impl(FILE *buf, datum *e, size_t depth, size_t start, bool pretty,
                            int flat, char *spacing) {
  size_t offset = 0;
  if (depth == 0) {
    offset += fprintf(buf, "<truncated>");
  } else if (datum_is_integer(e)) {
    offset += fprintf(buf, "%" PRId64, e->integer_value);
  } else if (datum_is_list(e)) {
    int first = 0;
    char *pair = "{}";
    char *sep = spacing;
    if (datum_is_list(e) && list_length(e) == 2 &&
        datum_is_the_symbol(list_at(e, 0), "call")) {
      first = 0;
      e = list_at(e, 1);
      pair = "()";
    } else if (pretty && list_length(e) > 0 &&
               datum_is_the_symbol(list_at(e, 0), "polysym")) {
      first = 1;
      pair = "";
      sep = "/";
    }
    if (strlen(pair) > 0) {
      offset += fprintf(buf, "%c", pair[0]);
    }
    int inhibit_newline = -100;
    int inhibit_double_newline = -100;
    int inhibit_child_newlines = -100;
    for (int i = first; i < list_length(e); ++i) {
      if (i > first) {
        if (sep[0] != '\n') {
          offset += fprintf(buf, "%s", sep);
        } else if (inhibit_newline >= 0 || flat == FLAT) {
          offset += fprintf(buf, " ");
        } else if (inhibit_double_newline >= 0) {
          offset += fprintf(buf, "\n");
          for (size_t i = 0; i < start; ++i) {
            offset += fprintf(buf, " ");
          }
        } else {
          offset += fprintf(buf, "%s", sep);
          for (size_t i = 0; i < start; ++i) {
            offset += fprintf(buf, " ");
          }
        }
      }
      datum *item = list_at(e, i);
      if (datum_is_the_symbol(item, "backquote")) {
        offset += fprintf(buf, "`");
        item = list_at(list_at(e, ++i), 0);
      }
      if (datum_is_the_symbol(item, "tilde")) {
        offset += fprintf(buf, "~");
        item = list_at(list_at(e, ++i), 0);
      }
      if (datum_is_the_symbol(item, "flat")) {
        offset += fprintf(buf, "^");
        item = list_at(list_at(e, ++i), 0);
      }
      if (datum_is_the_symbol(item, "at")) {
        offset += fprintf(buf, "@");
        item = list_at(list_at(e, ++i), 0);
      }
      if (datum_is_the_symbol(item, "quote") && i + 1 < list_length(e) &&
          datum_is_list(list_at(e, i + 1)) &&
          list_length(list_at(e, i + 1)) == 1) {
        offset += fprintf(buf, "'");
        ++i;
        assert(i < list_length(e));
        if (!datum_is_list(list_at(e, i))) {
          fprintf(stderr, "%s\n", datum_repr(list_at(e, i)));
        }
        assert(datum_is_list(list_at(e, i)));
        assert(!datum_is_nil(list_at(e, i)));
        item = list_at(list_at(e, i), 0);
      }
      if (datum_is_the_symbol(item, "defnx")) {
        inhibit_newline = 2;
      } else if (datum_is_the_symbol(item, "fn")) {
        inhibit_newline = 1;
        inhibit_double_newline = 2;
        inhibit_child_newlines = 1;
      } else if (datum_is_the_symbol(item, "fntest")) {
        inhibit_double_newline = 2;
      } else if (datum_is_the_symbol(item, "return")) {
        inhibit_newline = 1;
      } else if (datum_is_the_symbol(item, "if")) {
        inhibit_newline = 1;
      } else if (datum_is_the_symbol(item, "while")) {
        inhibit_newline = 1;
      } else if (datum_is_the_symbol(item, "switch")) {
        inhibit_newline = 1;
      } else if (i + 1 < list_length(e) &&
                 datum_is_the_symbol(list_at(e, i + 1), "=")) {
        inhibit_newline = 2;
      } else if (i + 1 < list_length(e) &&
                 datum_is_the_symbol(list_at(e, i + 1), ":=")) {
        inhibit_newline = 2;
        inhibit_child_newlines = 1;
      } else if (datum_is_the_symbol(item, "req")) {
        inhibit_newline = 0;
        inhibit_double_newline = 1;
        inhibit_child_newlines = 1;
      } else if (datum_is_the_symbol(item, "export")) {
        inhibit_newline = 0;
        inhibit_double_newline = 1;
        inhibit_child_newlines = 1;
      }
      char *child_sep = "\n";
      if (datum_is_list(item) && list_length(item) > 0 &&
          datum_is_the_symbol(list_at(item, 0), "call")) {
        child_sep = " ";
      }
      int child_flatness = flat;
      if (flat == FLAT_CHILDREN) {
        child_flatness = FLAT;
      }
      if (inhibit_newline >= 0 && inhibit_child_newlines >= 0) {
        child_flatness = FLAT;
      }
      if (inhibit_child_newlines >= 0) {
        child_flatness = child_flatness == FLAT ? FLAT : FLAT_CHILDREN;
      }
      offset += datum_repr_impl(buf, item, depth - 1, start + 1, pretty,
                                     child_flatness, child_sep);
      --inhibit_newline;
      --inhibit_double_newline;
      --inhibit_child_newlines;
    }
    if (strlen(pair) > 0) {
      offset += fprintf(buf, "%c", pair[1]);
    }
  } else if (pretty && datum_is_the_symbol(e, "empty-symbol")) {
  } else if (datum_is_symbol(e)) {
    offset += fprintf(buf, "%s", e->symbol_value);
  } else if (datum_is_bytestring(e)) {
    offset += fprintf(buf, "\"%s\"", escape_string(e->bytestring_value));
  } else {
    offset += fprintf(buf, "<fmt not implemented>");
  }
  return offset;
}

EXPORT bool datum_eq(datum *x, datum *y) {
  if (datum_is_symbol(x) && datum_is_symbol(y)) {
    if (!strcmp(x->symbol_value, y->symbol_value)) {
      return true;
    }
    return false;
  }
  if (datum_is_integer(x) && datum_is_integer(y)) {
    if (x->integer_value == y->integer_value) {
      return true;
    }
    return false;
  }
  if (datum_is_bytestring(x) && datum_is_bytestring(y)) {
    if (!strcmp(x->bytestring_value, y->bytestring_value)) {
      return true;
    }
    return false;
  }
  if (datum_is_list(x) && datum_is_list(y)) {
    if (list_length(x) != list_length(y)) {
      return false;
    }
    for (int i = 0; i < list_length(x); ++i) {
      if (!datum_eq(list_at(x, i), list_at(y, i))) {
        return false;
      }
    }
    return true;
  }
  return false;
}

EXPORT bool datum_is_constant(datum *d) {
  return (datum_is_integer(d) || datum_is_bytestring(d) ||
          (datum_is_symbol(d) && d->symbol_value[0] == ':'));
}

EXPORT bool datum_is_the_symbol(datum *d, char *val) {
  return datum_is_symbol(d) && !strcmp(d->symbol_value, val);
}

EXPORT array array_make_uninitialized(size_t length) {
  array res;
  res.begin = malloc(length * sizeof(datum));
  res.length = length;
  return res;
}

EXPORT vec vec_make(size_t capacity) {
  vec res;
  res.storage = array_make_uninitialized(capacity);
  res.length = 0;
  return res;
}

EXPORT datum list_make_copies(size_t length, datum val) {
  return datum_make_list(vec_make_copies(length, val));
}

EXPORT vec vec_make_copies(size_t length, datum val) {
  vec res = vec_make(length);
  for (size_t i = 0; i < length; ++i) {
    vec_append(&res, datum_copy(&val));
  }
  return res;
}

EXPORT datum *vec_append(vec *s, datum x) {
  if (s->length == s->storage.length) {
    size_t new_capacity = (s->storage.length + 1) * 2;
    datum *new_begin = malloc(sizeof(datum) * new_capacity);
    for (size_t i = 0; i < s->length; ++i) {
      new_begin[i] = s->storage.begin[i];
    }
    s->storage.length = new_capacity;
    s->storage.begin = new_begin;
  }
  size_t res = s->length++;
  (s->storage.begin)[res] = x;
  return s->storage.begin + res;
}

EXPORT vec vec_make_of_impl(size_t count, datum *values) {
  vec e = vec_make(count);
  for (size_t i = 0; i < count; ++i) {
    datum elem = values[i];
    vec_append(&e, elem);
  }
  return e;
}

EXPORT datum *vec_at(vec *s, size_t index) {
  if (index >= s->length) {
    fprintf(stderr, "prog slice index overflow\n");
    assert(false);
  }
  return s->storage.begin + index;
}

EXPORT size_t vec_length(vec *s) { return s->length; }

LOCAL array vec_to_array(vec v) {
  if (v.length == v.storage.length) {
    return v.storage;
  }
  array res = array_make_uninitialized(vec_length(&v));
  for (size_t i = 0; i < vec_length(&v); ++i) {
    *array_at(&res, i) = *vec_at(&v, i);
  }
  return res;
}

EXPORT datum datum_make_list(vec v) {
  datum res = datum_make_nil();
  res.list_value = vec_to_array(v);
  return res;
}

EXPORT datum datum_make_nil() {
  datum e;
  e.type = DATUM_LIST;
  e.list_value = vec_make(0).storage;
  return e;
}

EXPORT bool datum_is_list(datum *e) { return e->type == DATUM_LIST; }

EXPORT bool datum_is_nil(datum *e) {
  return datum_is_list(e) && list_length(e) == 0;
}

EXPORT datum datum_make_list_of_impl(size_t count, datum *values) {
  vec vals = vec_make_copies(count, datum_make_nil());
  for (size_t i = 0; i < count; ++i) {
    datum elem = values[i];
    *vec_at(&vals, i) = elem;
  }
  return datum_make_list(vals);
}

EXPORT size_t array_length(array *arr) { return arr->length; }

EXPORT datum *array_at(array *arr, size_t i) { return arr->begin + i; }

EXPORT int list_length(datum *seq) {
  assert(datum_is_list(seq));
  return array_length(&seq->list_value);
}

EXPORT datum *list_at(datum *list, unsigned index) {
  assert(datum_is_list(list));
  return array_at(&list->list_value, index);
}

EXPORT datum list_copy(datum *list, int from, int to) {
  datum res = list_make_copies(to - from, datum_make_nil());
  for (int i = 0; from + i < to; ++i) {
    *list_at(&res, i) = datum_copy(list_at(list, from + i));
  }
  return res;
}

EXPORT datum *list_get_last(datum *list) {
  assert(datum_is_list(list));
  assert(list_length(list) > 0);
  return list_at(list, list_length(list) - 1);
}

EXPORT datum list_get_tail(datum *list) {
  assert(datum_is_list(list));
  assert(list_length(list) > 0);
  datum e = list_make_copies(list_length(list) - 1, datum_make_nil());
  for (int i = 1; i < list_length(list); ++i) {
    *list_at(&e, i - 1) = datum_copy(list_at(list, i));
  }
  return e;
}

EXPORT void vec_extend(vec *list, datum *another) {
  assert(datum_is_list(another));
  for (int i = 0; i < list_length(another); ++i) {
    vec_append(list, *list_at(another, i));
  }
}

EXPORT int list_index_of(datum *xs, datum *x) {
  assert(datum_is_list(xs));
  int i = list_length(xs) - 1;
  for (; i >= 0; --i) {
    if (datum_eq(list_at(xs, i), x)) {
      return i;
    }
  }
  return -1;
}

EXPORT datum datum_copy(datum *d) {
  if (datum_is_list(d)) {
    return list_copy(d, 0, list_length(d));
  }
  return *d;
}

EXPORT vec vec_copy(vec *src) {
  vec dst = vec_make(src->storage.length);
  for (size_t i = 0; i < src->length; ++i) {
    vec_append(&dst, datum_copy(vec_at(src, i)));
  }
  return dst;
}

LOCAL vec array_to_vec(array arr) {
  vec res;
  res.length = arr.length;
  res.storage = arr;
  return res;
}

LOCAL array array_copy(array *arr) {
  array res = array_make_uninitialized(arr->length);
  for (size_t i = 0; i < arr->length; ++i) {
    *array_at(&res, i) = datum_copy(array_at(arr, i));
  }
  return res;
}

EXPORT vec list_to_vec(datum *val) {
  assert(datum_is_list(val));
  return array_to_vec(array_copy(&val->list_value));
}
