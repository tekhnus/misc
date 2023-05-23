// zlisp interpreter.
#include <assert.h>
#include <extern.h>
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
#endif

const int NON_FLAT = 0;
const int FLAT_CHILDREN = 1;
const int FLAT = 2;

enum fdatumype {
  FDATUM_OK,
  FDATUM_PANIC,
};

EXPORT bool datum_is_symbol(datum *e) { return e->type == DATUM_SYMBOL; }

EXPORT bool datum_is_integer(datum *e) { return e->type == DATUM_INTEGER; }

EXPORT bool datum_is_bytestring(datum *e) {
  return e->type == DATUM_BYTESTRING;
}

EXPORT bool datum_is_frame(datum *e) { return e->type == DATUM_FRAME; }

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

EXPORT datum datum_make_frame(frame fr) {
  datum e;
  e.type = DATUM_FRAME;
  e.frame_value = fr;
  return e;
}

EXPORT char *datum_repr(datum *e) { return datum_repr_bounded(e, 8); }

EXPORT char *datum_repr_bounded(datum *e, size_t depth) {
  return datum_format_bounded(e, depth, 0, false, FLAT, " ");
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

EXPORT char *datum_format_bounded(datum *e, size_t depth, size_t start,
                                  bool pretty, int flat, char *spacing) {
  char *buf = malloc(1024 * sizeof(char));
  char *end = buf;
  if (depth == 0) {
    sprintf(buf, "<truncated>");
  } else if (datum_is_integer(e)) {
    sprintf(buf, "%" PRId64, e->integer_value);
  } else if (datum_is_list(e) && list_length(e) == 3 &&
             datum_is_the_symbol(list_at(e, 0), "brackets") &&
             datum_is_the_symbol(list_at(e, 1), "quote")) {
    end += sprintf(
        end, "'%s",
        datum_format_bounded(list_at(e, 2), depth, start, pretty, flat, "\n"));
  } else if (datum_is_list(e) && list_length(e) == 3 &&
             datum_is_the_symbol(list_at(e, 0), "brackets") &&
             datum_is_the_symbol(list_at(e, 1), "at")) {
    end += sprintf(
        end, "@%s",
        datum_format_bounded(list_at(e, 2), depth, start, pretty, flat, "\n"));
  } else if (datum_is_list(e) && list_length(e) == 3 &&
             datum_is_the_symbol(list_at(e, 0), "brackets") &&
             datum_is_the_symbol(list_at(e, 1), "backquote")) {
    end += sprintf(
        end, "`%s",
        datum_format_bounded(list_at(e, 2), depth, start, pretty, flat, "\n"));
  } else if (datum_is_list(e) && list_length(e) == 3 &&
             datum_is_the_symbol(list_at(e, 0), "brackets") &&
             datum_is_the_symbol(list_at(e, 1), "tilde")) {
    end += sprintf(
        end, "~%s",
        datum_format_bounded(list_at(e, 2), depth, start, pretty, flat, "\n"));
  } else if (datum_is_list(e)) {
    int first = 0;
    char *pair = "()";
    char *sep = spacing;
    if (list_length(e) > 0 && datum_is_the_symbol(list_at(e, 0), "brackets")) {
      first = 1;
      pair = "{}";
    }
    if (pretty && list_length(e) > 1 &&
        datum_is_the_symbol(list_at(e, 0), "brackets") &&
        datum_is_the_symbol(list_at(e, 1), "polysym")) {
      first = 2;
      pair = "";
      sep = "/";
    }
    if (strlen(pair) > 0) {
      end += sprintf(end, "%c", pair[0]);
    }
    int inhibit_newline = -100;
    int inhibit_double_newline = -100;
    int inhibit_child_newlines = -100;
    for (int i = first; i < list_length(e); ++i) {
      if (i > first) {
        if (sep[0] != '\n') {
          end += sprintf(end, "%s", sep);
        } else if (inhibit_newline >= 0 || flat == FLAT) {
          end += sprintf(end, " ");
        } else if (inhibit_newline == -1 || inhibit_double_newline >= 0) {
          end += sprintf(end, "\n");
          for (size_t i = 0; i < start; ++i) {
            end += sprintf(end, " ");
          }
        } else {
          end += sprintf(end, "%s", sep);
          for (size_t i = 0; i < start; ++i) {
            end += sprintf(end, " ");
          }
        }
      }
      datum *item = list_at(e, i);
      if (datum_is_the_symbol(item, "defn")) {
        inhibit_newline = 2;
      } else if (datum_is_the_symbol(item, "defnx")) {
        inhibit_newline = 2;
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
      } else if (datum_is_the_symbol(item, "req")) {
        inhibit_newline = 0;
        inhibit_child_newlines = 1;
      } else if (datum_is_the_symbol(item, "export")) {
        inhibit_newline = 0;
        inhibit_child_newlines = 1;
      }
      char *child_sep = "\n";
      if (datum_is_list(item) && list_length(item) > 0 &&
          !datum_is_the_symbol(list_at(item, 0), "brackets")) {
        child_sep = " ";
      }
      int child_flatness = flat;
      if (flat == FLAT_CHILDREN) {
        child_flatness = FLAT;
      }
      if (inhibit_newline >= 0) {
        child_flatness = FLAT;
      }
      if (inhibit_child_newlines >= 0) {
        child_flatness = child_flatness == FLAT ? FLAT : FLAT_CHILDREN;
      }
      end += sprintf(end, "%s",
                     datum_format_bounded(item, depth - 1, start + 1, pretty,
                                          child_flatness, child_sep));
      --inhibit_newline;
      --inhibit_double_newline;
      --inhibit_child_newlines;
    }
    if (strlen(pair) > 0) {
      end += sprintf(end, "%c", pair[1]);
    }
  } else if (pretty && datum_is_the_symbol(e, "empty-symbol")) {
  } else if (datum_is_symbol(e)) {
    end += sprintf(end, "%s", e->symbol_value);
  } else if (datum_is_bytestring(e)) {
    end += sprintf(end, "\"%s\"", escape_string(e->bytestring_value));
  } else if (datum_is_frame(e)) {
    sprintf(buf, "<some frame>");
  } else {
    sprintf(buf, "<fmt not implemented>");
  }
  return buf;
}

EXPORT bool fdatum_is_panic(fdatum result) {
  return result.type == FDATUM_PANIC;
}

EXPORT fdatum fdatum_make_ok(datum v) {
  fdatum result = {.type = FDATUM_OK, .ok_value = v};
  return result;
}

EXPORT fdatum fdatum_make_panic(char *message) {
  fdatum result = {.type = FDATUM_PANIC, .panic_message = strdup(message)};
  return result;
}

fdatum fdatum_get_value(datum *args) { // used in lisp
  datum *arg = list_at(args, 0);
  if (!datum_is_integer(arg)) {
    return fdatum_make_panic("fdatum_get_value expected a pointer");
  }
  fdatum *val = (fdatum *)arg->integer_value;
  if (fdatum_is_panic(*val)) {
    return *val;
  }
  return fdatum_make_ok(
      datum_make_list_of(datum_make_int((int64_t)&val->ok_value)));
}

fdatum fdatum_repr_datum_pointer(datum *args) { // used in lisp
  datum *arg = list_at(args, 0);
  if (!datum_is_integer(arg)) {
    return fdatum_make_panic("fdatum_get_value expected a pointer");
  }
  datum *val = (datum *)arg->integer_value;
  char *res = datum_repr(val);
  return fdatum_make_ok(datum_make_list_of(datum_make_bytestring(res)));
}

fdatum fdatum_get_panic_message(datum *args) { // used in lisp
  datum *arg = list_at(args, 0);
  if (!datum_is_integer(arg)) {
    return fdatum_make_panic("fdatum_get_panic_message expected a pointer");
  }
  fdatum val = *(fdatum *)arg->integer_value;
  if (!fdatum_is_panic(val)) {
    return fdatum_make_panic("fdatum_get_panic_message expected a panic");
  }
  return fdatum_make_ok(
      datum_make_list_of(datum_make_bytestring(val.panic_message)));
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

EXPORT vec vec_make(size_t capacity) {
  vec res;
  res.begin = malloc(capacity * sizeof(datum));
  res.length = 0;
  res.capacity = capacity;
  return res;
}

EXPORT datum *vec_append(vec *s, datum x) {
  if (s->length == s->capacity) {
    size_t new_capacity = (s->capacity + 1) * 2;
    datum *new_begin = malloc(sizeof(datum) * new_capacity);
    for (size_t i = 0; i < s->length; ++i) {
      new_begin[i] = s->begin[i];
    }
    s->capacity = new_capacity;
    // free(s->begin);
    s->begin = new_begin;
  }
  size_t res = s->length++;
  (s->begin)[res] = x;
  return s->begin + res;
}

EXPORT vec vec_make_of(size_t count, ...) {
  vec e = vec_make(count);
  va_list args;
  va_start(args, count);
  for (size_t i = 0; i < count; ++i) {
    datum elem = va_arg(args, datum);
    vec_append(&e, elem);
  }
  va_end(args);
  return e;
}

EXPORT datum *vec_at(vec *s, size_t index) {
  if (index >= s->length) {
    fprintf(stderr, "prog slice index overflow\n");
    assert(false);
  }
  return s->begin + index;
}

EXPORT size_t vec_length(vec *s) { return s->length; }

EXPORT datum vec_to_datum(vec *v) {
  datum res = datum_make_nil();
  for (size_t i = 0; i < vec_length(v); ++i) {
    list_append(&res, datum_copy(vec_at(v, i)));
  }
  return res;
}

EXPORT datum vec_to_bracket_datum(vec *v) {
  datum res = datum_make_list_of(datum_make_symbol("brackets"));
  for (size_t i = 0; i < vec_length(v); ++i) {
    list_append(&res, datum_to_bracket_datum(vec_at(v, i)));
  }
  return res;
}

LOCAL datum datum_to_bracket_datum(datum *v) {
  datum res = datum_make_list_of(datum_make_symbol("brackets"));
  for (int i = 0; i < list_length(v); ++i) {
    list_append(&res, datum_copy(list_at(v, i)));
  }
  return res;
}

EXPORT datum vec_pop(vec *v) {
  size_t len = vec_length(v);
  assert(len > 0);
  datum *res = vec_at(v, len - 1);
  --v->length;
  return *res;
}

EXPORT datum datum_make_nil() {
  datum e;
  e.type = DATUM_LIST;
  e.list_value = vec_make(0);
  return e;
}

EXPORT bool datum_is_list(datum *e) { return e->type == DATUM_LIST; }

EXPORT bool datum_is_nil(datum *e) {
  return datum_is_list(e) && list_length(e) == 0;
}

EXPORT datum datum_make_list_of_impl(size_t count, datum *values) {
  datum e = datum_make_nil();
  for (size_t i = 0; i < count; ++i) {
    datum elem = values[i];
    list_append(&e, elem);
  }
  return e;
}

EXPORT int list_length(datum *seq) {
  assert(datum_is_list(seq));
  return vec_length(&seq->list_value);
}

EXPORT datum *list_at(datum *list, unsigned index) {
  assert(datum_is_list(list));
  return vec_at(&list->list_value, index);
}

EXPORT datum list_copy(datum *list, int from, int to) {
  datum res = datum_make_nil();
  for (int i = from; i < to; ++i) {
    list_append(&res, datum_copy(list_at(list, i)));
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
  datum e = datum_make_nil();
  for (int i = 1; i < list_length(list); ++i) {
    list_append(&e, datum_copy(list_at(list, i)));
  }
  return e;
}

EXPORT void list_append(datum *list, datum value) {
  vec_append(&list->list_value, value);
}

EXPORT datum list_pop(datum *list) {
  assert(datum_is_list(list));
  return vec_pop(&list->list_value);
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
  if (datum_is_frame(d)) {
    frame f = frame_copy(&d->frame_value);
    datum res = datum_make_frame(f);
    return res;
  }
  if (datum_is_list(d)) {
    datum e = datum_make_nil();
    for (int i = 0; i < list_length(d); ++i) {
      datum item = datum_copy(list_at(d, i));
      list_append(&e, item);
    }
    return e;
  }
  return *d;
}

LOCAL frame frame_copy(frame *src) {
  frame dst;
  dst.state = vec_copy(&src->state);
  dst.type_id = src->type_id;
  dst.parent_type_id = src->parent_type_id;
  return dst;
}

EXPORT vec vec_copy(vec *src) {
  vec dst = vec_make(src->capacity);
  for (size_t i = 0; i < src->length; ++i) {
    vec_append(&dst, datum_copy(vec_at(src, i)));
  }
  return dst;
}

EXPORT vec *list_to_vec(datum *val) {
  assert(datum_is_list(val));
  return &val->list_value;
}

EXPORT vec brackets_to_vec(datum *val) {
  assert(datum_is_list(val));
  assert(list_length(val) > 0);
  assert(datum_is_the_symbol(list_at(val, 0), "brackets"));
  vec res = vec_make(list_length(val));
  for (int i = 1; i < list_length(val); ++i) {
    vec_append(&res, brackets_to_list(list_at(val, i)));
  }
  return res;
}

LOCAL datum brackets_to_list(datum *val) {
  assert(datum_is_list(val));
  assert(list_length(val) > 0);
  assert(datum_is_the_symbol(list_at(val, 0), "brackets"));
  datum res = datum_make_nil();
  for (int i = 1; i < list_length(val); ++i) {
    list_append(&res, datum_copy(list_at(val, i)));
  }
  return res;
}
