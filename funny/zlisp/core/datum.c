// zlisp interpreter.
#include <assert.h>
#include <extern.h>
#if INTERFACE
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>

#endif
#include <stdlib.h>
#include <string.h>

#if EXPORT_INTERFACE
#define datum_make_list_of(...) datum_make_list_of_impl(sizeof((datum []){__VA_ARGS__}) / sizeof(datum), (datum[]){__VA_ARGS__})
#endif

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

EXPORT datum *datum_make_symbol(char *name) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_SYMBOL;
  size_t length = strlen(name);
  e->symbol_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->symbol_value[i] = name[i];
  }
  return e;
}

EXPORT datum *datum_make_bytestring(char *text) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_BYTESTRING;
  size_t length = strlen(text);
  e->bytestring_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->bytestring_value[i] = text[i];
  }
  return e;
}

EXPORT datum *datum_make_int(int64_t value) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_INTEGER;
  e->integer_value = value;
  return e;
}

EXPORT datum *datum_make_frame(frame fr) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_FRAME;
  e->frame_value = fr;
  return e;
}

EXPORT char *datum_repr(datum *e) { return datum_repr_bounded(e, 8); }

EXPORT char *datum_repr_bounded(datum *e, size_t depth) {
  char *buf = malloc(1024 * sizeof(char));
  char *end = buf;
  if (depth == 0) {
    sprintf(buf, "<truncated>");
  } else if (datum_is_integer(e)) {
    sprintf(buf, "%" PRId64, e->integer_value);
  } else if (datum_is_list(e)) {
    end += sprintf(end, "(");
    for (int i = 0; i < list_length(e); ++i) {
      datum *item = list_at(e, i);
      end +=
          sprintf(end, "%s ", datum_repr_bounded(item, depth - 1));
    }
    end += sprintf(end, ")");
  } else if (datum_is_symbol(e)) {
    end += sprintf(end, "%s", e->symbol_value);
  } else if (datum_is_bytestring(e)) {
    end += sprintf(end, "\"%s\"", e->bytestring_value);
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
      datum_make_list_of(*datum_make_int((int64_t)&val->ok_value)));
}

fdatum fdatum_repr_datum_pointer(datum *args) { // used in lisp
  datum *arg = list_at(args, 0);
  if (!datum_is_integer(arg)) {
    return fdatum_make_panic("fdatum_get_value expected a pointer");
  }
  datum *val = (datum *)arg->integer_value;
  char *res = datum_repr(val);
  return fdatum_make_ok(datum_make_list_of(*datum_make_bytestring(res)));
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
      datum_make_list_of(*datum_make_bytestring(val.panic_message)));
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

EXPORT size_t vec_append(vec *s, datum x) {
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
  return res;
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

EXPORT size_t vec_append_new(vec *s) {
  return vec_append(s, datum_make_list_of(*datum_make_symbol(":end")));
}

EXPORT void vec_extend(vec *s, datum *instructions) {
  for (int i = 0; i < list_length(instructions); ++i) {
    datum *ins = list_at(instructions, i);
    size_t index = vec_append_new(s);
    *vec_at(s, index) = *ins;
  }
}

EXPORT datum *vec_at(vec *s, size_t index) {
  if (index >= s->length) {
    fprintf(stderr, "prog slice index overflow\n");
    exit(EXIT_FAILURE);
  }
  return s->begin + index;
}

EXPORT size_t vec_length(vec *s) { return s->length; }

EXPORT datum vec_to_datum(vec *sl) {
  datum res = datum_make_nil();
  for (size_t i = 0; i < vec_length(sl); ++i) {
    list_append(&res, vec_at(sl, i));
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

EXPORT bool datum_is_list(datum *e) {
  return e->type == DATUM_LIST;
}

EXPORT bool datum_is_nil(datum *e) {
  return datum_is_list(e) && list_length(e) == 0;
}

EXPORT datum datum_make_list_of_impl(size_t count, datum *values) {
  datum e = datum_make_nil();
  for (size_t i = 0; i < count; ++i) {
    datum *elem = values + i;
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
    list_append(&e, list_at(list, i));
  }
  return e;
}

EXPORT void list_append(datum *list, datum *value) {
  vec_append(&list->list_value, *value);
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
    frame f;
    frame_copy(&f, &d->frame_value);
    datum res = *datum_make_frame(f);
    return res;
  }
  if (datum_is_list(d)) {
    datum e = datum_make_nil();
    for (int i = 0; i < list_length(d); ++i) {
      datum item = datum_copy(list_at(d, i));
      list_append(&e, &item);
    }
    return e;
  }
  return *d;
}

EXPORT void frame_copy(frame *dst, frame *src) {
  vec_copy(&dst->state, &src->state);
  dst->type_id = src->type_id;
  dst->parent_type_id = src->parent_type_id;
}

EXPORT void vec_copy(vec *dst, vec *src) {
  dst->capacity = src->capacity;
  dst->length = src->length;
  dst->begin = malloc(src->capacity * sizeof(datum));
  for (size_t i = 0; i < dst->length; ++i) {
    dst->begin[i] = datum_copy(vec_at(src, i));
  }
}
