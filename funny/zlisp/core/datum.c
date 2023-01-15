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

enum fdatumype {
  FDATUM_OK,
  FDATUM_PANIC,
};

EXPORT bool datum_is_nil(datum *e) { return e->type == DATUM_NIL; }

EXPORT bool datum_is_list(datum *e) {
  return e->type == DATUM_NIL || e->type == DATUM_LIST;
}

EXPORT bool datum_is_symbol(datum *e) { return e->type == DATUM_SYMBOL; }

EXPORT bool datum_is_integer(datum *e) { return e->type == DATUM_INTEGER; }

EXPORT bool datum_is_bytestring(datum *e) {
  return e->type == DATUM_BYTESTRING;
}

EXPORT bool datum_is_frame(datum *e) { return e->type == DATUM_FRAME; }

EXPORT datum *datum_make_nil() {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_NIL;
  return e;
}

EXPORT datum *datum_make_list(datum *head, datum *tail) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_LIST;
  e->list_head = head;
  e->list_tail = tail;
  return e;
}

EXPORT datum *datum_make_list_of(size_t count, ...) {
  datum *res = datum_make_nil();
  va_list args;
  va_start(args, count);
  for (size_t i = 0; i < count; ++i) {
    datum *elem = va_arg(args, datum *);
    res = list_append(res, elem);
  }
  va_end(args);
  return res;
}

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

EXPORT fdatum fdatum_make_ok(datum *v) {
  fdatum result = {.type = FDATUM_OK, .ok_value = v};
  return result;
}

EXPORT fdatum fdatum_make_panic(char *message) {
  fdatum result = {.type = FDATUM_PANIC, .panic_message = message};
  return result;
}

fdatum fdatum_get_value(datum *args) { // used in lisp
  datum *arg = list_at(args, 0);
  if (!datum_is_integer(arg)) {
    return fdatum_make_panic("fdatum_get_value expected a pointer");
  }
  fdatum val = *(fdatum *)arg->integer_value;
  if (fdatum_is_panic(val)) {
    return val;
  }
  return fdatum_make_ok(
      datum_make_list_of(1, datum_make_int((int64_t)val.ok_value)));
}

fdatum fdatum_repr_datum_pointer(datum *args) { // used in lisp
  datum *arg = list_at(args, 0);
  if (!datum_is_integer(arg)) {
    return fdatum_make_panic("fdatum_get_value expected a pointer");
  }
  datum *val = (datum *)arg->integer_value;
  char *res = datum_repr(val);
  return fdatum_make_ok(datum_make_list_of(1, datum_make_bytestring(res)));
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
      datum_make_list_of(1, datum_make_bytestring(val.panic_message)));
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

EXPORT prog_slice prog_slice_make(size_t capacity) {
  prog_slice res;
  res.begin = malloc(capacity * sizeof(datum));
  res.length = 0;
  res.capacity = capacity;
  return res;
}

EXPORT size_t prog_slice_append_new(prog_slice *s) {
  if (s->length == s->capacity) {
    fprintf(stderr, "prog slice capacity overflow %zu\n", s->capacity);
    exit(EXIT_FAILURE);
  }
  size_t res = s->length++;
  datum *p = s->begin + res;
  *p = *datum_make_list_of(1, datum_make_symbol(":end"));
  return res;
}

EXPORT void prog_slice_extend(prog_slice *s, datum *instructions) {
  for (int i = 0; i < list_length(instructions); ++i) {
    datum *ins = list_at(instructions, i);
    size_t index = prog_slice_append_new(s);
    *prog_slice_datum_at(*s, index) = *ins;
  }
}

EXPORT datum *prog_slice_datum_at(prog_slice s, size_t index) {
  if (index >= s.length) {
    fprintf(stderr, "prog slice index overflow\n");
    exit(EXIT_FAILURE);
  }
  return s.begin + index;
}

EXPORT size_t prog_slice_length(prog_slice s) { return s.length; }

EXPORT datum *prog_slice_to_datum(prog_slice sl) {
  datum *res = datum_make_nil();
  for (size_t i = 0; i < prog_slice_length(sl); ++i) {
    res = list_append(res, prog_slice_datum_at(sl, i));
  }
  return res;
}

EXPORT int list_length(datum *seq) {
  if (!datum_is_list(seq)) {
    fprintf(stderr, "not a list\n");
    exit(EXIT_FAILURE);
  }
  int res;
  for (res = 0; !datum_is_nil(seq); seq = seq->list_tail, ++res) {
  }
  return res;
}

EXPORT datum *list_at(datum *list, unsigned index) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    fprintf(stderr, "list_at panic\n");
    exit(EXIT_FAILURE);
  }
  if (index == 0) {
    return list->list_head;
  }
  return list_at(list->list_tail, index - 1);
}

EXPORT datum *list_get_last(datum *list) {
  assert(list_length(list) > 0);
  return list_at(list, list_length(list) - 1);
}

EXPORT datum *list_get_tail(datum *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    fprintf(stderr, "list_tail panic\n");
    exit(EXIT_FAILURE);
  }
  return list->list_tail;
}

EXPORT datum *list_append(datum *list, datum *value) {
  if (datum_is_nil(list)) {
    return datum_make_list(value, datum_make_nil());
  }
  return datum_make_list(list->list_head, list_append(list->list_tail, value));
}

EXPORT datum *list_chop_last(datum *list) {
  if (datum_is_nil(list)) {
    fprintf(stderr, "list_chop_last error\n");
    exit(EXIT_FAILURE);
  }
  if (list_length(list) == 1) {
    return datum_make_nil();
  }
  return datum_make_list(list->list_head, list_chop_last(list->list_tail));
}

EXPORT int list_index_of(datum *xs, datum *x) {
  int i = list_length(xs) - 1;
  for (; i >= 0; --i) {
    if (datum_eq(list_at(xs, i), x)) {
      return i;
    }
  }
  return -1;
}
