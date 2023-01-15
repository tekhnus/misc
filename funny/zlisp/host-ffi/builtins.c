#include <builtins.h>
#if INTERFACE
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
#endif

fdatum builtin_eq(datum *args) {
  datum *x = list_at(args, 0);
  datum *y = list_at(args, 1);
  datum *t = datum_make_list_of(1, datum_make_nil());
  datum *f = datum_make_nil();
  if (datum_eq(x, y)) {
    return fdatum_make_ok(datum_make_list_of(1, t));
  }
  return fdatum_make_ok(datum_make_list_of(1, f));
}

fdatum builtin_annotate(datum *args) {
  datum *arg_value = list_at(args, 0);
  char *type;
  if (datum_is_list(arg_value)) {
    type = ":list";
  } else if (datum_is_symbol(arg_value)) {
    type = ":symbol";
  } else if (datum_is_bytestring(arg_value)) {
    type = ":bytestring";
  } else if (datum_is_integer(arg_value)) {
    type = ":integer";
  } else {
    return fdatum_make_panic("incomplete implementation of type");
  }
  return fdatum_make_ok(
      datum_make_list_of(1, datum_make_list_of(2, datum_make_symbol(type), arg_value)));
}

fdatum builtin_is_constant(datum *args) {
  datum *arg_value = list_at(args, 0);
  if (datum_is_constant(arg_value)) {
    return fdatum_make_ok(
        datum_make_list_of(1, datum_make_list_of(1, datum_make_nil())));
  }
  return fdatum_make_ok(datum_make_list_of(1, datum_make_nil()));
}

fdatum builtin_panic(datum *args) {
  datum *arg_value = list_at(args, 0);
  return fdatum_make_panic(datum_repr(arg_value));
}

fdatum builtin_repr(datum *args) {
  datum *v = list_at(args, 0);
  return fdatum_make_ok(
      datum_make_list_of(1, datum_make_bytestring(datum_repr(v))));
}

fdatum builtin_concat_bytestrings(datum *args) {
  datum *x = list_at(args, 0);
  datum *y = list_at(args, 1);
  if (!datum_is_bytestring(x) || !datum_is_bytestring(y)) {
    return fdatum_make_panic("expected integers");
  }
  char *buf =
      malloc(strlen(x->bytestring_value) + strlen(y->bytestring_value) + 1);
  buf[0] = '\0';
  strcat(buf, x->bytestring_value);
  strcat(buf, y->bytestring_value);
  return fdatum_make_ok(datum_make_list_of(1, datum_make_bytestring(buf)));
}

fdatum builtin_add(datum *args) {
  datum *x = list_at(args, 0);
  datum *y = list_at(args, 1);
  if (!datum_is_integer(x) || !datum_is_integer(y)) {
    return fdatum_make_panic("expected integers");
  }
  return fdatum_make_ok(
      datum_make_list_of(1, datum_make_int(x->integer_value + y->integer_value)));
}

fdatum builtin_cons(datum *args) {
  datum *head = list_at(args, 0);
  datum *tail = list_at(args, 1);
  if (!datum_is_list(tail)) {
    return fdatum_make_panic("cons requires a list as a second argument");
  }
  return fdatum_make_ok(datum_make_list_of(1, datum_make_list(head, tail)));
}

fdatum builtin_head(datum *args) {
  datum *list = list_at(args, 0);
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("car expects a nonempty list");
  }
  return fdatum_make_ok(datum_make_list_of(1, list_at(list, 0)));
}

fdatum builtin_tail(datum *args) {
  datum *list = list_at(args, 0);
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("cdr expects a nonempty list");
  }
  return fdatum_make_ok(datum_make_list_of(1, list_get_tail(list)));
}
