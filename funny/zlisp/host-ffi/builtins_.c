#include <builtins_.h>
#include <types.h>
#if INTERFACE
#include <assert.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
#endif

datum builtin_eq(datum *args, context *ctxt) {
  if (ctxt == ctxt + 1) {
  }
  datum *x = list_at(args, 0);
  datum *y = list_at(args, 1);
  datum nil = datum_make_nil();
  datum t = datum_make_list_of(nil);
  datum *f = &nil;
  if (datum_eq(x, y)) {
    return (datum_make_list_of(t));
  }
  return (datum_make_list_of(*f));
}

datum builtin_annotate(datum *args, context *ctxt) {
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
    abortf(ctxt, "incomplete implementation of type");
    return (datum){};
  }
  return (datum_make_list_of(
      datum_make_list_of(datum_make_symbol(type), datum_copy(arg_value))));
}

datum builtin_is_constant(datum *args, context *ctxt) {
  if (ctxt == ctxt + 1) {
  }
  datum nil = datum_make_nil();
  datum *arg_value = list_at(args, 0);
  if (datum_is_constant(arg_value)) {
    return (datum_make_list_of(datum_make_list_of(nil)));
  }
  return (datum_make_list_of(nil));
}

datum builtin_repr(datum *args, context *ctxt) {
  if (ctxt == ctxt + 1) {
  }
  datum *v = list_at(args, 0);
  return (datum_make_list_of(datum_make_bytestring(datum_repr(v))));
}

datum builtin_concat_bytestrings(datum *args, context *ctxt) {
  datum *x = list_at(args, 0);
  datum *y = list_at(args, 1);
  if (!datum_is_bytestring(x) || !datum_is_bytestring(y)) {
    abortf(ctxt, "expected integers");
    return (datum){};
  }
  char buf[1024 * 16];
  buf[0] = '\0';
  strcat(buf, datum_get_bytestring(x));
  strcat(buf, datum_get_bytestring(y));
  return (datum_make_list_of(datum_make_bytestring(buf)));
}

datum builtin_add(datum *args, context *ctxt) {
  datum *x = list_at(args, 0);
  datum *y = list_at(args, 1);
  if (!datum_is_integer(x) || !datum_is_integer(y)) {
    abortf(ctxt, "expected integers");
    return (datum){};
  }
  return (
      datum_make_list_of(datum_make_int(datum_get_integer(x) + datum_get_integer(y))));
}

datum builtin_cons(datum *args, context *ctxt) {
  datum *head = list_at(args, 0);
  datum *tail = list_at(args, 1);
  if (!datum_is_list(tail)) {
    abortf(ctxt, "cons requires a list as a second argument");
    return (datum){};
  }
  vec vc = vec_make_copies(list_length(tail) + 1, datum_make_nil());
  *vec_at(&vc, 0) = datum_copy(head);
  for (int i = 0; i < list_length(tail); ++i) {
    *vec_at(&vc, i + 1) = datum_copy(list_at(tail, i));
  }
  return (datum_make_list_of(datum_make_list_vec(vc)));
}

datum builtin_head(datum *args, context *ctxt) {
  datum *list = list_at(args, 0);
  if (!datum_is_list(list) || datum_is_nil(list)) {
    abortf(ctxt, "car expects a nonempty list");
    return (datum){};
  }
  return (datum_make_list_of(datum_copy(list_at(list, 0))));
}

datum builtin_tail(datum *args, context *ctxt) {
  datum *list = list_at(args, 0);
  if (!datum_is_list(list) || datum_is_nil(list)) {
    abortf(ctxt, "cdr expects a nonempty list");
    return (datum){};
  }
  datum tail = list_get_tail(list);
  return (datum_make_list_of(tail));
}

datum builtin_len(datum *args, context *ctxt) {
  assert(list_length(args) == 1);
  datum *arg = list_at(args, 0);
  if (datum_is_blob(arg)) {
    return datum_make_list_of(datum_make_int(datum_get_blob(arg)->length));
  }
  abortf(ctxt, "unsupported type for len");
  return (datum){};
}
