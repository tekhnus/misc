#include <zlisp-impl/zlisp-impl.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <dlfcn.h>

prog *compile_prog(datum *source);

LOCAL fdatum builtin_eq(datum *x, datum *y) {
  datum *t = datum_make_list_1(datum_make_nil());
  datum *f = datum_make_nil();
  if (datum_eq(x, y)) {
    return fdatum_make_ok(t);
  }
  return fdatum_make_ok(f);
}

LOCAL fdatum builtin_annotate(datum *arg_value) {
  char *type;
  if (datum_is_list(arg_value)) {
    type = ":list";
  } else if (datum_is_symbol(arg_value)) {
    type = ":symbol";
  } else if (datum_is_bytestring(arg_value)) {
    type = ":bytestring";
  } else if (datum_is_integer(arg_value)) {
    type = ":integer";
  } else if (datum_is_routine(arg_value)) {
    type = ":operator";
  } else if (datum_is_pointer(arg_value)) {
    type = ":pointer";
  } else {
    return fdatum_make_panic("incomplete implementation of type");
  }
  return fdatum_make_ok(datum_make_list_2(datum_make_symbol(type), arg_value));
}

LOCAL fdatum builtin_is_constant(datum *arg_value) {
  if (datum_is_constant(arg_value)) {
    return fdatum_make_ok(datum_make_list_1(datum_make_nil()));
  }
  return fdatum_make_ok(datum_make_nil());
}

LOCAL fdatum builtin_panic(datum *arg_value) {
  if (!datum_is_bytestring(arg_value)) {
    return fdatum_make_panic("panic expects a bytestring");
  }
  return fdatum_make_panic(arg_value->bytestring_value);
}

LOCAL fdatum builtin_shared_library(datum *library_name) {
  if (!datum_is_bytestring(library_name)) {
    return fdatum_make_panic("load-shared-library expects a bytestring");
  }
  void **handle = malloc(sizeof(void *));
  *handle = dlopen(library_name->bytestring_value, RTLD_LAZY);
  char *err = dlerror();
  if (!*handle) {
    return fdatum_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                            datum_make_bytestring(err)));
  }
  return fdatum_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"), datum_make_pointer_to_pointer(handle)));
}

LOCAL fdatum builtin_extern_pointer(datum *shared_library, datum *name,
                              datum *descriptor) {
  if (!datum_is_pointer(shared_library) ||
      !datum_is_symbol(shared_library->pointer_descriptor) ||
      strcmp(shared_library->pointer_descriptor->symbol_value, "pointer")) {
    return fdatum_make_panic("wrong externcdata usage");
  }
  void *handle = *(void **)shared_library->pointer_value;
  if (!datum_is_bytestring(name)) {
    return fdatum_make_panic("externcdata expected a string");
  }
  void *call_ptr = dlsym(handle, name->bytestring_value);
  char *err = dlerror();
  if (err != NULL) {
    return fdatum_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                            datum_make_bytestring(err)));
  }
  return fdatum_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"), datum_make_pointer(call_ptr, descriptor)));
}

LOCAL fdatum builtin_repr(datum *v) {
  return fdatum_make_ok(datum_make_bytestring(datum_repr(v)));
}


LOCAL fdatum builtin_concat_bytestrings(datum *x, datum *y) {
  if (!datum_is_bytestring(x) || !datum_is_bytestring(y)) {
    return fdatum_make_panic("expected integers");
  }
  char *buf =
      malloc(strlen(x->bytestring_value) + strlen(y->bytestring_value) + 1);
  buf[0] = '\0';
  strcat(buf, x->bytestring_value);
  strcat(buf, y->bytestring_value);
  return fdatum_make_ok(datum_make_bytestring(buf));
}

LOCAL fdatum builtin_add(datum *x, datum *y) {
  if (!datum_is_integer(x) || !datum_is_integer(y)) {
    return fdatum_make_panic("expected integers");
  }
  return fdatum_make_ok(datum_make_int(x->integer_value + y->integer_value));
}

LOCAL fdatum builtin_cons(datum *head, datum *tail) {
  if (!datum_is_list(tail)) {
    return fdatum_make_panic("cons requires a list as a second argument");
  }
  return fdatum_make_ok(datum_make_list(head, tail));
}

LOCAL fdatum builtin_head(datum *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("car expects a nonempty list");
  }
  return fdatum_make_ok(list->list_head);
}

LOCAL fdatum builtin_tail(datum *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("cdr expects a nonempty list");
  }
  return fdatum_make_ok(list->list_tail);
}

LOCAL void namespace_def_extern_fn(state **ctxt, char *name, fdatum (*fn)(),
                             int cnt) {
  datum *sig = datum_make_nil();
  for (int i = 0; i < cnt; ++i) {
    sig = datum_make_list(datum_make_symbol("datum"), sig);
  }
  datum *wrapped_fn =
      datum_make_pointer(__extension__(void *) fn,
                         datum_make_list_2(sig, datum_make_symbol("val")));
  *ctxt = state_set_var(*ctxt, datum_make_symbol(name), wrapped_fn);
}


state *state_make_builtins() {
  state *ns = state_make_fresh();

  namespace_def_extern_fn(&ns, "panic--", builtin_panic, 1);
  namespace_def_extern_fn(&ns, "shared-library--", builtin_shared_library, 1);
  namespace_def_extern_fn(&ns, "extern-pointer--", builtin_extern_pointer, 3);
  namespace_def_extern_fn(&ns, "cons--", builtin_cons, 2);
  namespace_def_extern_fn(&ns, "head--", builtin_head, 1);
  namespace_def_extern_fn(&ns, "tail--", builtin_tail, 1);
  namespace_def_extern_fn(&ns, "eq--", builtin_eq, 2);
  namespace_def_extern_fn(&ns, "annotate--", builtin_annotate, 1);
  namespace_def_extern_fn(&ns, "is-constant--", builtin_is_constant, 1);
  namespace_def_extern_fn(&ns, "repr--", builtin_repr, 1);
  namespace_def_extern_fn(&ns, "concat-bytestrings--",
                          builtin_concat_bytestrings, 2);
  namespace_def_extern_fn(&ns, "+--", builtin_add, 2);

  char *prelude_src =
      "(builtin.defn panic (return (pointer-call panic-- args)))"
      "(builtin.defn shared-library (return (pointer-call shared-library-- "
      "args)))"
      "(builtin.defn extern-pointer (return (pointer-call extern-pointer-- "
      "args)))"
      "(builtin.defn cons (return (pointer-call cons-- args)))"
      "(builtin.defn head (return (pointer-call head-- args)))"
      "(builtin.defn tail (return (pointer-call tail-- args)))"
      "(builtin.defn eq (return (pointer-call eq-- args)))"
      "(builtin.defn annotate (return (pointer-call annotate-- args)))"
      "(builtin.defn is-constant (return (pointer-call is-constant-- args)))"
      "(builtin.defn repr (return (pointer-call repr-- args)))"
      "(builtin.defn concat-bytestrings (return (pointer-call "
      "concat-bytestrings-- args)))"
      "(builtin.defn + (return (pointer-call +-- args)))";
  FILE *prelude_f = fmemopen(prelude_src, strlen(prelude_src), "r");
  fdatum prelude_d = datum_read_all(prelude_f);
  if (fdatum_is_panic(prelude_d)) {
    fprintf(stderr, "prelude syntax: %s", prelude_d.panic_message);
    exit(EXIT_FAILURE);
  }
  prog *prelude_p = compile_prog(prelude_d.ok_value);
  if (prelude_p == NULL) {
      fprintf(stderr, "prelude compilation failure");
      exit(EXIT_FAILURE);
  }
  fdatum err = routine_run_and_get_value(&ns, prelude_p);
  if (fdatum_is_panic(err)) {
    fprintf(stderr, "prelude evaluation failure: %s", err.panic_message);
    exit(EXIT_FAILURE);
  }
  return ns;
}
