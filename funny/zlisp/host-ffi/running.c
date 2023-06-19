#include <running.h>
#if INTERFACE
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <zlisp/common.h>
#endif

EXPORT fdatum routine_run_in_ffi_host(vec sl, datum *r0d) {
  // This one is for lisp.
  result r = host_ffi_run(sl, r0d, datum_make_nil());
  if (datum_is_the_symbol(&r.type, "halt")) {
    return fdatum_make_ok(r.value);
  }
  return fdatum_make_panic(datum_repr(&r.value));
}

EXPORT result host_ffi_run(vec sl, datum *r0d, datum args) {
  context ctxt = {};
  result res;
  datum current_statement = datum_make_nil();
  for (;;) {
    res = routine_run(sl, r0d, args, &ctxt);
    if (ctxt.aborted) {
      res = (result){datum_make_symbol("interpreter-panic"), datum_make_symbol("some panic")};
      break;
    }
    datum *sec = &res.value;
    datum *yield_type = &res.type;
    fdatum handler_res = host_ffi(yield_type, sec);
    if (fdatum_is_panic(handler_res) &&
        strcmp(handler_res.panic_message, "<not implemented>")) {
      res = (result){datum_make_symbol("panic"),
                     datum_make_bytestring(handler_res.panic_message)};
      break;
    }
    if (!fdatum_is_panic(handler_res)) {
      args = handler_res.ok_value;
      continue;
    }
    if (datum_is_list(yield_type) && list_length(yield_type) == 3 &&
        datum_is_the_symbol(list_at(yield_type, 0), "debugger")) {
      datum *cmd = list_at(yield_type, 1);
      if (datum_is_the_symbol(cmd, "statement")) {
        current_statement = *list_at(yield_type, 2);
      } else {
        fprintf(stderr, "unknown debugger cmd\n");
        exit(EXIT_FAILURE);
      }
      args = datum_make_nil();
      continue;
    }
    break;
  }
  if (datum_is_the_symbol(&res.type, "panic")) {
    fprintf(stderr, "CURRENT STATEMENT: %s\n", datum_repr(&current_statement));
    print_backtrace(sl, r0d);
  }
  if (datum_is_the_symbol(&res.type, "interpreter-panic")) {
    fprintf(stderr, "CURRENT STATEMENT: %s\n", datum_repr(&current_statement));
    print_backtrace(sl, r0d);
  }
  return res;
}

LOCAL fdatum host_ffi(datum *type, datum *args) {
  if (!datum_is_list(type) || list_length(type) != 2 ||
      !datum_is_the_symbol(list_at(type, 0), "host")) {
    return fdatum_make_panic("<not implemented>");
  }
  datum *name = list_at(type, 1);
  if (!datum_is_bytestring(name)) {
    return fdatum_make_panic("host instruction should be a string");
  }
  datum res;
  if (!strcmp(name->bytestring_value, "call-extension")) {
    if (!datum_is_list(args) || list_length(args) == 0) {
      return fdatum_make_panic("call-extension expected at least a single arg");
    }
    datum *fn = list_at(args, 0);
    datum callargs = list_get_tail(args);
    if (!datum_is_integer(fn)) {
      return fdatum_make_panic("call-extension expected a pointer to function");
    }
    fdatum (*fnptr)(datum *) = (fdatum(*)(datum *))fn->integer_value;
    fdatum results = fnptr(&callargs);
    return results;
  } else if (!strcmp(name->bytestring_value, "deref-pointer")) {
    res = datum_make_int((int64_t)datum_deref);
  } else if (!strcmp(name->bytestring_value, "mkptr-pointer")) {
    res = datum_make_int((int64_t)datum_mkptr);
  } else if (!strcmp(name->bytestring_value, "pointer-call-pointer")) {
    res = datum_make_int((int64_t)pointer_call);
  } else if (!strcmp(name->bytestring_value, "head")) {
    res = datum_make_int((int64_t)builtin_head);
  } else if (!strcmp(name->bytestring_value, "tail")) {
    res = datum_make_int((int64_t)builtin_tail);
  } else if (!strcmp(name->bytestring_value, "cons")) {
    res = datum_make_int((int64_t)builtin_cons);
  } else if (!strcmp(name->bytestring_value, "eq")) {
    res = datum_make_int((int64_t)builtin_eq);
  } else if (!strcmp(name->bytestring_value, "dlopen")) {
    // TODO(): repair its usage in lisp.
    // dlopen actually has an int argument, not a size_t.
    res = datum_make_int((int64_t)dlopen);
  } else if (!strcmp(name->bytestring_value, "dlsym")) {
    res = datum_make_int((int64_t)dlsym);
  } else if (!strcmp(name->bytestring_value, "RTLD_LAZY")) {
    res = datum_make_int(RTLD_LAZY);
  } else {
    return fdatum_make_panic("unknown host instruction");
  }
  return fdatum_make_ok(datum_make_list_of(res));
}

ffi_type ffi_type_fdatum;
ffi_type *ffi_type_fdatum_elements[4];
ffi_type ffi_type_vec;
ffi_type *ffi_type_vec_elements[4];

LOCAL void init_standard_types() {
  // TODO: UPDATE THE FFI DEFINITIONS, THEY ARE OUTDATED.

  ffi_type *type = &ffi_type_fdatum;
  (type)->type = FFI_TYPE_STRUCT;
  (type)->size = 0; // Lost 5 hours debugging non-deterministic failures on
                    // Mac before adding this line.
  (type)->alignment = 0;
  ffi_type **elements = ffi_type_fdatum_elements;
  elements[0] = &ffi_type_sint;
  elements[1] = &ffi_type_pointer; // not a pointer!
  elements[2] = &ffi_type_pointer;
  elements[3] = NULL;
  (type)->elements = elements;

  type = &ffi_type_vec;
  (type)->type = FFI_TYPE_STRUCT;
  (type)->size = 0;
  (type)->alignment = 0;
  elements = ffi_type_vec_elements;
  elements[0] = &ffi_type_pointer;
  elements[1] = &ffi_type_uint64; // it's actually size_t, danger!
  elements[2] = &ffi_type_uint64; // it's size_t too!
  elements[3] = NULL;
  (type)->elements = elements;
}

LOCAL bool ffi_type_init(ffi_type **type, datum *definition) {
  if (!datum_is_symbol(definition)) {
    return false;
  }
  if (!strcmp(definition->symbol_value, "string")) {
    *type = &ffi_type_pointer;
    return true;
  }
  if (!strcmp(definition->symbol_value, "pointer")) {
    *type = &ffi_type_pointer;
    return true;
  }
  if (!strcmp(definition->symbol_value, "sizet")) {
    *type = &ffi_type_uint64; // danger!
    return true;
  }
  if (!strcmp(definition->symbol_value, "int")) {
    *type = &ffi_type_sint;
    return true;
  }
  if (!strcmp(definition->symbol_value, "val") ||
      !strcmp(definition->symbol_value, "fdatum")) {
    init_standard_types();
    *type = &ffi_type_fdatum;
    return true;
  }
  if (!strcmp(definition->symbol_value, "progslice")) {
    init_standard_types();
    *type = &ffi_type_vec;
    return true;
  }
  return false;
}

LOCAL char *pointer_ffi_init_cif(datum *sig, ffi_cif *cif, ffi_type **arg_types,
                                 ffi_type **ret_type) {
  if (list_length(sig) != 2) {
    return "the signature should be a two-item list";
  }
  datum *arg_defs = list_at(sig, 0);
  int arg_count;
  for (arg_count = 0; arg_count < list_length(arg_defs); ++arg_count) {
    if (!ffi_type_init(arg_types + arg_count, list_at(arg_defs, arg_count))) {
      return "something wrong with the argument type signature";
    }
  }
  if (!ffi_type_init(ret_type, list_at(sig, 1))) {
    return "something wrong with the return type signature";
  }
  ffi_status status;
  // TODO: for variadic functions, prep_cif_var must be used.
  // Without it, linux works somehow and mac does not.
  if ((status = ffi_prep_cif(cif, FFI_DEFAULT_ABI, arg_count, *ret_type,
                             arg_types)) != FFI_OK) {
    return "something went wrong during ffi_prep_cif";
  }
  return NULL;
}

LOCAL char *pointer_ffi_serialize_args(datum *args, void **cargs, int nargs) {
  if (list_length(args) != nargs) {
    return "incorrect number of args for FFI call";
  }
  int arg_cnt = 0;
  for (arg_cnt = 0; arg_cnt < nargs; ++arg_cnt) {
    datum *a = list_at(args, arg_cnt);

    if (!datum_is_integer(a)) {
      // fprintf(stderr, "!!! %s\n", datum_repr(a));
      return "int pointer expected, got something else";
    }
    cargs[arg_cnt] = (void *)a->integer_value;
  }
  return NULL;
}

LOCAL fdatum datum_mkptr(datum *args) {
  datum *form = args;
  if (!datum_is_list(form) || list_length(form) != 2) {
    return fdatum_make_panic("mkptr expected a pair on stack");
  }
  datum *d = list_at(form, 0);
  datum *desc = list_at(form, 1);
  if (!datum_is_symbol(desc)) {
    return fdatum_make_panic("mkptr expected a symbol");
  }
  char *des = desc->symbol_value;
  if (!strcmp(des, "string")) {
    if (!datum_is_bytestring(d)) {
      return fdatum_make_panic("string expected, got something else");
    }
    return fdatum_make_ok(
        datum_make_list_of(datum_make_int((int64_t) & (d->bytestring_value))));
  } else if (!strcmp(des, "sizet")) {
    if (!datum_is_integer(d)) {
      return fdatum_make_panic("int expected, got something else");
    }
    return fdatum_make_ok(
        datum_make_list_of(datum_make_int((int64_t) & (d->integer_value))));
  } else {
    return fdatum_make_panic("cannot load an argument");
  }
}

LOCAL fdatum datum_deref(datum *args) {
  datum *form = args;
  if (!datum_is_list(form) || list_length(form) != 2) {
    return fdatum_make_panic("deref expected a pair on stack");
  }
  datum *what = list_at(form, 0);
  datum *how = list_at(form, 1);
  if (!datum_is_integer(what)) {
    return fdatum_make_panic("deref expected a pointer");
  }
  if (!datum_is_symbol(how)) {
    return fdatum_make_panic("deref expected a symbol");
  }
  char *rettype = how->symbol_value;
  void *wha = (void *)what->integer_value;
  if (!strcmp(rettype, "sizet")) {
    return fdatum_make_ok(
        datum_make_list_of(datum_make_int((int64_t) * (size_t *)wha)));
  } else if (!strcmp(rettype, "int")) {
    return fdatum_make_ok(
        datum_make_list_of(datum_make_int((int64_t) * (int *)wha)));
  } else if (!strcmp(rettype, "int64")) {
    return fdatum_make_ok(datum_make_list_of(datum_make_int(*(int64_t *)wha)));
  } else if (!strcmp(rettype, "string")) {
    return fdatum_make_ok(
        datum_make_list_of(datum_make_bytestring(*(char **)wha)));
  } else {
    return fdatum_make_panic("unknown return type for deref");
  }
}

LOCAL void *allocate_space_for_return_value(datum *sig) {
  char *rettype = list_at(sig, 1)->symbol_value;
  void *res;
  if (!strcmp(rettype, "pointer")) {
    res = malloc(sizeof(void *));
  } else if (!strcmp(rettype, "sizet")) {
    res = malloc(sizeof(size_t));
  } else if (!strcmp(rettype, "int")) {
    res = malloc(sizeof(int));
  } else if (!strcmp(rettype, "string")) {
    res = malloc(sizeof(char *));
  } else if (!strcmp(rettype, "fdatum")) {
    res = malloc(sizeof(fdatum));
  } else if (!strcmp(rettype, "progslice")) {
    res = malloc(sizeof(vec));
  } else if (!strcmp(rettype, "val")) {
    res = malloc(sizeof(fdatum));
  } else {
    res = NULL;
  }
  return res;
}

LOCAL fdatum pointer_call(datum *argz) {
  if (!datum_is_list(argz) || list_length(argz) != 3) {
    return fdatum_make_panic("pointer-call expected a triple on stack");
  }

  datum *fpt = list_at(argz, 0);
  datum *sig = list_at(argz, 1);
  datum *args = list_at(argz, 2);
  if (!datum_is_integer(fpt)) {
    return fdatum_make_panic("not a function pointer");
  }
  void (*fn_ptr)(void) = datum_to_function_pointer(fpt);
  ffi_cif cif;
  char *err = NULL;
  ffi_type *arg_types[32];
  ffi_type *ret_type;
  err = pointer_ffi_init_cif(sig, &cif, &arg_types[0], &ret_type);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  int nargs = list_length(list_at(sig, 0));
  void *cargs[32];
  err = pointer_ffi_serialize_args(args, cargs, nargs);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  void *res = allocate_space_for_return_value(sig);
  if (res == NULL) {
    return fdatum_make_panic("unknown return type for extern func");
  }
  ffi_call(&cif, fn_ptr, res, cargs);
  return fdatum_make_ok(datum_make_list_of(datum_make_int((int64_t)res)));
}

LOCAL void (*datum_to_function_pointer(datum *d))(void) {
  assert(datum_is_integer(d));
  return __extension__(void (*)(void)) d->integer_value;
}
