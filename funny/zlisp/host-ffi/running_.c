#include <running_.h>
#if INTERFACE
#include <assert.h>
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
#endif

EXPORT datum *routine_run_in_ffi_host(vec *sl, datum *r0d, context *ctxt) {
  // This one is for lisp.
  result r = host_ffi_run(sl, r0d, datum_make_nil(), ctxt);
  if (ctxt->aborted) {
    return NULL;
  }
  datum *res = malloc(sizeof(datum));
  if (datum_is_the_symbol(&r.type, "halt")) {
    *res = (r.value);
  } else {
    abortf(ctxt, "panic while running");
    return NULL;
  }
  return res;
}

EXPORT result host_ffi_run(vec *sl, datum *r0d, datum args, context *ctxt) {
  result res;
  datum current_statement = datum_make_nil();
  for (;;) {
    res = routine_run(sl, r0d, args, ctxt);
    if (ctxt->aborted) {
      res = (result){datum_make_symbol("interpreter-panic"),
                     datum_make_bytestring(ctxt->error)};
      break;
    }
    datum *sec = &res.value;
    datum *yield_type = &res.type;
    if (datum_is_list(yield_type) && list_length(yield_type) == 2 &&
        datum_is_the_symbol(list_at(yield_type, 0), "host")) {
      datum handler_res = host_ffi(yield_type, sec, ctxt);
      if (ctxt->aborted) {
        res = (result){datum_make_symbol("panic"),
                       datum_make_bytestring(ctxt->error)};
        break;
      }
      args = handler_res;
      continue;
    }
    if (datum_is_list(yield_type) && list_length(yield_type) == 3 &&
        datum_is_the_symbol(list_at(yield_type, 0), "debugger")) {
      datum *cmd = list_at(yield_type, 1);
      if (datum_is_the_symbol(cmd, "statement")) {
        current_statement = *list_at(yield_type, 2);
      } else {
        abortf(ctxt, "unknown debugger cmd\n");
        return (result){};
      }
      args = datum_make_nil();
      continue;
    }
    break;
  }
  if (datum_is_the_symbol(&res.type, "panic")) {
    fprintf(stderr, "CURRENT STATEMENT: %s\n", datum_repr(&current_statement));
  }
  if (datum_is_the_symbol(&res.type, "interpreter-panic")) {
    fprintf(stderr, "CURRENT STATEMENT: %s\n", datum_repr(&current_statement));
  }
  return res;
}

LOCAL datum host_ffi(datum *type, datum *args, context *ctxt) {
  assert(datum_is_list(type) || list_length(type) == 2 ||
         datum_is_the_symbol(list_at(type, 0), "host"));
  datum *name = list_at(type, 1);
  if (!datum_is_bytestring(name)) {
    abortf(ctxt, "host instruction should be a string");
    return (datum){};
  }
  // fprintf(stderr, "host ffi %s\n", datum_repr(name));
  datum res;
  if (!strcmp(name->bytestring_value, "call-extension")) {
    if (!datum_is_list(args) || list_length(args) == 0) {
      abortf(ctxt, "call-extension expected at least a single arg");
      return (datum){};
    }
    datum *fn = list_at(args, 0);
    datum callargs = list_get_tail(args);
    datum (*fnptr)(datum *, context *) = datum_get_builtin_ptr(fn, ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
    // fprintf(stderr, "call extension %p %s\n", fnptr, datum_repr(args));
    datum results = fnptr(&callargs, ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
    return results;
  } else if (!strcmp(name->bytestring_value, "deref-pointer")) {
    res = datum_make_pointer(datum_deref);
  } else if (!strcmp(name->bytestring_value, "serialize-pointer")) {
    res = datum_make_pointer(datum_serialize);
  } else if (!strcmp(name->bytestring_value, "copy-to-heap-pointer")) {
    res = datum_make_pointer(datum_copy_to_heap);
  } else if (!strcmp(name->bytestring_value, "ser-pointer")) {
    res = datum_make_pointer(datum_ser);
  } else if (!strcmp(name->bytestring_value, "pointer-call-pointer")) {
    res = datum_make_pointer(pointer_call);
  } else if (!strcmp(name->bytestring_value, "head")) {
    res = datum_make_pointer(builtin_head);
  } else if (!strcmp(name->bytestring_value, "tail")) {
    res = datum_make_pointer(builtin_tail);
  } else if (!strcmp(name->bytestring_value, "cons")) {
    res = datum_make_pointer(builtin_cons);
  } else if (!strcmp(name->bytestring_value, "eq")) {
    res = datum_make_pointer(builtin_eq);
  } else if (!strcmp(name->bytestring_value, "null")) {
    res = datum_make_pointer(NULL);
  } else if (!strcmp(name->bytestring_value, "dlopen")) {
    res = datum_make_pointer(dlopen);
  } else if (!strcmp(name->bytestring_value, "dlsym")) {
    res = datum_make_pointer(dlsym);
  } else if (!strcmp(name->bytestring_value, "RTLD_LAZY")) {
    res = datum_make_int(RTLD_LAZY);
  } else {
    abortf(ctxt, "unknown host instruction");
    return (datum){};
  }
  return (datum_make_list_of(res));
}

LOCAL void ffi_type_init(ffi_type **type, datum *definition, context *ctxt) {
  if (!datum_is_symbol(definition)) {
    abortf(ctxt, "type should be a symbol");
    return;
  }
  if (!strcmp(definition->symbol_value, "string")) {
    *type = &ffi_type_pointer;
    return;
  }
  if (!strcmp(definition->symbol_value, "sizet")) {
    *type = &ffi_type_uint64; // danger!
    return;
  }
  if (!strcmp(definition->symbol_value, "pointer")) {
    *type = &ffi_type_pointer;
    return;
  }
  if (!strcmp(definition->symbol_value, "int")) {
    *type = &ffi_type_sint;
    return;
  }
  abortf(ctxt, "unknown type: %s", datum_repr(definition));
  return;
}

LOCAL void pointer_ffi_init_cif(datum *sig, ffi_cif *cif, ffi_type **arg_types,
                                ffi_type **ret_type, context *ctxt) {
  if (list_length(sig) != 2) {
    abortf(ctxt, "the signature should be a two-item list");
    return;
  }
  datum *arg_defs = list_at(sig, 0);
  int arg_count;
  for (arg_count = 0; arg_count < list_length(arg_defs); ++arg_count) {
    ffi_type_init(arg_types + arg_count, list_at(arg_defs, arg_count), ctxt);
    if (ctxt->aborted) {
      return;
    }
  }
  ffi_type_init(ret_type, list_at(sig, 1), ctxt);
  if (ctxt->aborted) {
    return;
  }
  ffi_status status;
  // TODO(): for variadic functions, prep_cif_var must be used.
  // Without it, linux works somehow and mac does not.
  if ((status = ffi_prep_cif(cif, FFI_DEFAULT_ABI, arg_count, *ret_type,
                             arg_types)) != FFI_OK) {
    abortf(ctxt, "something went wrong during ffi_prep_cif");
    return;
  }
}

LOCAL void pointer_ffi_serialize_args(datum *args, void **cargs, int nargs,
                                      context *ctxt) {
  if (list_length(args) != nargs) {
    abortf(ctxt, "incorrect number of args for FFI call");
    return;
  }
  int arg_cnt = 0;
  for (arg_cnt = 0; arg_cnt < nargs; ++arg_cnt) {
    datum *a = list_at(args, arg_cnt);

    if (!datum_is_blob(a)) {
      abortf(ctxt, "pointer-call expects blobs");
      return;
    }
    blob *b = datum_get_blob(a);
    cargs[arg_cnt] = b->begin;
  }
}

LOCAL datum datum_copy_to_heap(datum *args, context *ctxt) {
  datum *form = args;
  if (!datum_is_list(form) || list_length(form) != 1) {
    abortf(ctxt, "copy-to-heap expected a single argument");
    return (datum){};
  }
  datum *d = list_at(form, 0);
  if (!datum_is_blob(d)) {
    abortf(ctxt, "blob expected, got something else");
    return (datum){};
  }
  blob *b = datum_get_blob(d);
  void *cpy = malloc(b->length);
  memcpy(cpy, b->begin, b->length);
  return datum_make_list_of(datum_make_pointer(cpy));
}

LOCAL datum datum_ser(datum *args, context *ctxt) {
  datum *form = args;
  if (!datum_is_list(form) || list_length(form) != 1) {
    abortf(ctxt, "ser expected a single argument");
    return (datum){};
  }
  datum *d = list_at(form, 0);
  if (datum_is_bytestring(d)) {
    blob b = blob_make(d->bytestring_value, 1 + strlen(d->bytestring_value));
    return datum_make_list_of(datum_make_blob(b));
  }
  if (datum_is_integer(d)) {
    return datum_make_blob_int64_t(d->integer_value);
  }
  abortf(ctxt, "serialization not supported for %s", datum_repr(d));
  return (datum){};
}

LOCAL datum datum_serialize(datum *args, context *ctxt) {
  datum *form = args;
  if (!datum_is_list(form) || list_length(form) != 2) {
    abortf(ctxt, "serialize expected a pair on stack");
    return (datum){};
  }
  datum *d = list_at(form, 0);
  datum *desc = list_at(form, 1);
  if (!datum_is_symbol(desc)) {
    abortf(ctxt, "serialize expected a symbol");
    return (datum){};
  }
  char *des = desc->symbol_value;
  if (!strcmp(des, "string")) {
    if (!datum_is_bytestring(d)) {
      abortf(ctxt, "string expected, got something else");
      return (datum){};
    }
    void *addr;
    if (!strcmp(d->bytestring_value, "__magic_null_string__")) {
      addr = NULL;
    } else {
      addr = d->bytestring_value;
    }
    return (datum_make_list_of(datum_make_pointer(addr)));
  } else if (!strcmp(des, "sizet")) {
    if (!datum_is_integer(d)) {
      abortf(ctxt, "int expected, got something else");
      return (datum){};
    }
    return (datum_make_list_of(datum_make_blob_size_t(d->integer_value)));
  } else if (!strcmp(des, "int")) {
    if (!datum_is_integer(d)) {
      abortf(ctxt, "int expected, got something else");
      return (datum){};
    }
    return (datum_make_list_of(datum_make_blob_int(d->integer_value)));
  } else if (!strcmp(des, "pointer")) {
    return datum_make_list_of(datum_copy(d));
  } else {
    abortf(ctxt, "cannot load an argument");
    return (datum){};
  }
}

LOCAL datum datum_deref(datum *args, context *ctxt) {
  datum *form = args;
  if (!datum_is_list(form) || list_length(form) != 2) {
    abortf(ctxt, "deref expected a pair on stack");
    return (datum){};
  }
  datum *what = list_at(form, 0);
  datum *how = list_at(form, 1);
  size_t sz = get_sizeof(how, ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  void *ptr = *datum_get_pointer(what, ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  blob dereferenced = blob_make(ptr, sz);
  return datum_make_list_of(datum_make_blob(dereferenced));
}

LOCAL size_t get_sizeof(datum *rett, context *ctxt) {
  if (!datum_is_symbol(rett)) {
    abortf(ctxt, "bad type: %s", datum_repr(rett));
    return 0;
  }
  char *rettype = rett->symbol_value;
  if (!strcmp(rettype, "sizet")) {
    return (sizeof(size_t));
  } else if (!strcmp(rettype, "pointer")) {
    return (sizeof(void *));
  } else if (!strcmp(rettype, "int")) {
    return (sizeof(int));
  } else if (!strcmp(rettype, "string")) {
    return (sizeof(char *));
  }
  abortf(ctxt, "uknown type: %s", datum_repr(rett));
  return 0;
}

LOCAL datum pointer_call(datum *argz, context *ctxt) {
  if (!datum_is_list(argz) || list_length(argz) != 3) {
    abortf(ctxt, "pointer-call expected a triple on stack");
    return (datum){};
  }

  datum *fpt = list_at(argz, 0);
  datum *sig = list_at(argz, 1);
  datum *args = list_at(argz, 2);
  void (*fn_ptr)(void) = datum_get_fn_ptr(fpt, ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  ffi_cif cif;
  ffi_type *arg_types[32];
  ffi_type *ret_type;
  pointer_ffi_init_cif(sig, &cif, &arg_types[0], &ret_type, ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  int nargs = list_length(list_at(sig, 0));
  void *cargs[32];
  pointer_ffi_serialize_args(args, cargs, nargs, ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  datum *rettype = list_at(sig, 1);
  size_t sz = get_sizeof(rettype, ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  blob blb = blob_make_uninitialized(sz);
  ffi_call(&cif, fn_ptr, blb.begin, cargs);
  return (datum_make_list_of(datum_make_blob(blb)));
}

LOCAL void (*datum_get_fn_ptr(datum *d, context *ctxt))(void) {
  void **ptr = datum_get_pointer(d, ctxt);
  if (ctxt->aborted) {
    return NULL;
  }
  return __extension__(void (*)(void)) * ptr;
}

LOCAL datum (*datum_get_builtin_ptr(datum *d, context *ctxt))(datum *,
                                                              context *) {
  void **ptr = datum_get_pointer(d, ctxt);
  if (ctxt->aborted) {
    return NULL;
  }
  return (datum(*)(datum *, context *)) * ptr;
}
