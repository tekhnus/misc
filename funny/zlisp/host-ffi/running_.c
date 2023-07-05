#if INTERFACE
#include <assert.h>
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
#endif
#include <ffi.h>
struct cif_and_data {
  ffi_cif cif;
  ffi_type types[1024];
  ffi_type *(pointers[1024 * 256]);
  size_t ntypes;
  size_t npointers;
};
#include <running_.h>

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
  if (!strcmp(datum_get_bytestring(name), "call-builtin")) {
    if (!datum_is_list(args) || list_length(args) == 0) {
      abortf(ctxt, "call-builtin expected at least a single arg");
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
  } else if (!strcmp(datum_get_bytestring(name), "deref")) {
    res = datum_make_pointer(builtin_deref);
  } else if (!strcmp(datum_get_bytestring(name), "copy-to-memory")) {
    res = datum_make_pointer(builtin_copy_to_memory);
  } else if (!strcmp(datum_get_bytestring(name), "serialize")) {
    res = datum_make_pointer(builtin_serialize);
  } else if (!strcmp(datum_get_bytestring(name), "call-ffi")) {
    res = datum_make_pointer(builtin_call_ffi);
  } else if (!strcmp(datum_get_bytestring(name), "head")) {
    res = datum_make_pointer(builtin_head);
  } else if (!strcmp(datum_get_bytestring(name), "tail")) {
    res = datum_make_pointer(builtin_tail);
  } else if (!strcmp(datum_get_bytestring(name), "cons")) {
    res = datum_make_pointer(builtin_cons);
  } else if (!strcmp(datum_get_bytestring(name), "eq")) {
    res = datum_make_pointer(builtin_eq);
  } else if (!strcmp(datum_get_bytestring(name), "len")) {
    res = datum_make_pointer(builtin_len);
  } else if (!strcmp(datum_get_bytestring(name), "null")) {
    res = datum_make_pointer(NULL);
  } else if (!strcmp(datum_get_bytestring(name), "malloc")) {
    res = datum_make_pointer(malloc);
  } else if (!strcmp(datum_get_bytestring(name), "dlopen")) {
    res = datum_make_pointer(dlopen);
  } else if (!strcmp(datum_get_bytestring(name), "dlsym")) {
    res = datum_make_pointer(dlsym);
  } else if (!strcmp(datum_get_bytestring(name), "RTLD_LAZY")) {
    res = datum_make_blob_int(RTLD_LAZY);
  } else {
    abortf(ctxt, "unknown host instruction");
    return (datum){};
  }
  return (datum_make_list_of(res));
}

LOCAL ffi_type **cifd_alloc_pointers(struct cif_and_data *cifd, size_t count) {
  ffi_type **result = cifd->pointers + cifd->npointers;
  cifd->npointers += count;
  return result;
}

LOCAL ffi_type *cifd_alloc_type(struct cif_and_data *cifd) {
  ffi_type *result = cifd->types + cifd->ntypes;
  cifd->ntypes += 1;
  return result;
}

LOCAL ffi_type *ffi_type_init(struct cif_and_data *cifd, datum *definition,
                              context *ctxt) {
  ffi_type *result = cifd_alloc_type(cifd);
  if (datum_is_list(definition) && list_length(definition) == 2) {
    datum *sz = list_at(definition, 0);
    assert(datum_is_integer(sz));
    size_t siz = datum_get_integer(sz);
    datum *elem = list_at(definition, 1);
    ffi_type *elem_t = ffi_type_init(cifd, elem, ctxt);
    if (ctxt->aborted) {
      return NULL;
    }
    result->type = FFI_TYPE_STRUCT;
    result->size = 0;
    result->alignment = 0;
    result->elements = cifd_alloc_pointers(cifd, siz + 1);
    for (size_t i = 0; i < siz; ++i) {
      result->elements[i] = elem_t;
    }
    result->elements[siz] = NULL;
  } else if (!datum_is_symbol(definition)) {
    abortf(ctxt, "type should be a symbol");
    return NULL;
  } else if (!strcmp(datum_get_symbol(definition), "sizet")) {
    *result = ffi_type_uint64; // danger!
  } else if (!strcmp(datum_get_symbol(definition), "pointer")) {
    *result = ffi_type_pointer;
  } else if (!strcmp(datum_get_symbol(definition), "uint8_t")) {
    *result = ffi_type_uint8;
  } else if (!strcmp(datum_get_symbol(definition), "int64_t")) {
    *result = ffi_type_sint64;
  } else if (!strcmp(datum_get_symbol(definition), "char")) {
    *result = ffi_type_schar; // danger!
  } else if (!strcmp(datum_get_symbol(definition), "int")) {
    *result = ffi_type_sint;
  } else if (!strcmp(datum_get_symbol(definition), "int64_t")) {
    *result = ffi_type_sint64;
  } else if (!strcmp(datum_get_symbol(definition), "context")) {
    *result = *ffi_type_init_struct(
        cifd,
        datum_make_list_of(datum_make_symbol("uint8_t"),
                           datum_make_list_of(datum_make_int(1024),
                                              datum_make_symbol("char"))),
        ctxt);
    if (ctxt->aborted) {
      return NULL;
    }
  } else if (!strcmp(datum_get_symbol(definition), "array")) {
    *result =
        *ffi_type_init_struct(cifd,
                              datum_make_list_of(datum_make_symbol("pointer"),
                                                 datum_make_symbol("sizet")),
                              ctxt);
    if (ctxt->aborted) {
      return NULL;
    }
  } else if (!strcmp(datum_get_symbol(definition), "vec")) {
    *result =
        *ffi_type_init_struct(cifd,
                              datum_make_list_of(datum_make_symbol("array"),
                                                 datum_make_symbol("sizet")),
                              ctxt);
    if (ctxt->aborted) {
      return NULL;
    }
  } else if (!strcmp(datum_get_symbol(definition), "blob")) {
    *result =
        *ffi_type_init_struct(cifd,
                              datum_make_list_of(datum_make_symbol("pointer"),
                                                 datum_make_symbol("sizet")),
                              ctxt);
    if (ctxt->aborted) {
      return NULL;
    }
  } else if (!strcmp(datum_get_symbol(definition), "datum")) {
    *result = *ffi_type_init_struct(
        cifd,
        datum_make_list_of(
            datum_make_symbol("uint8_t"), datum_make_symbol("array"),
            datum_make_symbol("pointer"), datum_make_symbol("pointer"),
            datum_make_symbol("blob"), datum_make_symbol("int64_t"), ),
        ctxt);
    if (ctxt->aborted) {
      return NULL;
    }
  } else if (!strcmp(datum_get_symbol(definition), "extension")) {
    *result = *ffi_type_init_struct(
        cifd, datum_make_list_of(datum_make_symbol("pointer")), ctxt);
    if (ctxt->aborted) {
      return NULL;
    }
  } else if (!strcmp(datum_get_symbol(definition), "lisp_extension")) {
    *result = *ffi_type_init_struct(
        cifd,
        datum_make_list_of(datum_make_symbol("extension"),
                           datum_make_symbol("vec"), datum_make_symbol("datum"),
                           datum_make_symbol("datum"),
                           datum_make_symbol("pointer")),
        ctxt);
    if (ctxt->aborted) {
      return NULL;
    }
  } else if (!strcmp(datum_get_symbol(definition), "result")) {
    *result =
        *ffi_type_init_struct(cifd,
                              datum_make_list_of(datum_make_symbol("datum"),
                                                 datum_make_symbol("datum")),
                              ctxt);
    if (ctxt->aborted) {
      return NULL;
    }
  } else {
    abortf(ctxt, "unknown type: %s", datum_repr(definition));
    return NULL;
  }
  return result;
}

LOCAL ffi_type *ffi_type_init_struct(struct cif_and_data *cifd, datum members,
                                     context *ctxt) {
  ffi_type *result = cifd_alloc_type(cifd);
  assert(datum_is_list(&members));
  int n = list_length(&members);
  result->type = FFI_TYPE_STRUCT;
  result->size = 0;
  result->alignment = 0;
  result->elements = cifd_alloc_pointers(cifd, n + 1);
  for (int i = 0; i < n; ++i) {
    result->elements[i] = ffi_type_init(cifd, list_at(&members, i), ctxt);
    if (ctxt->aborted) {
      return NULL;
    }
  }
  result->elements[n] = NULL;
  return result;
}

LOCAL void pointer_ffi_init_cif(datum *sig, struct cif_and_data *cifd,
                                context *ctxt) {
  ffi_type *ret_type;
  datum *arg_defs = list_at(sig, 0);
  int arg_count = list_length(arg_defs);
  ffi_type **args2 = cifd_alloc_pointers(cifd, arg_count);
  if (list_length(sig) != 2) {
    abortf(ctxt, "the signature should be a two-item list");
    return;
  }
  for (int i = 0; i < list_length(arg_defs); ++i) {
    args2[i] = ffi_type_init(cifd, list_at(arg_defs, i), ctxt);
    if (ctxt->aborted) {
      return;
    }
  }
  ret_type = ffi_type_init(cifd, list_at(sig, 1), ctxt);
  if (ctxt->aborted) {
    return;
  }
  ffi_status status =
      ffi_prep_cif(&cifd->cif, FFI_DEFAULT_ABI, arg_count, ret_type, args2);
  // TODO(): for variadic functions, prep_cif_var must be used.
  // Without it, linux works somehow and mac does not.
  if (status != FFI_OK) {
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
      abortf(ctxt, "call-ffi expects blobs");
      return;
    }
    blob *b = datum_get_blob(a);
    cargs[arg_cnt] = b->begin;
  }
}

LOCAL datum builtin_copy_to_memory(datum *args, context *ctxt) {
  datum *form = args;
  if (!datum_is_list(form) || list_length(form) != 2) {
    abortf(ctxt, "copy-to-heap expected two arguments");
    return (datum){};
  }
  datum *dst = list_at(form, 0);
  void *dstptr = *datum_get_pointer(dst, ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  datum *d = list_at(form, 1);
  if (!datum_is_blob(d)) {
    abortf(ctxt, "blob expected, got something else");
    return (datum){};
  }
  blob *b = datum_get_blob(d);
  memcpy(dstptr, b->begin, b->length);
  return datum_make_list_of();
}

LOCAL datum builtin_serialize(datum *args, context *ctxt) {
  datum *form = args;
  if (!datum_is_list(form) || list_length(form) != 1) {
    abortf(ctxt, "ser expected a single argument");
    return (datum){};
  }
  datum *d = list_at(form, 0);
  if (datum_is_bytestring(d)) {
    blob b = blob_make_copy(
      datum_get_bytestring(d),
      1 + strlen(datum_get_bytestring(d)));
    return datum_make_list_of(datum_make_blob(b));
  }
  if (datum_is_integer(d)) {
    return datum_make_list_of(datum_make_blob_int64_t(datum_get_integer(d)));
  }
  abortf(ctxt, "serialization not supported for %s", datum_repr(d));
  return (datum){};
}

LOCAL datum builtin_deref(datum *args, context *ctxt) {
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
  blob dereferenced = blob_make_copy(ptr, sz);
  return datum_make_list_of(datum_make_blob(dereferenced));
}

LOCAL size_t get_sizeof(datum *rett, context *ctxt) {
  if (!datum_is_symbol(rett)) {
    abortf(ctxt, "bad type: %s", datum_repr(rett));
    return 0;
  }
  char *rettype = datum_get_symbol(rett);
  if (!strcmp(rettype, "sizet")) {
    return (sizeof(size_t));
  } else if (!strcmp(rettype, "pointer")) {
    return (sizeof(void *));
  } else if (!strcmp(rettype, "int")) {
    return (sizeof(int));
  } else if (!strcmp(rettype, "context")) {
    return (sizeof(context));
  } else if (!strcmp(rettype, "array")) {
    return (sizeof(array));
  } else if (!strcmp(rettype, "vec")) {
    return (sizeof(vec));
  } else if (!strcmp(rettype, "blob")) {
    return (sizeof(blob));
  } else if (!strcmp(rettype, "datum")) {
    return (sizeof(datum));
  } else if (!strcmp(rettype, "extension")) {
    return (sizeof(extension));
  } else if (!strcmp(rettype, "lisp_extension")) {
    return (sizeof(lisp_extension));
  } else if (!strcmp(rettype, "result")) {
    return (sizeof(result));
  }
  abortf(ctxt, "sizeof type not known: %s", datum_repr(rett));
  return 0;
}

LOCAL datum builtin_call_ffi(datum *argz, context *ctxt) {
  if (!datum_is_list(argz) || list_length(argz) != 3) {
    abortf(ctxt, "call-ffi expected a triple on stack");
    return (datum){};
  }

  datum *fpt = list_at(argz, 0);
  datum *sig = list_at(argz, 1);
  datum *args = list_at(argz, 2);
  void (*fn_ptr)(void) = datum_get_fn_ptr(fpt, ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  struct cif_and_data cifd = {};
  pointer_ffi_init_cif(sig, &cifd, ctxt);
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
  blob blb = blob_make(sz);
  ffi_call(&cifd.cif, fn_ptr, blb.begin, cargs);
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
