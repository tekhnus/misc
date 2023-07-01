#include <running.h>
#if INTERFACE
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
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
      res = (result){datum_make_symbol("interpreter-panic"), datum_make_bytestring(ctxt->error)};
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
    datum results = fnptr(&callargs, ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
    return results;
  } else if (!strcmp(name->bytestring_value, "deref-pointer")) {
    res = datum_make_ptr(datum_deref);
  } else if (!strcmp(name->bytestring_value, "mkptr-pointer")) {
    res = datum_make_ptr(datum_mkptr);
  } else if (!strcmp(name->bytestring_value, "pointer-call-pointer")) {
    res = datum_make_ptr(pointer_call);
  } else if (!strcmp(name->bytestring_value, "head")) {
    res = datum_make_ptr(builtin_head);
  } else if (!strcmp(name->bytestring_value, "tail")) {
    res = datum_make_ptr(builtin_tail);
  } else if (!strcmp(name->bytestring_value, "cons")) {
    res = datum_make_ptr(builtin_cons);
  } else if (!strcmp(name->bytestring_value, "eq")) {
    res = datum_make_ptr(builtin_eq);
  } else if (!strcmp(name->bytestring_value, "dlopen")) {
    res = datum_make_ptr(dlopen);
  } else if (!strcmp(name->bytestring_value, "dlsym")) {
    res = datum_make_ptr(dlsym);
  } else if (!strcmp(name->bytestring_value, "RTLD_LAZY")) {
    res = datum_make_int(RTLD_LAZY);
  } else {
    abortf(ctxt, "unknown host instruction");
    return (datum){};
  }
  return (datum_make_list_of(res));
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
  if (!strcmp(definition->symbol_value, "int64")) {
    *type = &ffi_type_pointer;
    return true;
  }
  if (!strcmp(definition->symbol_value, "int")) {
    *type = &ffi_type_sint;
    return true;
  }
  return false;
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
    if (!ffi_type_init(arg_types + arg_count, list_at(arg_defs, arg_count))) {
      abortf(ctxt, "something wrong with the argument type signature");
      return;
    }
  }
  if (!ffi_type_init(ret_type, list_at(sig, 1))) {
    abortf(ctxt, "something wrong with the ret type signature");
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

LOCAL void pointer_ffi_serialize_args(datum *args, void **cargs, int nargs, context *ctxt) {
  if (list_length(args) != nargs) {
    abortf(ctxt, "incorrect number of args for FFI call");
    return;
  }
  int arg_cnt = 0;
  for (arg_cnt = 0; arg_cnt < nargs; ++arg_cnt) {
    datum *a = list_at(args, arg_cnt);

    void **ptr = datum_get_ptr(a, ctxt);
    if (ctxt->aborted) {
      return;
    }
    cargs[arg_cnt] = *ptr;
  }
}

LOCAL datum datum_mkptr(datum *args, context *ctxt) {
  datum *form = args;
  if (!datum_is_list(form) || list_length(form) != 2) {
    abortf(ctxt, "mkptr expected a pair on stack");
    return (datum){};
  }
  datum *d = list_at(form, 0);
  datum *desc = list_at(form, 1);
  if (!datum_is_symbol(desc)) {
    abortf(ctxt, "mkptr expected a symbol");
    return (datum){};
  }
  char *des = desc->symbol_value;
  if (!strcmp(des, "string")) {
    if (!datum_is_bytestring(d)) {
      abortf(ctxt, "string expected, got something else");
      return (datum){};
    }
    return (
        datum_make_list_of(datum_make_ptr(& (d->bytestring_value))));
  } else if (!strcmp(des, "sizet")) {
    if (!datum_is_integer(d)) {
      abortf(ctxt, "int expected, got something else");
      return (datum){};
    }
    return (
        datum_make_list_of(datum_make_ptr(& (d->integer_value))));
  } else if (!strcmp(des, "int")) {
    if (!datum_is_integer(d)) {
      abortf(ctxt, "int expected, got something else");
      return (datum){};
    }
    return (
        datum_make_list_of(datum_make_ptr(& (d->integer_value))));
  } else if (!strcmp(des, "int64")) {
    void **ptr = datum_get_ptr(d, ctxt);
    if (ctxt->aborted) {
      return (datum){};
    }
    return (
        datum_make_list_of(datum_make_ptr(ptr)));
  } else if (!strcmp(des, "pointer")) {
    if (ctxt->aborted) {
      return (datum){};
    }
    return (
        datum_make_list_of(datum_copy(d)));
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
  if (!datum_is_symbol(how)) {
    abortf(ctxt, "deref expected a symbol");
    return (datum){};
  }
  char *rettype = how->symbol_value;
  void *wha = datum_get_blob(what)->begin;
  if (!strcmp(rettype, "sizet")) {
    return (
        datum_make_list_of(datum_make_int((int64_t) * (size_t *)wha)));
  } else if (!strcmp(rettype, "int")) {
    return (
        datum_make_list_of(datum_make_int((int64_t) * (int *)wha)));
  } else if (!strcmp(rettype, "int64")) {
    return datum_make_list_of(datum_make_ptr(*(void **)wha));
  } else if (!strcmp(rettype, "pointer")) {
    return datum_make_list_of(datum_make_ptr(wha));
  } else if (!strcmp(rettype, "string")) {
    return (
        datum_make_list_of(datum_make_bytestring(*(char **)wha)));
  } else {
    abortf(ctxt, "unknown return type for deref");
    return (datum){};
  }
}

LOCAL size_t get_sizeof(datum *sig) {
  char *rettype = list_at(sig, 1)->symbol_value;
  if (!strcmp(rettype, "pointer")) {
    return (sizeof(void *));
  } else if (!strcmp(rettype, "sizet")) {
    return (sizeof(size_t));
  } else if (!strcmp(rettype, "int64")) {
    return (sizeof(void *));
  } else if (!strcmp(rettype, "int")) {
    return (sizeof(int));
  } else if (!strcmp(rettype, "string")) {
    return (sizeof(char *));
  }
  return 0;
}

LOCAL datum pointer_call(datum *argz, context *ctxt) {
  // fprintf(stderr, "??? pointer-call\n");
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
  size_t sz = get_sizeof(sig);
  if (sz == 0) {
    abortf(ctxt, "unknown return type for extern func");
    return (datum){};
  }
  blob blb = blob_make_uninitialized(sz);
  datum b = datum_make_blob(blb);
  ffi_call(&cif, fn_ptr, datum_get_blob(&b)->begin, cargs);
  return (datum_make_list_of(b));
}

LOCAL datum datum_make_ptr(void *ptr) {
  // return datum_make_pointer(ptr);
  return datum_make_int((int64_t) ptr);
}

LOCAL void **datum_get_ptr(datum *d, context *ctxt) {
  // return datum_get_pointer(d, ctxt);
  if(!datum_is_integer(d)) {
    abortf(ctxt, "expected a pointer");
    return NULL;
  }
  return (void **)&d->integer_value;  
}

LOCAL void (*datum_get_fn_ptr(datum *d, context *ctxt))(void) {
  void **ptr = datum_get_ptr(d, ctxt);
  if (ctxt->aborted) {
    return NULL;
  }
  return __extension__(void (*)(void))*ptr;
}

LOCAL datum(*datum_get_builtin_ptr(datum *d, context *ctxt))(datum *, context *) {
  void **ptr = datum_get_ptr(d, ctxt);
  if (ctxt->aborted) {
    return NULL;
  }
  return (datum(*)(datum *, context *))*ptr;
}
