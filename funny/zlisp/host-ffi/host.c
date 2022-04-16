#define _GNU_SOURCE
#include <host.h>
#if INTERFACE
#include <zlisp-impl/zlisp-impl.h>
#include <string.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <ffi.h>
#endif

void *simplified_dlopen(char *path) {
  if (strlen(path) == 0) {
    return RTLD_DEFAULT;
  }
  return dlopen(path, RTLD_LAZY);
}

void *simplified_dlsym(void *handle, const char *symbol) {
  return dlsym(handle, symbol);
}

LOCAL fdatum builtin_ptr_not_null_pointer(datum *pointer) {
  if (!datum_is_pointer(pointer)) {
    return fdatum_make_panic("not-null-pointer expects a pointer");
  }
  if (datum_get_pointer_value(pointer) != NULL) {
    return fdatum_make_ok(datum_make_list_1(datum_make_nil()));
  }
  return fdatum_make_ok(datum_make_nil());
}

LOCAL fdatum builtin_ptr_dereference_and_cast(datum *ptpt, datum *new_descriptor) {
  if (!datum_is_pointer(ptpt) || !datum_is_the_symbol(datum_get_pointer_descriptor(ptpt), "pointer")) {
    return fdatum_make_panic("dereference expected a pointer to pointer");
  }
  return fdatum_make_ok(datum_make_pointer(*((void **)datum_get_pointer_value(ptpt)), new_descriptor));
}

fdatum builtin_ptr_wrap_ptr_into_ptr(datum *pt) {
  if (!datum_is_pointer(pt)) {
    return fdatum_make_panic("wrap-ptr-into-ptr expected a pointer");
  }
  void **ptpt = malloc(sizeof(void **));
  *ptpt = datum_get_pointer_value(pt);
  return fdatum_make_ok(datum_make_pointer(ptpt, datum_make_symbol("pointer")));
}

fdatum perform_host_instruction(datum *name, datum *arg) {
  if (!datum_is_bytestring(name)) {
    return fdatum_make_panic("host instruction should be a string");
  }
  datum *res;
  if (!strcmp(name->bytestring_value, "wrap-pointer-into-pointer")) {
    res = datum_make_int((int64_t)builtin_ptr_wrap_ptr_into_ptr);
  } else if (!strcmp(name->bytestring_value, "not-null-pointer")) {
    res = datum_make_int((int64_t)builtin_ptr_not_null_pointer);
  } else if (!strcmp(name->bytestring_value, "panic")) {
    res = datum_make_int((int64_t)builtin_panic);
  } else if (!strcmp(name->bytestring_value, "head")) {
    res = datum_make_int((int64_t)builtin_head);
  } else if (!strcmp(name->bytestring_value, "tail")) {
    res = datum_make_int((int64_t)builtin_tail);
  } else if (!strcmp(name->bytestring_value, "dlopen")) {
    res = datum_make_int((int64_t)simplified_dlopen);
  } else if (!strcmp(name->bytestring_value, "dlsym")) {
    res = datum_make_int((int64_t)simplified_dlsym);
  } else if (!strcmp(name->bytestring_value, "dereference-and-cast")) {
    res = datum_make_int((int64_t)builtin_ptr_dereference_and_cast);
  } else if (!strcmp(name->bytestring_value, "pointer-call")) {
    datum *form = arg;
    if (!datum_is_list(form) || list_length(form) != 2) {
      return fdatum_make_panic("pointer-call expected a pair on stack");
    }
    datum *fn = form->list_head;
    datum *args = form->list_tail->list_head;
    fdatum resu = pointer_call(fn, args);
    if (fdatum_is_panic(resu)) {
      return fdatum_make_panic(resu.panic_message);
    }
    res = resu.ok_value;
  } else if (!strcmp(name->bytestring_value, "dereference-datum")){
    datum *form = arg;
    if (!datum_is_integer(arg)) {
      return fdatum_make_panic("dereference-datum expected a pointer");
    }
    return *(fdatum *)arg->integer_value;
  } else if (!strcmp(name->bytestring_value, "deref")) {
    datum *form = arg;
    if (!datum_is_list(form) || list_length(form) != 2) {
      return fdatum_make_panic("deref expected a pair on stack");
    }
    datum *what = form->list_head;
    datum *how = form->list_tail->list_head;
    if (!datum_is_integer(what)) {
      return fdatum_make_panic("deref expected a pointer");
    }
    if (!datum_is_symbol(how)) {
      return fdatum_make_panic("deref expected a symbol");
    }
    char *rettype = how->symbol_value;
    void *wha = (void *)what->integer_value;
    if (!strcmp(rettype, "pointer")) {
      return fdatum_make_ok(datum_make_pointer_to_pointer(wha));
    }
    else if (!strcmp(rettype, "sizet")) {
      return fdatum_make_ok(datum_make_int((int64_t)*(size_t *)wha));
    }
    else if (!strcmp(rettype, "int")) {
      return fdatum_make_ok(datum_make_int((int64_t)*(int *)wha));
    }
    else if (!strcmp(rettype, "string")) {
      return fdatum_make_ok(datum_make_bytestring((char *)wha));
    }
    else if (!strcmp(rettype, "fdatum")) {
      return fdatum_make_ok(datum_make_pointer(wha, datum_make_symbol("fdatum")));
    }
    else if (!strcmp(rettype, "val")) {
      return *(fdatum *)wha;
    } else {
      return fdatum_make_panic("unknown return type for deref");
    }
  }
  else {
    return fdatum_make_panic("unknown host instruction");
  }
  return fdatum_make_ok(res);
}

fdatum routine_run_and_get_value_c_host(state **ctxt, prog *p) {
  return routine_run_and_get_value(ctxt, p, perform_host_instruction);
}


bool ffi_type_init(ffi_type **type, datum *definition) {
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
    *type = &ffi_type_uint64;
    return true;
  }
  if (!strcmp(definition->symbol_value, "int")) {
    *type = &ffi_type_sint;
    return true;
  }
  if (!strcmp(definition->symbol_value, "datum")) {
    *type = &ffi_type_pointer;
    return true;
  }
  if (!strcmp(definition->symbol_value, "val") || !strcmp(definition->symbol_value, "fdatum")) {
    *type = malloc(sizeof(ffi_type));
    (*type)->type = FFI_TYPE_STRUCT;
    (*type)->size = 0; // Lost 5 hours debugging non-deterministic failures on
                       // Mac before adding this line.
    (*type)->alignment = 0;
    ffi_type **elements = malloc(4 * sizeof(ffi_type *));
    elements[0] = &ffi_type_sint;
    elements[1] = &ffi_type_pointer;
    elements[2] = &ffi_type_pointer;
    elements[3] = NULL;
    (*type)->elements = elements;
    return type;
  }
  return false;
}

char *pointer_ffi_init_cif(datum *f, ffi_cif *cif) {
  datum *sig = datum_get_pointer_descriptor(f);
  if (!datum_is_list(sig) || datum_is_nil(sig) ||
      datum_is_nil(sig->list_tail) ||
      !datum_is_nil(sig->list_tail->list_tail)) {
    return "the signature should be a two-item list";
  }
  ffi_type **arg_types = malloc(sizeof(ffi_type *) * 32);
  int arg_count = 0;
  datum *arg_def;
  for (arg_def = datum_get_pointer_descriptor(f)->list_head; !datum_is_nil(arg_def);
       arg_def = arg_def->list_tail) {
    if (!ffi_type_init(arg_types + arg_count, arg_def->list_head)) {
      return "something wrong with the argument type signature";
    }
    ++arg_count;
  }
  ffi_type *ret_type;
  if (!ffi_type_init(&ret_type, sig->list_tail->list_head)) {
    return "something wrong with the return type signature";
  }
  ffi_status status;
  if ((status = ffi_prep_cif(cif, FFI_DEFAULT_ABI, arg_count, ret_type,
                             arg_types)) != FFI_OK) {
    return "something went wrong during ffi_prep_cif";
  }
  return NULL;
}

char *pointer_ffi_serialize_args(datum *f, datum *args, void **cargs) {
  int arg_cnt = 0;
  datum *arg = args;
  for (datum *argt = datum_get_pointer_descriptor(f)->list_head; !datum_is_nil(argt);
       argt = argt->list_tail) {
    if (datum_is_nil(arg)) {
      return "too few arguments";
    }
    if (!strcmp(argt->list_head->symbol_value, "string")) {
      if (!datum_is_bytestring(arg->list_head)) {
        return "string expected, got something else";
      }
      cargs[arg_cnt] = &arg->list_head->bytestring_value;
    } else if (!strcmp(argt->list_head->symbol_value, "sizet")) {
      if (!datum_is_integer(arg->list_head)) {
        return "int expected, got something else";
      }
      cargs[arg_cnt] = &arg->list_head->integer_value;
    } else if (!strcmp(argt->list_head->symbol_value, "pointer")) {
      datum *sig;
      if (!datum_is_pointer(arg->list_head) ||
          !datum_is_symbol(sig = datum_get_pointer_descriptor(arg->list_head)) ||
          strcmp(sig->symbol_value, "pointer")) {
        return "pointer expected, got something else";
      }
      cargs[arg_cnt] = datum_get_pointer_value(arg->list_head);
    } else if (!strcmp(argt->list_head->symbol_value, "datum")) {
      cargs[arg_cnt] = &arg->list_head;
    } else if (!strcmp(argt->list_head->symbol_value, "fdatum")) {
      datum *sig;
      if (!datum_is_pointer(arg->list_head) ||
          !datum_is_symbol(sig = datum_get_pointer_descriptor(arg->list_head)) ||
          strcmp(sig->symbol_value, "fdatum")) {
        return "fdatum expected, got something else";
      }
      cargs[arg_cnt] = datum_get_pointer_value(arg->list_head);
    } else {
      return "cannot load an argument";
    }
    arg = arg->list_tail;
    ++arg_cnt;
  }
  if (!datum_is_nil(arg)) {
    return "too much arguments";
  }
  return NULL;
}

fdatum pointer_ffi_call(datum *f, ffi_cif *cif, void **cargs) {
  void (*fn_ptr)(void) = __extension__(void (*)(void))(datum_get_pointer_value(f));
  char *rettype = datum_get_pointer_descriptor(f)->list_tail->list_head->symbol_value;

  void *res;
  if (!strcmp(rettype, "pointer")) {
    res = malloc(sizeof(void *));
  }
  else if (!strcmp(rettype, "sizet")) {
    res = malloc(sizeof(size_t));
  }
  else if (!strcmp(rettype, "int")) {
    res = malloc(sizeof(int));
  }
  else if (!strcmp(rettype, "string")) {
    res = malloc(sizeof(char *));
  }
  else if (!strcmp(rettype, "fdatum")) {
    res = malloc(sizeof(fdatum));
  }
  else if (!strcmp(rettype, "val")) {
    res = malloc(sizeof(fdatum));
  } else {
    return fdatum_make_panic("unknown return type for extern func");
  }
  ffi_call(cif, fn_ptr, res, cargs);
  return fdatum_make_ok(datum_make_int((int64_t)res));

}

fdatum pointer_call(datum *f, datum *args) {
  if (!datum_is_pointer(f)) {
    return fdatum_make_panic("pointer_call expects a pointer");
  }
  ffi_cif cif;
  char *err = NULL;
  err = pointer_ffi_init_cif(f, &cif);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  void *cargs[32];
  err = pointer_ffi_serialize_args(f, args, cargs);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  return pointer_ffi_call(f, &cif, cargs);
}

datum *datum_make_pointer(void *data, datum *signature) {
  return datum_make_list_2(datum_make_symbol("cptr"), datum_make_list_2(datum_make_int((int64_t)data), signature));
}

datum *datum_make_pointer_to_pointer(void **ptr) {
  return datum_make_pointer(ptr, datum_make_symbol("pointer"));
}

void *datum_get_pointer_value(datum *d) {
  if (!datum_is_pointer(d)) {
    fprintf(stderr, "Not a pointer!");
    exit(1);
  }
  return (void*)d->list_tail->list_head->list_head->integer_value;
}

datum *datum_get_pointer_descriptor(datum *d) {
  if (!datum_is_pointer(d)) {
    fprintf(stderr, "Not a pointer!");
    exit(1);
  }
  return d->list_tail->list_head->list_tail->list_head;
}

bool datum_is_pointer(datum *e) { return datum_is_list(e) && !datum_is_nil(e) && datum_is_the_symbol(e->list_head, "cptr"); }
