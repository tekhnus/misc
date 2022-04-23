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

fdatum perform_host_instruction(datum *name, datum *arg) {
  if (!datum_is_bytestring(name)) {
    return fdatum_make_panic("host instruction should be a string");
  }
  datum *res;
  if (!strcmp(name->bytestring_value, "pointer-call") || !strcmp(name->bytestring_value, "pointer-call-datums")) {
    datum *form = arg;
    if (!datum_is_list(form) || list_length(form) != 2) {
      return fdatum_make_panic("pointer-call expected a pair on stack");
    }
    datum *fn = form->list_head;
    datum *args = form->list_tail->list_head;
    bool datums = !strcmp(name->bytestring_value, "pointer-call-datums");
    fdatum resu = pointer_call(fn, args, datums);
    if (fdatum_is_panic(resu)) {
      return fdatum_make_panic(resu.panic_message);
    }
    res = resu.ok_value;
  } else if (!strcmp(name->bytestring_value, "deref")) {
    return datum_deref(arg);
  } else if (!strcmp(name->bytestring_value, "mkptr")) {
    return datum_mkptr(arg);
  } else if (!strcmp(name->bytestring_value, "panic")) {
    res = datum_make_int((int64_t)builtin_panic);
  } else if (!strcmp(name->bytestring_value, "head")) {
    res = datum_make_int((int64_t)builtin_head);
  } else if (!strcmp(name->bytestring_value, "tail")) {
    res = datum_make_int((int64_t)builtin_tail);
  } else if (!strcmp(name->bytestring_value, "cons")) {
    res = datum_make_int((int64_t)builtin_cons);
  } else if (!strcmp(name->bytestring_value, "eq")) {
    res = datum_make_int((int64_t)builtin_eq);
  } else if (!strcmp(name->bytestring_value, "dlopen")) {
    res = datum_make_int((int64_t)simplified_dlopen);
  } else if (!strcmp(name->bytestring_value, "dlsym")) {
    res = datum_make_int((int64_t)simplified_dlsym);
  } else {
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
    *type = &ffi_type_uint64; // danger!
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
  if (!strcmp(definition->symbol_value, "progslice")) {
    *type = malloc(sizeof(ffi_type));
    (*type)->type = FFI_TYPE_STRUCT;
    (*type)->size = 0; // Lost 5 hours debugging non-deterministic failures on
                       // Mac before adding this line.
    (*type)->alignment = 0;
    ffi_type **elements = malloc(4 * sizeof(ffi_type *));
    elements[0] = &ffi_type_pointer;
    elements[1] = &ffi_type_uint64; // it's actually size_t, danger!
    elements[2] = &ffi_type_uint64;
    elements[3] = NULL;
    (*type)->elements = elements;
    return type;
  }
  return false;
}

char *pointer_ffi_init_cif(datum *f, ffi_cif *cif) {
  datum *sig = datum_get_fnpointer_descriptor(f);
  if (!datum_is_list(sig) || datum_is_nil(sig) ||
      datum_is_nil(sig->list_tail) ||
      !datum_is_nil(sig->list_tail->list_tail)) {
    return "the signature should be a two-item list";
  }
  ffi_type **arg_types = malloc(sizeof(ffi_type *) * 32);
  int arg_count = 0;
  datum *arg_def;
  for (arg_def = datum_get_fnpointer_descriptor(f)->list_head; !datum_is_nil(arg_def);
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

char *pointer_ffi_serialize_args(datum *args, void **cargs, int nargs, bool datums) {
  int arg_cnt = 0;
  datum *arg = args;
  for (arg_cnt = 0; arg_cnt < nargs; ++arg_cnt) {
    if (datum_is_nil(arg)) {
      return "too few arguments";
    }

    if (!datums) {
      if (!datum_is_integer(arg->list_head)) {
        return "int pointer expected, got something else";
      }
      cargs[arg_cnt] = (void *)arg->list_head->integer_value;
    } else {
      cargs[arg_cnt] = &arg->list_head;
    }
    
    arg = arg->list_tail;
  }
  if (!datum_is_nil(arg)) {
    return "too much arguments";
  }
  return NULL;
}

fdatum datum_mkptr(datum *arg) {
  datum *form = arg;
  if (!datum_is_list(form) || list_length(form) != 2) {
    return fdatum_make_panic("mkptr expected a pair on stack");
  }
  datum *d = form->list_head;
  datum *desc = form->list_tail->list_head;
  if (!datum_is_symbol(desc)) {
    return fdatum_make_panic("mkptr expected a symbol");
  }
  char *des = desc->symbol_value;
  if (!strcmp(des, "string")) {
    if (!datum_is_bytestring(d)) {
      return fdatum_make_panic("string expected, got something else");
    }
    return fdatum_make_ok(datum_make_int((int64_t)&(d->bytestring_value)));
  } else if (!strcmp(des, "sizet")) {
    if (!datum_is_integer(d)) {
      return fdatum_make_panic("int expected, got something else");
    }
    return fdatum_make_ok(datum_make_int((int64_t)&(d->integer_value)));
  } else if (!strcmp(des, "datum")) {
    datum **p = malloc(sizeof(datum **));
    *p = d;
    return fdatum_make_ok(datum_make_int((int64_t)p));
  } else {
    return fdatum_make_panic("cannot load an argument");
  }
}

fdatum datum_deref(datum *arg) {
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
  if (!strcmp(rettype, "sizet")) {
    return fdatum_make_ok(datum_make_int((int64_t)*(size_t *)wha));
  }
  else if (!strcmp(rettype, "int")) {
    return fdatum_make_ok(datum_make_int((int64_t)*(int *)wha));
  }
  else if (!strcmp(rettype, "int64")) {
    return fdatum_make_ok(datum_make_int(*(int64_t *)wha));
  }
  else if (!strcmp(rettype, "string")) {
    return fdatum_make_ok(datum_make_bytestring((char *)wha));
  }
  else if (!strcmp(rettype, "val")) {
    return *(fdatum *)wha;
  } else {
    return fdatum_make_panic("unknown return type for deref");
  }
}

fdatum pointer_ffi_call(datum *f, ffi_cif *cif, void **cargs) {
  void (*fn_ptr)(void) = __extension__(void (*)(void))(datum_get_fnpointer_value(f));
  char *rettype = datum_get_fnpointer_descriptor(f)->list_tail->list_head->symbol_value;

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
  else if (!strcmp(rettype, "progslice")) {
    res = malloc(sizeof(prog_slice));
  }
  else if (!strcmp(rettype, "val")) {
    res = malloc(sizeof(fdatum));
  } else {
    return fdatum_make_panic("unknown return type for extern func");
  }
  ffi_call(cif, fn_ptr, res, cargs);
  return fdatum_make_ok(datum_make_int((int64_t)res));

}

fdatum pointer_call(datum *f, datum *args, bool datums) {
  if (!datum_is_fnpointer(f)) {
    return fdatum_make_panic("pointer_call expects a pointer");
  }
  ffi_cif cif;
  char *err = NULL;
  err = pointer_ffi_init_cif(f, &cif);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  int nargs = list_length(datum_get_fnpointer_descriptor(f)->list_head);
  void *cargs[32];
  err = pointer_ffi_serialize_args(args, cargs, nargs, datums);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  return pointer_ffi_call(f, &cif, cargs);
}

datum *datum_make_fnpointer(void *data, datum *signature) {
  return datum_make_list_2(datum_make_symbol("cptr"), datum_make_list_2(datum_make_int((int64_t)data), signature));
}

void *datum_get_fnpointer_value(datum *d) {
  if (!datum_is_fnpointer(d)) {
    fprintf(stderr, "Not a pointer!");
    exit(1);
  }
  return (void*)d->list_tail->list_head->list_head->integer_value;
}

datum *datum_get_fnpointer_descriptor(datum *d) {
  if (!datum_is_fnpointer(d)) {
    fprintf(stderr, "Not a pointer!");
    exit(1);
  }
  return d->list_tail->list_head->list_tail->list_head;
}

bool datum_is_fnpointer(datum *e) { return datum_is_list(e) && !datum_is_nil(e) && datum_is_the_symbol(e->list_head, "cptr"); }
