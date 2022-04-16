#include <host.h>
#if INTERFACE
#include <zlisp-impl/zlisp-impl.h>
#include <string.h>
#include <dlfcn.h>
#include <stdlib.h>
#endif

void *simplified_dlopen(char *path) {
  if (strlen(path) == 0) {
    return RTLD_DEFAULT;
  }
  return dlopen(path, RTLD_LAZY);
}

LOCAL fdatum builtin_ptr_not_null_pointer(datum *pointer) {
  if (!datum_is_pointer(pointer)) {
    return fdatum_make_panic("not-null-pointer expects a pointer");
  }
  if (pointer->pointer_value != NULL) {
    return fdatum_make_ok(datum_make_list_1(datum_make_nil()));
  }
  return fdatum_make_ok(datum_make_nil());
}

LOCAL fdatum builtin_ptr_dereference_and_cast(datum *ptpt, datum *new_descriptor) {
  if (!datum_is_pointer(ptpt) || !datum_is_the_symbol(ptpt->pointer_descriptor, "pointer")) {
    return fdatum_make_panic("dereference expected a pointer to pointer");
  }
  return fdatum_make_ok(datum_make_pointer(*((void **)ptpt->pointer_value), new_descriptor));
}

fdatum builtin_ptr_wrap_ptr_into_ptr(datum *pt) {
  if (!datum_is_pointer(pt)) {
    return fdatum_make_panic("wrap-ptr-into-ptr expected a pointer");
  }
  void **ptpt = malloc(sizeof(void **));
  *ptpt = pt->pointer_value;
  return fdatum_make_ok(datum_make_pointer(ptpt, datum_make_symbol("pointer")));
}

fdatum perform_host_instruction(datum *name, datum *arg) {
  if (!datum_is_bytestring(name)) {
    return fdatum_make_panic("host instruction should be a string");
  }
  datum *res;
  if (!strcmp(name->bytestring_value, "wrap-pointer-into-pointer")) {
    res = datum_make_pointer((void *)builtin_ptr_wrap_ptr_into_ptr, datum_make_list_2(datum_make_list_1(datum_make_symbol("datum")), datum_make_symbol("val")));
  } else if (!strcmp(name->bytestring_value, "not-null-pointer")) {
    res = datum_make_pointer((void *)builtin_ptr_not_null_pointer, datum_make_list_2(datum_make_list_1(datum_make_symbol("datum")), datum_make_symbol("val")));
  } else if (!strcmp(name->bytestring_value, "dlopen")) {
    res = datum_make_pointer((void *)simplified_dlopen, datum_make_list_2(datum_make_list_1(datum_make_symbol("string")), datum_make_symbol("pointer")));
  } else if (!strcmp(name->bytestring_value, "dlsym")) {
    res = datum_make_pointer((void *)dlsym, datum_make_list_2(datum_make_list_2(datum_make_symbol("pointer"), datum_make_symbol("string")), datum_make_symbol("pointer")));
  } else if (!strcmp(name->bytestring_value, "dereference-and-cast")) {
    res = datum_make_pointer((void *)builtin_ptr_dereference_and_cast, datum_make_list_2(datum_make_list_2(datum_make_symbol("datum"), datum_make_symbol("datum")), datum_make_symbol("val")));
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
  } else {
    return fdatum_make_panic("unknown host instruction");
  }
  return fdatum_make_ok(res);
}

fdatum routine_run_and_get_value_c_host(state **ctxt, prog *p) {
  return routine_run_and_get_value(ctxt, p, perform_host_instruction);
}
