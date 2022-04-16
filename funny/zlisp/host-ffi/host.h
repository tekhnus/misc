/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp-impl/zlisp-impl.h>
#include <string.h>
#include <dlfcn.h>
#include <stdlib.h>
#include <ffi.h>
datum *datum_make_pointer_to_pointer(void **ptr);
fdatum pointer_ffi_call(datum *f,ffi_cif *cif,void **cargs);
char *pointer_ffi_serialize_args(datum *f,datum *args,void **cargs);
char *pointer_ffi_init_cif(datum *f,ffi_cif *cif);
bool ffi_type_init(ffi_type **type,datum *definition);
fdatum routine_run_and_get_value_c_host(state **ctxt,prog *p);
fdatum pointer_call(datum *f,datum *args);
#include <stdint.h>
fdatum builtin_tail(datum *list);
fdatum builtin_head(datum *list);
fdatum builtin_panic(datum *arg_value);
fdatum perform_host_instruction(datum *name,datum *arg);
fdatum builtin_ptr_wrap_ptr_into_ptr(datum *pt);
datum *datum_make_pointer(void *data,datum *signature);
datum *datum_get_pointer_descriptor(datum *d);
#define LOCAL static
LOCAL fdatum builtin_ptr_dereference_and_cast(datum *ptpt,datum *new_descriptor);
void *datum_get_pointer_value(datum *d);
bool datum_is_pointer(datum *e);
LOCAL fdatum builtin_ptr_not_null_pointer(datum *pointer);
void *simplified_dlsym(void *handle,const char *symbol);
void *simplified_dlopen(char *path);
#define INTERFACE 0
