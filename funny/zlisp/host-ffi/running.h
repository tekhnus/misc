/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
#define LOCAL static
LOCAL void(*datum_to_function_pointer(datum *d))(void);
LOCAL void *allocate_space_for_return_value(datum *sig);
LOCAL char *pointer_ffi_serialize_args(datum *args,void **cargs,int nargs,bool datums);
LOCAL char *pointer_ffi_init_cif(datum *sig,ffi_cif *cif);
LOCAL bool ffi_type_init(ffi_type **type,datum *definition);
LOCAL void *simplified_dlsym(void *handle,const char *symbol);
LOCAL void *simplified_dlopen(char *path);
#include <stdint.h>
fdatum builtin_eq(datum *x,datum *y);
fdatum builtin_cons(datum *head,datum *tail);
fdatum builtin_tail(datum *list);
fdatum builtin_head(datum *list);
fdatum builtin_panic(datum *arg_value);
LOCAL fdatum datum_mkptr(datum *arg);
LOCAL fdatum datum_deref(datum *arg);
LOCAL fdatum pointer_call(datum *fpt,datum *sig,datum *args,bool datums);
fdatum routine_run_and_get_value_c_host_new_new(prog_slice sl,datum **r0d);
LOCAL fdatum perform_host_instruction(datum *name,datum *arg);
fdatum routine_run_and_get_value_c_host_new(prog_slice sl,datum **r0d);
#define EXPORT
#define INTERFACE 0
