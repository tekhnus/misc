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
fdatum builtin_eq(datum *args);
fdatum builtin_cons(datum *args);
fdatum builtin_tail(datum *args);
fdatum builtin_head(datum *args);
fdatum builtin_panic(datum *args);
LOCAL fdatum datum_mkptr(datum *args);
LOCAL fdatum datum_deref(datum *args);
LOCAL fdatum pointer_call(datum *fpt,datum *sig,datum *args,bool datums);
LOCAL fdatum perform_host_instruction(datum *name,datum *args);
fdatum routine_run_and_get_value_c_host_new_new(prog_slice sl,datum **r0d);
#define EXPORT
#define INTERFACE 0
