/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
void(*datum_to_function_pointer(datum *d))(void);
void *allocate_space_for_return_value(datum *sig);
char *pointer_ffi_serialize_args(datum *args,void **cargs,int nargs,bool datums);
char *pointer_ffi_init_cif(datum *sig,ffi_cif *cif);
bool ffi_type_init(ffi_type **type,datum *definition);
fdatum routine_run_and_get_value_c_host(prog_slice sl,datum **ctxt,size_t prg);
#include <stdint.h>
fdatum builtin_eq(datum *x,datum *y);
fdatum builtin_cons(datum *head,datum *tail);
fdatum builtin_tail(datum *list);
fdatum builtin_head(datum *list);
fdatum builtin_panic(datum *arg_value);
fdatum datum_mkptr(datum *arg);
fdatum datum_deref(datum *arg);
fdatum pointer_call(datum *fpt,datum *sig,datum *args,bool datums);
fdatum perform_host_instruction(datum *name,datum *arg);
void *simplified_dlsym(void *handle,const char *symbol);
void *simplified_dlopen(char *path);
#define INTERFACE 0
