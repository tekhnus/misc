/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
datum *datum_make_fnpointer(void *data,datum *signature);
bool datum_is_fnpointer(datum *e);
void *datum_get_fnpointer_value(datum *d);
fdatum pointer_ffi_call(datum *f,ffi_cif *cif,void **cargs);
char *pointer_ffi_serialize_args(datum *args,void **cargs,int nargs,bool datums);
datum *datum_get_fnpointer_descriptor(datum *d);
char *pointer_ffi_init_cif(datum *f,ffi_cif *cif);
bool ffi_type_init(ffi_type **type,datum *definition);
fdatum routine_run_and_get_value_c_host(prog_slice sl,state **ctxt,prog *p);
#include <stdint.h>
fdatum builtin_eq(datum *x,datum *y);
fdatum builtin_cons(datum *head,datum *tail);
fdatum builtin_tail(datum *list);
fdatum builtin_head(datum *list);
fdatum builtin_panic(datum *arg_value);
fdatum datum_mkptr(datum *arg);
fdatum datum_deref(datum *arg);
fdatum pointer_call(datum *f,datum *args,bool datums);
fdatum perform_host_instruction(datum *name,datum *arg);
void *simplified_dlsym(void *handle,const char *symbol);
void *simplified_dlopen(char *path);
#define INTERFACE 0
