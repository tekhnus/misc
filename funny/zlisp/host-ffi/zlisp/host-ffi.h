#include <zlisp-impl/zlisp-impl.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
fdatum builtin_eq(datum *x,datum *y);
fdatum builtin_annotate(datum *arg_value);
fdatum builtin_is_constant(datum *arg_value);
fdatum builtin_panic(datum *arg_value);
fdatum builtin_repr(datum *v);
fdatum builtin_concat_bytestrings(datum *x,datum *y);
fdatum builtin_add(datum *x,datum *y);
fdatum builtin_cons(datum *head,datum *tail);
fdatum builtin_head(datum *list);
fdatum builtin_tail(datum *list);
state *state_make_builtins();
#include <unistd.h>
char *prog_init_module_c_host(prog_slice *sl,prog *p,datum *source);
char *prog_init_submodule_c_host(prog_slice *sl,prog *p,datum *source);
char *module_routine(prog_slice *sl,prog *p,char *module);
fdatum module_source(char *module);
char *module_to_filename(char *module);
fdatum file_source(char *fname);
#include <dlfcn.h>
#include <ffi.h>
void *simplified_dlopen(char *path);
void *simplified_dlsym(void *handle,const char *symbol);
fdatum perform_host_instruction(datum *name,datum *arg);
fdatum routine_run_and_get_value_c_host(state **ctxt,prog *p);
bool ffi_type_init(ffi_type **type,datum *definition);
char *pointer_ffi_init_cif(datum *f,ffi_cif *cif);
char *pointer_ffi_serialize_args(datum *args,void **cargs,int nargs,bool datums);
fdatum datum_mkptr(datum *arg);
fdatum datum_deref(datum *arg);
fdatum pointer_ffi_call(datum *f,ffi_cif *cif,void **cargs);
fdatum pointer_call(datum *f,datum *args,bool datums);
datum *datum_make_fnpointer(void *data,datum *signature);
void *datum_get_fnpointer_value(datum *d);
datum *datum_get_fnpointer_descriptor(datum *d);
bool datum_is_fnpointer(datum *e);
#define INTERFACE 0
#define EXPORT_INTERFACE 0
#define LOCAL_INTERFACE 0
#define EXPORT
#define LOCAL static
#define PUBLIC
#define PRIVATE
#define PROTECTED
