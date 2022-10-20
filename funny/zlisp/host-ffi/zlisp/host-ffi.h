#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
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
#include <unistd.h>
char *prog_build_c_host(prog_slice *sl,size_t *p,size_t *bp,datum *source,datum **compdata,datum **builder_compdata);
fdatum module_routine(char *module);
fdatum module_source(char *module);
char *module_to_filename(char *module);
fdatum file_source(char *fname);
#include <dlfcn.h>
#include <ffi.h>
void *simplified_dlopen(char *path);
void *simplified_dlsym(void *handle,const char *symbol);
fdatum perform_host_instruction(datum *name,datum *arg);
fdatum routine_run_and_get_value_c_host_new(prog_slice sl,datum **r0d);
bool ffi_type_init(ffi_type **type,datum *definition);
char *pointer_ffi_init_cif(datum *sig,ffi_cif *cif);
char *pointer_ffi_serialize_args(datum *args,void **cargs,int nargs,bool datums);
fdatum datum_mkptr(datum *arg);
fdatum datum_deref(datum *arg);
void *allocate_space_for_return_value(datum *sig);
fdatum pointer_call(datum *fpt,datum *sig,datum *args,bool datums);
void(*datum_to_function_pointer(datum *d))(void);
#define INTERFACE 0
#define EXPORT_INTERFACE 0
#define LOCAL_INTERFACE 0
#define EXPORT
#define LOCAL static
#define PUBLIC
#define PRIVATE
#define PROTECTED
