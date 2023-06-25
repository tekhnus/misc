/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <dlfcn.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <zlisp/common.h>
#define LOCAL static
LOCAL void(*datum_to_function_pointer(datum *d))(void);
LOCAL void *allocate_space_for_return_value(datum *sig);
LOCAL char *pointer_ffi_serialize_args(datum *args,void **cargs,int nargs);
LOCAL char *pointer_ffi_init_cif(datum *sig,ffi_cif *cif,ffi_type **arg_types,ffi_type **ret_type);
LOCAL bool ffi_type_init(ffi_type **type,datum *definition);
LOCAL void init_standard_types();
extern ffi_type *ffi_type_vec_elements[4];
extern ffi_type ffi_type_vec;
#include <stdint.h>
datum builtin_eq(datum *args,context *ctxt);
datum builtin_cons(datum *args,context *ctxt);
datum builtin_tail(datum *args,context *ctxt);
datum builtin_head(datum *args,context *ctxt);
LOCAL datum pointer_call(datum *argz,context *ctxt);
LOCAL datum datum_mkptr(datum *args,context *ctxt);
LOCAL datum datum_deref(datum *args,context *ctxt);
LOCAL datum host_ffi(datum *type,datum *args,context *ctxt);
result host_ffi_run(vec *sl,datum *r0d,datum args,context *ctxt);
datum *routine_run_in_ffi_host(vec *sl,datum *r0d,context *ctxt);
#define EXPORT
#define INTERFACE 0
