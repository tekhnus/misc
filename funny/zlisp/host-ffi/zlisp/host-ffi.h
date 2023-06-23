#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
datum builtin_eq(datum *args,context *ctxt);
datum builtin_annotate(datum *args,context *ctxt);
datum builtin_is_constant(datum *args,context *ctxt);
datum builtin_repr(datum *args,context *ctxt);
datum builtin_concat_bytestrings(datum *args,context *ctxt);
datum builtin_add(datum *args,context *ctxt);
datum builtin_cons(datum *args,context *ctxt);
datum builtin_head(datum *args,context *ctxt);
datum builtin_tail(datum *args,context *ctxt);
#include <dlfcn.h>
#include <ffi.h>
#include <assert.h>
fdatum routine_run_in_ffi_host(vec sl,datum *r0d);
result host_ffi_run(vec sl,datum *r0d,datum args);
extern ffi_type ffi_type_fdatum;
extern ffi_type *ffi_type_fdatum_elements[4];
extern ffi_type ffi_type_vec;
extern ffi_type *ffi_type_vec_elements[4];
#define INTERFACE 0
#define EXPORT_INTERFACE 0
#define LOCAL_INTERFACE 0
#define EXPORT
#define LOCAL static
#define PUBLIC
#define PRIVATE
#define PROTECTED
