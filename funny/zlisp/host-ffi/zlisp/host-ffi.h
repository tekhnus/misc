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
#include <dlfcn.h>
#include <ffi.h>
fdatum routine_run_and_get_value_c_host_new_new(prog_slice sl,datum **r0d);
#define INTERFACE 0
#define EXPORT_INTERFACE 0
#define LOCAL_INTERFACE 0
#define EXPORT
#define LOCAL static
#define PUBLIC
#define PRIVATE
#define PROTECTED
