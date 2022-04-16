/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <zlisp-impl/zlisp-impl.h>
#include <string.h>
#include <dlfcn.h>
#include <stdlib.h>
fdatum routine_run_and_get_value_c_host(state **ctxt,prog *p);
fdatum perform_host_instruction(datum *name,datum *arg);
fdatum builtin_ptr_wrap_ptr_into_ptr(datum *pt);
#define LOCAL static
LOCAL fdatum builtin_ptr_dereference_and_cast(datum *ptpt,datum *new_descriptor);
LOCAL fdatum builtin_ptr_not_null_pointer(datum *pointer);
void *simplified_dlopen(char *path);
#define INTERFACE 0
