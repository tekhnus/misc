/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <zlisp/common.h>
fdatum builtin_tail(datum *list);
fdatum builtin_head(datum *list);
fdatum builtin_cons(datum *head,datum *tail);
fdatum builtin_add(datum *x,datum *y);
fdatum builtin_concat_bytestrings(datum *x,datum *y);
fdatum builtin_repr(datum *v);
fdatum builtin_panic(datum *arg_value);
fdatum builtin_is_constant(datum *arg_value);
fdatum builtin_annotate(datum *arg_value);
fdatum builtin_eq(datum *x,datum *y);
#define INTERFACE 0
