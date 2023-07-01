#pragma once

#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>

enum datum_type {
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_BLOB,
  DATUM_INTEGER,
};

typedef struct datum datum;

struct blob {
  void *begin;
  size_t length;
};

typedef struct blob blob;

struct array {
  datum *begin;
  size_t length;
};

typedef struct array array;

struct datum {
  enum datum_type type;
  union {
    array list_value;
    char *symbol_value;
    char *bytestring_value;
    blob blob_value;
    int64_t integer_value;
  };
};

struct result {
  datum type;
  datum value;
};

struct vec {
  array storage;
  size_t length;
};

typedef struct vec vec;
typedef struct result result;

typedef struct context context;
struct context {
  bool aborted;
  char error[1024];
};
typedef struct extension extension;
struct extension {
  void (*call)(extension *self, vec *sl, datum *stmt, int *i, datum *compdata,
               context *ctxt);
};
typedef struct lisp_extension lisp_extension;

struct lisp_extension {
  extension base;
  vec program;
  datum routine_;
  datum compdata;
  result (*runner)(vec *, datum *, datum, context *);
};

#define datum_make_list_of(...)                                                \
  datum_make_list_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),      \
                          (datum[]){__VA_ARGS__})

#define vec_make_of(...)                                                       \
  vec_make_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),             \
                   (datum[]){__VA_ARGS__})
