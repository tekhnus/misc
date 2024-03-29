#pragma once

#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>

typedef uint8_t datum_type;

typedef struct datum datum;

struct blob {
  void *_begin;
  size_t _length;
};

typedef struct blob blob;

struct array {
  datum *_begin;
  size_t _length;
};

typedef struct array array;

struct datum {
  datum_type _type;
  array _list_value;
  blob _leaf_value;
};

struct result {
  datum type;
  datum value;
};

struct vec {
  array _storage;
  size_t _length;
};

typedef struct vec vec;
typedef struct result result;

typedef struct context context;
struct context {
  uint8_t aborted;
  char error[1024];
};
typedef struct extension extension;
struct extension {
  datum (*call)(extension *self, vec *sl, datum *stmt, int *i,
                datum *compdata, context *ctxt);
};
typedef struct lisp_extension lisp_extension;

struct lisp_extension {
  extension base;
  vec program;
  datum routine_;
  datum compdata;
  result (*runner)(vec *, datum *, datum, context *);
};

#define datum_make_list_of(...)                                 \
  datum_make_list_of_impl(sizeof((datum[]){__VA_ARGS__}) /      \
                              sizeof(datum),                    \
                          (datum[]){__VA_ARGS__})

#define vec_make_of(...)                                        \
  vec_make_of_impl(sizeof((datum[]){__VA_ARGS__}) /             \
                       sizeof(datum),                           \
                   (datum[]){__VA_ARGS__})
