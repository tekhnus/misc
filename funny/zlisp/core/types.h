#pragma once

#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>

typedef uint8_t datum_type;

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
  uint8_t moved;
  datum_type _type;
  array _list_value;
  blob _leaf_value;
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
  uint8_t aborted;
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

#define soft_assert(cond) (true || cond ? fprintf(stderr, "") : fprintf(stderr, "warning: %s %d\n", __FILE__, __LINE__))

#define own(var) (soft_assert((var).moved), (var).moved = 0)

#define move(var) (soft_assert(!(var).moved), (var).moved = 1, (var))

#define borrow(var) (soft_assert(!(var).moved), &(var))
