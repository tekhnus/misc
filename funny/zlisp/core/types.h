#pragma once

#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>

enum datum_type {
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
};

typedef struct datum datum;

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
