#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>

#if EXPORT_INTERFACE

enum datum_type {
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
  DATUM_FRAME,
};

struct datum {
  enum datum_type type;
  union {
    vec list_value;
    char *symbol_value;
    char *bytestring_value;
    int64_t integer_value;
    struct {
      frame frame_value;
    };
  };
};

enum read_result_type {
  READ_RESULT_OK,
  READ_RESULT_PANIC,
  READ_RESULT_EOF,
  READ_RESULT_RIGHT_PAREN,
  READ_RESULT_RIGHT_SQUARE,
  READ_RESULT_RIGHT_CURLY,
};

struct read_result {
  enum read_result_type type;
  union {
    struct datum ok_value;
    char *panic_message;
  };
};

struct fdatum {
  int type;
  struct datum ok_value;
  char *panic_message;
};

struct result {
  datum type;
  datum value;
};

struct vec {
  datum *begin;
  size_t length;
  size_t capacity;
};

struct frame {
  vec state;
  int type_id;
  int parent_type_id;
};

struct frame_view {
  vec *state;
  int type_id;
  int parent_type_id;
};

#endif
