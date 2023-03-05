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
      routine *frame_pointers;
    };
  };
};

enum read_result_type {
  READ_RESULT_OK,
  READ_RESULT_PANIC,
  READ_RESULT_EOF,
  READ_RESULT_RIGHT_PAREN,
};

struct read_result {
  enum read_result_type type;
  union {
    struct datum *ok_value;
    char *panic_message;
  };
};

struct fdatum {
  int type;
  struct datum ok_value;
  char *panic_message;
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

struct routine {
  struct frame *frames[10];
  size_t cnt;
};

#endif
