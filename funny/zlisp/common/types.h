#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>

#if EXPORT_INTERFACE
enum datum_type {
  DATUM_NIL,
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
};

struct datum {
  enum datum_type type;
  union {
    struct {
      struct datum *list_head;
      struct datum *list_tail;
    };
    char *symbol_value;
    char *bytestring_value;
    int64_t integer_value;
  };
};

struct state {
  struct datum *vars;
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
  struct datum *ok_value;
  char *panic_message;
};

struct prog_slice {
  datum *begin;
  size_t length;
  size_t capacity;
};

#endif
