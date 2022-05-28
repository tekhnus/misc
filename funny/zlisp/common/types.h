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
  struct datum *stack;
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

enum prog_type {
  PROG_END,
  PROG_IF,
  PROG_NOP,
  PROG_PUT_CONST,
  PROG_PUT_VAR,
  PROG_ARGS,
  PROG_CALL,
  PROG_HOST,
  PROG_COLLECT,
  PROG_UNCOLLECT,
  PROG_POP,
  PROG_SET_CLOSURES,
  PROG_PUT_PROG,
  PROG_RETURN,
  PROG_YIELD,
  PROG_IMPORT,
};

struct prog {
  enum prog_type type;
  union {
    struct {
      ptrdiff_t if_true;
      ptrdiff_t if_false;
    };
    struct {
      ptrdiff_t nop_next;
    };
    struct {
      struct datum *put_const_value;
      ptrdiff_t put_const_next;
    };
    struct {
      struct datum *put_var_value;
      ptrdiff_t put_var_next;
    };
    ptrdiff_t args_next;
    struct {
      bool call_hat;
      ptrdiff_t call_next;
    };
    struct {
      struct datum *host_instruction;
      ptrdiff_t host_next;
    };
    ptrdiff_t collect_next;
    ptrdiff_t uncollect_next;
    struct {
      struct datum *pop_var;
      ptrdiff_t pop_next;
    };
    struct {
      ptrdiff_t put_prog_value;
      int put_prog_capture;
      ptrdiff_t put_prog_next;
    };
    struct {
      ptrdiff_t set_closures_prog;
      struct datum *set_closures_name;
      bool set_closures_hat;
      ptrdiff_t set_closures_next;
    };
    bool return_hat;
    struct {
      bool yield_hat;
      ptrdiff_t yield_next;
    };
    ptrdiff_t import_next;
  };
};

struct prog_slice {
  struct prog *begin;
  size_t length;
  size_t capacity;
};

#endif
