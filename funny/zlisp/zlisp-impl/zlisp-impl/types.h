#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <ffi.h>

typedef struct prog prog;
typedef struct state state;
typedef struct datum datum;
typedef struct routine routine;

enum datum_type {
  DATUM_NIL,
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
  DATUM_ROUTINE,
  DATUM_POINTER,
  DATUM_VOID,
};

struct routine {
  prog *prog_;
  state *state_;
};

struct datum {
  enum datum_type type;
  union {
    struct {
      datum *list_head;
      datum *list_tail;
    };
    char *symbol_value;
    char *bytestring_value;
    int64_t integer_value;
    routine routine_value;
    struct {
      void *pointer_value;
      datum *pointer_descriptor;
    };
  };
};

struct state {
  datum *vars;
  datum *stack;
  routine parent;
  routine hat_parent;
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
    datum *ok_value;
    char *panic_message;
  };
};

struct fdatum {
  int type;
  datum *ok_value;
  char *panic_message;
};

struct fstate {
  int type;
  state *ok_value;
  char *panic_message;
};

enum prog_type {
  PROG_END,
  PROG_IF,
  PROG_NOP,
  PROG_PUT_CONST,
  PROG_PUT_ROUTINE,
  PROG_PUT_VAR,
  PROG_ARGS,
  PROG_CALL,
  PROG_COLLECT,
  PROG_POP,
  PROG_POP_PROG,
  PROG_RETURN,
  PROG_YIELD,
  PROG_MODULE_END,
};

struct prog {
  enum prog_type type;
  union {
    struct {
      prog *if_true;
      prog *if_false;
    };
    struct {
      prog *nop_next;
    };
    struct {
      datum *put_const_value;
      prog *put_const_next;
    };
    struct {
      datum *put_routine_value;
      prog *put_routine_next;
    };
    struct {
      datum *put_var_value;
      prog *put_var_next;
    };
    struct prog *args_next;
    struct {
      bool call_hat;
      prog *call_next;
    };
    struct prog *collect_next;
    struct {
      datum *pop_var;
      prog *pop_next;
    };
    struct {
      datum *pop_prog_var;
      prog *pop_prog_next;
    };
    bool return_hat;
    struct {
      bool yield_hat;
      prog *yield_next;
    };
  };
};
