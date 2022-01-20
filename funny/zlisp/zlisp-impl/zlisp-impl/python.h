/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdint.h>
#include <stdbool.h>
typedef struct datum datum;
bool datum_is_pointer(datum *e);
bool datum_is_routine(datum *e);
bool datum_is_void(datum *e);
bool datum_is_integer(datum *e);
bool datum_is_bytestring(datum *e);
bool datum_is_symbol(datum *e);
bool datum_is_list(datum *e);
bool datum_is_nil(datum *e);
#include <inttypes.h>
#include <stdio.h>
#include <ffi.h>
enum datum_type {
  DATUM_NIL,
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
  DATUM_ROUTINE,
  DATUM_ROUTINE_0,
  DATUM_ROUTINE_1,
  DATUM_POINTER,
  DATUM_VOID,
};
typedef enum datum_type datum_type;
typedef struct routine routine;
typedef struct prog prog;
typedef struct state state;
struct routine {
  struct prog *prog_;
  struct state *state_;
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
    struct routine routine_value;
    struct routine routine_0_value;
    struct routine routine_1_value;
    struct {
      void *pointer_value;
      struct datum *pointer_descriptor;
    };
  };
};
char *datum_to_python(datum *d);
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
  PROG_POP,
  PROG_SET_CLOSURES,
  PROG_RETURN,
  PROG_YIELD,
  PROG_IMPORT,
};
typedef enum prog_type prog_type;
struct prog {
  enum prog_type type;
  union {
    struct {
      struct prog *if_true;
      struct prog *if_false;
    };
    struct {
      struct prog *nop_next;
    };
    struct {
      struct datum *put_const_value;
      struct prog *put_const_next;
    };
    struct {
      struct datum *put_var_value;
      struct prog *put_var_next;
    };
    struct prog *args_next;
    struct {
      bool call_hat;
      struct prog *call_next;
    };
    struct {
      struct datum *host_instruction;
      struct prog *host_next;
    };
    struct prog *collect_next;
    struct {
      struct datum *pop_var;
      struct prog *pop_next;
    };
    struct {
      struct prog *set_closures_prog;
      struct datum *set_closures_name;
      bool set_closures_hat;
      struct prog *set_closures_next;
    };
    bool return_hat;
    struct {
      bool yield_hat;
      struct prog *yield_next;
    };
    struct prog *import_next;
  };
};
char *routine_to_python(routine c);
struct state {
  struct datum *vars;
  struct datum *stack;
  struct routine parent;
  struct routine hat_parent;
};
