// zlisp interpreter headers.
#include <ctype.h>
#include <dlfcn.h>
#include <ffi.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct read_result read_result_t;
typedef struct datum datum_t;
typedef struct fdatum fdatum_t;
typedef struct fstate fstate_t;
typedef struct prog prog_t;
typedef struct state state_t;
typedef struct routine routine_t;

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
  prog_t *prog;
  state_t *state;
};

struct datum {
  enum datum_type type;
  union {
    struct {
      datum_t *list_head;
      datum_t *list_tail;
    };
    char *symbol_value;
    char *bytestring_value;
    int64_t integer_value;
    routine_t routine_value;
    struct {
      void *pointer_value;
      datum_t *pointer_descriptor;
    };
  };
};

struct state {
  datum_t *vars;
  datum_t *stack;
  routine_t parent;
  routine_t hat_parent;
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
    datum_t *ok_value;
    char *panic_message;
  };
};

enum fdatum_type {
  FDATUM_OK,
  FDATUM_PANIC,
};

struct fdatum {
  int type;
  datum_t *ok_value;
  char *panic_message;
};

enum fstate_type {
  FSTATE_OK,
  FSTATE_PANIC,
};

struct fstate {
  int type;
  state_t *ok_value;
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
      prog_t *if_true;
      prog_t *if_false;
    };
    struct {
      prog_t *nop_next;
    };
    struct {
      datum_t *put_const_value;
      prog_t *put_const_next;
    };
    struct {
      datum_t *put_routine_value;
      prog_t *put_routine_next;
    };
    struct {
      datum_t *put_var_value;
      prog_t *put_var_next;
    };
    prog_t *args_next;
    struct {
      bool call_hat;
      prog_t *call_next;
    };
    struct {
      datum_t *pop_var;
      prog_t *pop_next;
    };
    struct {
      datum_t *pop_prog_var;
      prog_t *pop_prog_next;
    };
    bool return_hat;
    struct {
      bool yield_hat;
      prog_t *yield_next;
    };
  };
};

bool datum_is_nil(datum_t *e);

bool datum_is_list(datum_t *e);

bool datum_is_symbol(datum_t *e);

bool datum_is_integer(datum_t *e);

bool datum_is_bytestring(datum_t *e);

bool datum_is_routine(datum_t *e);

bool datum_is_pointer(datum_t *e);

bool datum_is_void(datum_t *e);

datum_t *datum_make_nil();

datum_t *datum_make_list(datum_t *head, datum_t *tail);

datum_t *datum_make_list_1(datum_t *head);

datum_t *datum_make_list_2(datum_t *head, datum_t *second);

datum_t *datum_make_list_3(datum_t *head, datum_t *second, datum_t *third);

datum_t *datum_make_symbol(char *name);

datum_t *datum_make_bytestring(char *text);

datum_t *datum_make_int(int64_t value);

datum_t *datum_make_routine(prog_t *s, state_t *lexical_bindings);

datum_t *datum_make_pointer(void *data, datum_t *signature);

datum_t *datum_make_pointer_to_pointer(void **ptr);

datum_t *datum_make_void();

bool read_result_is_ok(read_result_t x);

bool read_result_is_panic(read_result_t x);

bool read_result_is_eof(read_result_t x);

bool read_result_is_right_paren(read_result_t x);

read_result_t read_result_make_ok(datum_t *e);

read_result_t read_result_make_panic(char *message);

read_result_t read_result_make_eof(void);

read_result_t read_result_make_right_paren(void);

read_result_t datum_read(FILE *strm);

char *datum_repr(datum_t *e);

bool fdatum_is_ok(fdatum_t result);

bool fdatum_is_panic(fdatum_t result);

fdatum_t fdatum_make_ok(datum_t *v);

fdatum_t fdatum_make_panic(char *message);

bool fstate_is_ok(fstate_t result);

bool fstate_is_panic(fstate_t result);

fstate_t fstate_make_ok(state_t *v);

fstate_t fstate_make_panic(char *message);

state_t *state_make(datum_t *vars, datum_t *stack, routine_t parent, routine_t hat_parent);

state_t *state_make_fresh();

fdatum_t state_get_var(state_t *ns, datum_t *symbol);

state_t *state_set_var(state_t *ns, datum_t *symbol, datum_t *value);

state_t *state_set_fn(state_t *ns, datum_t *symbol, datum_t *value);

datum_t *state_list_vars(state_t *ns);

char* state_value_eval(state_t **ctxt, datum_t *v, fdatum_t (*module_source)(char *module));

void state_value_put(state_t **ctxt, datum_t *v);

datum_t *state_value_pop(state_t **ctxt);

state_t *state_make_builtins();

routine_t routine_make(prog_t *s, state_t *ctxt);

prog_t *prog_make();

char *prog_init_module(prog_t *s, datum_t *source, fdatum_t (*module_source)(char *module));

fstate_t routine_run(routine_t c);

fdatum_t list_map(fdatum_t (*fn)(datum_t *, state_t *), datum_t *items,
                  state_t *ctxt);
