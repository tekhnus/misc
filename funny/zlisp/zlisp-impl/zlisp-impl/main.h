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

typedef struct datum datum_t;
typedef struct read_result read_result_t;
typedef struct namespace namespace_t;
typedef struct val val_t;
typedef struct ctx ctx_t;
typedef struct state state_t;

enum datum_type {
  DATUM_NIL,
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
  DATUM_OPERATOR,
  DATUM_POINTER,
  DATUM_VOID,
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
    struct {
      state_t *operator_state;
      namespace_t *operator_context;
    };
    struct {
      void *pointer_value;
      datum_t *pointer_descriptor;
    };
  };
};

struct namespace {
  datum_t *vars;
  datum_t *stack;
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

enum val_type {
  VAL_OK,
  VAL_PANIC,
};

struct val {
  int type;
  datum_t *ok_value;
  char *panic_message;
};

enum ctx_type {
  CTX_OK,
  CTX_PANIC,
};

struct ctx {
  int type;
  namespace_t *ok_value;
  char *panic_message;
};

enum state_type {
  STATE_END,
  STATE_IF,
  STATE_NOP,
  STATE_PUT_CONST,
  STATE_PUT_VAR,
  STATE_ARGS,
  STATE_CALL,
  STATE_POP,
  STATE_CALL_SPECIAL,
  STATE_RETURN,
};

struct state {
  enum state_type type;
  union {
    struct {
      state_t *if_true;
      state_t *if_false;
    };
    struct {
      state_t *nop_next;
    };
    struct {
      datum_t *put_const_value;
      state_t *put_const_next;
    };
    struct {
      datum_t *put_var_value;
      state_t *put_var_next;
    };
    state_t *args_next;
    state_t *call_next;
    state_t *pop_next;
    struct {
      ctx_t (*call_special_func)(datum_t *, namespace_t *);
      state_t *call_special_next;
    };
  };
};

bool datum_is_nil(datum_t *e);

bool datum_is_list(datum_t *e);

bool datum_is_symbol(datum_t *e);

bool datum_is_integer(datum_t *e);

bool datum_is_bytestring(datum_t *e);

bool datum_is_operator(datum_t *e);

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

datum_t *datum_make_operator(state_t *s, namespace_t *lexical_bindings);

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

bool val_is_ok(val_t result);

bool val_is_panic(val_t result);

val_t val_make_ok(datum_t *v);

val_t val_make_panic(char *message);

bool ctx_is_ok(ctx_t result);

bool ctx_is_panic(ctx_t result);

ctx_t ctx_make_ok(namespace_t *v);

ctx_t ctx_make_panic(char *message);

namespace_t *namespace_make(datum_t *vars, datum_t *stack);

namespace_t *namespace_make_empty();

val_t namespace_get(namespace_t *ns, datum_t *symbol);

namespace_t *namespace_set(namespace_t *ns, datum_t *symbol, datum_t *value);

namespace_t *namespace_set_fn(namespace_t *ns, datum_t *symbol,
                              datum_t *value);

datum_t *namespace_list(namespace_t *ns);

namespace_t *namespace_put(namespace_t *ns, datum_t *value);

val_t namespace_peek(namespace_t *ns);

namespace_t *namespace_pop(namespace_t *ns);

ctx_t datum_eval(datum_t *e, namespace_t *ctxt);

ctx_t namespace_make_prelude();

ctx_t namespace_make_eval_file(char *filename);

extern unsigned char zlisp_impl_prelude_lisp[];

extern unsigned int zlisp_impl_prelude_lisp_len;
