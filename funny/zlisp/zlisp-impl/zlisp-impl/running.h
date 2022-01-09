/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdint.h>
#include <stdbool.h>
typedef struct datum datum;
bool datum_is_the_symbol(datum *d,char *val);
bool datum_is_pointer(datum *e);
typedef struct state state;
datum *state_list_vars(state *ns);
datum *datum_make_void();
datum *state_stack_collect(state **s);
typedef struct fdatum fdatum;
#include <inttypes.h>
#include <stdio.h>
#include <ffi.h>
struct fdatum {
  int type;
  struct datum *ok_value;
  char *panic_message;
};
fdatum pointer_call(datum *f,datum *args);
int list_length(datum *seq);
bool datum_is_list(datum *e);
#define LOCAL static
LOCAL fdatum builtin_ptr_dereference_and_cast(datum *ptpt,datum *new_descriptor);
void *simplified_dlopen(char *path);
datum *datum_make_symbol(char *name);
datum *datum_make_list_1(datum *head);
datum *datum_make_list_2(datum *head,datum *second);
LOCAL fdatum builtin_ptr_not_null_pointer(datum *pointer);
datum *datum_make_pointer(void *data,datum *signature);
bool datum_is_bytestring(datum *e);
typedef struct routine routine;
typedef struct prog prog;
struct routine {
  struct prog *prog_;
  struct state *state_;
};
state *state_change_parent(state *ns,routine new_parent,bool hat);
routine state_get_parent(state *ns,bool hat);
LOCAL void switch_context(routine *c,routine b,datum *v);
void state_stack_new(state **s);
state *state_set_fn(state *ns,datum *symbol,datum *value);
state *state_set_var(state *ns,datum *symbol,datum *value);
bool fdatum_is_panic(fdatum result);
fdatum state_get_var(state *ns,datum *symbol);
datum *datum_make_routine(prog *s,state *lexical_bindings);
routine routine_make_null();
datum *datum_make_nil();
state *state_make(datum *vars,datum *stack,routine parent,routine hat_parent);
bool datum_is_routine(datum *e);
void state_stack_put(state **ns,datum *value);
bool datum_is_nil(datum *e);
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
typedef enum datum_type datum_type;
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
    struct {
      void *pointer_value;
      struct datum *pointer_descriptor;
    };
  };
};
typedef struct fstate fstate;
struct fstate {
  int type;
  struct state *ok_value;
  char *panic_message;
};
fstate fstate_make_ok(state *v);
fstate fstate_make_panic(char *message);
bool routine_is_null(routine r);
datum *state_stack_pop(state **s);
fdatum fdatum_make_ok(datum *v);
fdatum fdatum_make_panic(char *message);
bool fstate_is_panic(fstate result);
LOCAL fstate routine_run(routine c);
routine routine_make(prog *s,state *ctxt);
enum prog_type {
  PROG_END,
  PROG_IF,
  PROG_NOP,
  PROG_PUT_CONST,
  PROG_PUT_ROUTINE,
  PROG_PUT_VAR,
  PROG_ARGS,
  PROG_CALL,
  PROG_HOST,
  PROG_COLLECT,
  PROG_POP,
  PROG_POP_PROG,
  PROG_RETURN,
  PROG_YIELD,
  PROG_MODULE_END,
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
      struct datum *put_routine_value;
      struct prog *put_routine_next;
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
      struct datum *pop_prog_var;
      struct prog *pop_prog_next;
    };
    bool return_hat;
    struct {
      bool yield_hat;
      struct prog *yield_next;
    };
  };
};
struct state {
  struct datum *vars;
  struct datum *stack;
  struct routine parent;
  struct routine hat_parent;
};
fdatum routine_run_and_get_value(state **ctxt,prog *p);
