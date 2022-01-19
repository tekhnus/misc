/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdint.h>
#include <stdbool.h>
typedef struct datum datum;
typedef struct state state;
datum *state_list_vars(state *ns);
int list_length(datum *seq);
datum *state_stack_collect(state **s);
void state_stack_new(state **s);
state *state_set_var(state *ns,datum *symbol,datum *value);
typedef struct fdatum fdatum;
#include <inttypes.h>
#include <stdio.h>
#include <ffi.h>
struct fdatum {
  int type;
  struct datum *ok_value;
  char *panic_message;
};
bool fdatum_is_panic(fdatum result);
fdatum state_get_var(state *ns,datum *symbol);
#define LOCAL static
typedef struct fstate fstate;
struct fstate {
  int type;
  struct state *ok_value;
  char *panic_message;
};
typedef struct prog prog;
LOCAL fstate routine_0_step(prog **p,state *s,fdatum(*perform_host_instruction)(datum *,datum *));
LOCAL fstate routine_1_step(prog **p,state *s,fdatum(*perform_host_instruction)(datum *,datum *));
datum *datum_make_list_2(datum *head,datum *second);
datum *datum_make_routine(prog *s,state *lexical_bindings);
typedef struct routine routine;
struct routine {
  struct prog *prog_;
  struct state *state_;
};
routine routine_make_null();
bool routine_is_null(routine r);
state *state_set_fn(state *ns,datum *symbol,datum *value);
state *state_change_parent(state *ns,routine new_parent,bool hat);
void state_stack_put(state **ns,datum *value);
routine routine_make(prog *s,state *ctxt);
bool datum_is_routine(datum *e);
fstate fstate_make_panic(char *message);
bool datum_is_nil(datum *e);
bool datum_is_list(datum *e);
LOCAL fstate routine_2_step(prog **p,state *s,fdatum(*perform_host_instruction)(datum *,datum *));
fstate fstate_make_ok(state *v);
datum *state_stack_pop(state **s);
fdatum fdatum_make_ok(datum *v);
fdatum fdatum_make_panic(char *message);
bool fstate_is_panic(fstate result);
LOCAL fstate routine_2_run(prog *p,state *s,fdatum(*perform_host_instruction)(datum *,datum *));
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
  PROG_POP_PROG,
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
      struct datum *pop_prog_var;
      bool pop_prog_hat;
      struct prog *pop_prog_next;
    };
    bool return_hat;
    struct {
      bool yield_hat;
      struct prog *yield_next;
    };
    struct prog *import_next;
  };
};
struct state {
  struct datum *vars;
  struct datum *stack;
  struct routine parent;
  struct routine hat_parent;
};
fdatum routine_run_and_get_value(state **ctxt,prog *p,fdatum(*perform_host_instruction)(datum *,datum *));
