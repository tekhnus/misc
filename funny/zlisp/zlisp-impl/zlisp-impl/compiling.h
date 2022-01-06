/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
typedef struct prog prog;
void prog_append_call(prog **begin,int hat);
void prog_append_collect(prog **begin);
void prog_append_args(prog **begin);
typedef struct datum datum;
typedef struct fdatum fdatum;
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <ffi.h>
struct fdatum {
  int type;
  struct datum *ok_value;
  char *panic_message;
};
char *prog_append_backquoted_statement(prog **begin,datum *stmt,fdatum(*module_source)(char *module));
void prog_append_yield(prog **begin,bool hat);
void prog_append_return(prog **begin,bool hat);
bool datum_is_the_symbol_pair(datum *d,char *val1,char *val2);
char *prog_append_require(prog **begin,datum *src,fdatum(*module_source)(char *module));
bool fdatum_is_panic(fdatum result);
bool datum_is_bytestring(datum *e);
void prog_append_put_routine(prog **begin,datum *val);
void prog_append_pop_prog(prog **begin,datum *var);
typedef struct state state;
datum *datum_make_routine(prog *s,state *lexical_bindings);
char *prog_init_routine(prog *s,datum *stmt,fdatum(*module_source)(char *module));
void prog_append_pop(prog **begin,datum *var);
datum *datum_make_void();
void prog_join(prog *a,prog *b,prog *e);
int list_length(datum *seq);
bool datum_is_the_symbol(datum *d,char *val);
bool datum_is_list(datum *e);
void prog_append_put_var(prog **begin,datum *val);
bool datum_is_symbol(datum *e);
void prog_append_put_const(prog **begin,datum *val);
bool datum_is_constant(datum *d);
void prog_append_module_end(prog **begin);
char *prog_append_statement(prog **begin,datum *stmt,fdatum(*module_source)(char *module));
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
typedef struct routine routine;
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
    struct {
      void *pointer_value;
      struct datum *pointer_descriptor;
    };
  };
};
char *prog_init_module(prog *s,datum *source,fdatum(*module_source)(char *module));
prog *prog_make();
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
