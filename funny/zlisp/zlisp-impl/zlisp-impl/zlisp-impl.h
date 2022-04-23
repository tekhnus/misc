/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdint.h>
#include <stdbool.h>
#define LOCAL static
typedef struct routine_0 routine_0;
typedef struct fdatum fdatum;
#include <inttypes.h>
#include <stdio.h>
typedef struct datum datum;
struct fdatum {
  int type;
  struct datum *ok_value;
  char *panic_message;
};
LOCAL char *routine_0_step(routine_0 *r,fdatum(*perform_host_instruction)(datum *,datum *));
typedef struct routine_1 routine_1;
LOCAL char *routine_1_step(routine_1 *r,fdatum(*perform_host_instruction)(datum *,datum *));
typedef struct prog prog;
typedef struct state state;
struct routine_0 {
  struct prog *prog_;
  struct state *state_;
};
struct routine_1 {
  struct routine_0 cur;
  struct routine_1 *par;
};
LOCAL routine_1 routine_1_deep_copy(routine_1 r);
LOCAL routine_0 routine_1_pop_frame(routine_1 *r);
LOCAL void routine_1_push_frame(routine_1 *r,routine_0 sub);
typedef struct routine_2 routine_2;
LOCAL routine_1 routine_2_pop_frame(routine_2 *r);
LOCAL void routine_2_push_frame(routine_2 *r,routine_1 sub);
LOCAL char *routine_2_step(routine_2 *r,fdatum(*perform_host_instruction)(datum *,datum *));
LOCAL char *routine_2_run(routine_2 *r,fdatum(*perform_host_instruction)(datum *,datum *));
fdatum routine_run_and_get_value(state **ctxt,prog *p,fdatum(*perform_host_instruction)(datum *,datum *));
typedef struct prog_slice prog_slice;
LOCAL void prog_append_import(prog_slice *sl,prog **begin);
LOCAL void prog_append_call(prog_slice *sl,prog **begin,bool hat);
LOCAL void prog_append_collect(prog_slice *sl,prog **begin);
LOCAL void prog_append_args(prog_slice *sl,prog **begin);
LOCAL void prog_append_host(prog_slice *sl,prog **begin,datum *name);
LOCAL char *prog_append_backquoted_statement(prog_slice *sl,prog **begin,datum *stmt,char *(*module_source)(prog_slice *sl,prog *p,char *));
LOCAL void prog_append_return(prog_slice *sl,prog **begin,bool hat);
LOCAL char *prog_append_require(prog_slice *sl,prog **begin,char *pkg,char *(*module_source)(prog_slice *sl,prog *p,char *));
LOCAL void prog_append_set_closures(prog_slice *sl,prog **begin,prog *p,datum *var,bool hat);
LOCAL char *prog_init_routine(prog_slice *sl,prog *s,datum *stmt,char *(*module_source)(prog_slice *sl,prog *p,char *));
LOCAL bool datum_is_the_symbol_pair(datum *d,char *val1,char *val2);
LOCAL void prog_join(prog *a,prog *b,prog *e);
LOCAL void prog_append_put_var(prog_slice *sl,prog **begin,datum *val);
LOCAL void prog_append_yield(prog_slice *sl,prog **begin,bool hat);
char *prog_init_submodule(prog_slice *sl,prog *s,datum *source,char *(*module_source)(prog_slice *sl,prog *p,char *));
LOCAL char *prog_append_statement(prog_slice *sl,prog **begin,datum *stmt,char *(*module_source)(prog_slice *sl,prog *p,char *));
LOCAL void prog_append_pop(prog_slice *sl,prog **begin,datum *var);
LOCAL void prog_append_put_const(prog_slice *sl,prog **begin,datum *val);
char *prog_init_module(prog_slice *sl,prog *s,datum *source,char *(*module_source)(prog_slice *sl,prog *p,char *));
prog *prog_make();
struct prog_slice {
  struct prog *begin;
  size_t length;
  size_t capacity;
};
size_t prog_slice_length(prog_slice s);
prog *prog_slice_last(prog_slice s);
prog *prog_slice_at(prog_slice s,size_t index);
prog *prog_slice_append_new(prog_slice *s);
prog_slice prog_slice_make(size_t capacity);
datum *state_stack_collect(state **s);
void state_stack_new(state **s);
datum *state_stack_pop(state **s);
void state_stack_put(state **ns,datum *value);
bool datum_is_constant(datum *d);
bool datum_eq(datum *x,datum *y);
datum *state_list_vars(state *ns);
fdatum list_map(fdatum(*fn)(datum *,state *),datum *items,state *ctxt);
fdatum state_get_var(state *ns,datum *symbol);
void state_set_var(state **ns,datum *symbol,datum *value);
state *state_make_fresh();
LOCAL state *state_make(datum *vars,datum *stack);
char *fdatum_get_panic_message(fdatum result);
fdatum fdatum_get_value(fdatum result);
bool fdatum_is_panic(fdatum result);
bool fdatum_is_ok(fdatum result);
char *datum_repr(datum *e);
fdatum datum_read_all(FILE *stre);
fdatum fdatum_make_ok(datum *v);
fdatum fdatum_make_panic(char *message);
fdatum datum_read_one(FILE *stre);
typedef struct read_result read_result;
enum read_result_type {
  READ_RESULT_OK,
  READ_RESULT_PANIC,
  READ_RESULT_EOF,
  READ_RESULT_RIGHT_PAREN,
};
typedef enum read_result_type read_result_type;
struct read_result {
  enum read_result_type type;
  union {
    struct datum *ok_value;
    char *panic_message;
  };
};
read_result datum_read(FILE *strm);
struct token token_read(FILE *strm);
bool consume_control_sequence(char c,datum **form);
bool is_allowed_inside_symbol(char c);
bool is_whitespace(char c);
read_result read_result_make_right_paren(void);
read_result read_result_make_eof(void);
read_result read_result_make_panic(char *message);
read_result read_result_make_ok(datum *e);
bool read_result_is_right_paren(read_result x);
bool read_result_is_eof(read_result x);
bool read_result_is_panic(read_result x);
bool read_result_is_ok(read_result x);
datum *datum_make_void();
datum *datum_make_routine_1(routine_1 r);
datum *datum_make_routine_0(routine_0 r);
datum *datum_make_int(int64_t value);
datum *datum_make_bytestring(char *text);
datum *datum_make_symbol(char *name);
datum *datum_make_list_3(datum *head,datum *second,datum *third);
datum *datum_make_list_2(datum *head,datum *second);
datum *datum_make_list_1(datum *head);
datum *datum_make_list(datum *head,datum *tail);
datum *datum_make_nil();
bool datum_is_void(datum *e);
bool datum_is_routine_1(datum *e);
bool datum_is_routine_0(datum *e);
bool datum_is_bytestring(datum *e);
bool datum_is_integer(datum *e);
bool datum_is_nil(datum *e);
bool datum_is_list(datum *e);
int list_length(datum *seq);
struct routine_2 {
  struct routine_1 cur;
  struct routine_2 *par;
};
bool routine_2_is_null(routine_2 r);
bool routine_1_is_null(routine_1 r);
bool routine_0_is_null(routine_0 r);
routine_2 routine_2_make_null();
routine_1 routine_1_make_null();
routine_0 routine_0_make_null();
struct state {
  struct datum *vars;
  struct datum *stack;
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
routine_0 routine_0_make(prog *s,state *ctxt);
bool datum_is_symbol(datum *e);
enum datum_type {
  DATUM_NIL,
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
  DATUM_ROUTINE_0,
  DATUM_ROUTINE_1,
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
    struct routine_0 routine_0_value;
    struct routine_1 routine_1_value;
    struct {
      void *pointer_value;
      struct datum *pointer_descriptor;
    };
  };
};
bool datum_is_the_symbol(datum *d,char *val);
#define INTERFACE 0
