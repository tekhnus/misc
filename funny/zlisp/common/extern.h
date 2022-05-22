/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdbool.h>
#include <stdint.h>
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
typedef struct prog_slice prog_slice;
typedef struct prog prog;
struct prog_slice {
  struct prog *begin;
  size_t length;
  size_t capacity;
};
typedef struct routine_1 routine_1;
LOCAL char *routine_1_step(prog_slice sl,routine_1 *r,fdatum(*perform_host_instruction)(datum *,datum *));
LOCAL char *datum_to_routine_1(routine_1 *res,prog_slice sl,datum *fns);
LOCAL char *datum_to_routine_0(routine_0 *res,prog_slice sl,datum *fn);
LOCAL datum *routine_1_to_datum(prog_slice sl,routine_1 r);
LOCAL datum *routine_0_to_datum(prog_slice sl,routine_0 r);
LOCAL routine_0 routine_1_pop_frame(routine_1 *r);
LOCAL void routine_1_push_frame(routine_1 *r,routine_0 sub);
typedef struct routine_2 routine_2;
LOCAL routine_1 routine_2_pop_frame(routine_2 *r);
LOCAL void routine_2_push_frame(routine_2 *r,routine_1 sub);
LOCAL char *routine_2_step(prog_slice sl,routine_2 *r,fdatum(*perform_host_instruction)(datum *,datum *));
LOCAL char *routine_2_run(prog_slice sl,routine_2 *r,fdatum(*perform_host_instruction)(datum *,datum *));
typedef struct state state;
fdatum routine_run_and_get_value(prog_slice sl,state **ctxt,prog *p,fdatum(*perform_host_instruction)(datum *,datum *));
char *prog_build_one(prog_slice *sl,prog *s,datum *stmt,fdatum(*module_source)(prog_slice *sl,prog *p,char *));
LOCAL char *prog_build_deps(prog_slice *sl,prog **p,datum *deps,fdatum(*module_source)(prog_slice *sl,prog *p,char *));
char *prog_build(prog_slice *sl,prog *entrypoint,datum *source,fdatum(*module_source)(prog_slice *sl,prog *p,char *));
void prog_append_put_prog(prog_slice *sl,prog **begin,prog *val,int capture);
void prog_append_call(prog_slice *sl,prog **begin,bool hat);
void prog_append_collect(prog_slice *sl,prog **begin);
void prog_append_args(prog_slice *sl,prog **begin);
LOCAL void prog_append_host(prog_slice *sl,prog **begin,datum *name);
LOCAL char *prog_append_backquoted_statement(prog_slice *sl,prog **begin,datum *stmt);
LOCAL void prog_append_return(prog_slice *sl,prog **begin,bool hat);
LOCAL void prog_append_import(prog_slice *sl,prog **begin);
LOCAL void prog_append_set_closures(prog_slice *sl,prog **begin,prog *p,datum *var,bool hat);
LOCAL char *prog_init_routine(prog_slice *sl,prog *s,datum *stmt);
LOCAL bool datum_is_the_symbol_pair(datum *d,char *val1,char *val2);
datum *datum_make_void();
LOCAL void prog_join(prog *a,prog *b,prog *e);
LOCAL void prog_append_put_var(prog_slice *sl,prog **begin,datum *val);
void prog_append_put_const(prog_slice *sl,prog **begin,datum *val);
LOCAL void prog_append_yield(prog_slice *sl,prog **begin,bool hat);
char *prog_append_statement(prog_slice *sl,prog **begin,datum *stmt);
void prog_append_pop(prog_slice *sl,prog **begin,datum *var);
LOCAL void prog_append_uncollect(prog_slice *sl,prog **begin);
fdatum prog_read_usages(datum *spec);
fdatum prog_init_submodule(prog_slice *sl,prog *s,datum *source);
datum *prog_to_offset(prog_slice sl,prog *p);
LOCAL datum *prog_to_datum(prog_slice sl,prog *p);
datum *prog_slice_to_datum(prog_slice sl);
size_t prog_slice_length(prog_slice s);
prog *prog_slice_at(prog_slice s,size_t index);
prog *prog_slice_append_new(prog_slice *s);
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
    struct prog *uncollect_next;
    struct {
      struct datum *pop_var;
      struct prog *pop_next;
    };
    struct {
      struct prog *put_prog_value;
      int put_prog_capture;
      struct prog *put_prog_next;
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
prog_slice prog_slice_make(size_t capacity);
datum *state_stack_collect(state **s);
void state_stack_new(state **s);
datum *state_stack_pop(state **s);
void state_stack_put(state **ns,datum *value);
bool datum_is_constant(datum *d);
bool datum_eq(datum *x,datum *y);
datum *state_list_vars(state *ns);
fdatum state_get_var(state *ns,datum *symbol);
void state_set_var(state **ns,datum *symbol,datum *value);
state *state_make_fresh();
state *state_make(datum *vars,datum *stack);
struct state {
  struct datum *vars;
  struct datum *stack;
};
char *fdatum_get_panic_message(fdatum result);
bool fdatum_is_panic(fdatum result);
char *datum_repr(datum *e);
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
LOCAL struct token token_read(FILE *strm);
LOCAL bool consume_control_sequence(char c,datum **form);
LOCAL bool is_allowed_inside_symbol(char c);
LOCAL bool is_whitespace(char c);
LOCAL read_result read_result_make_right_paren(void);
LOCAL read_result read_result_make_eof(void);
LOCAL read_result read_result_make_panic(char *message);
LOCAL read_result read_result_make_ok(datum *e);
bool read_result_is_right_paren(read_result x);
LOCAL bool read_result_is_eof(read_result x);
bool read_result_is_panic(read_result x);
bool read_result_is_ok(read_result x);
datum *datum_make_int(int64_t value);
datum *datum_make_bytestring(char *text);
datum *datum_make_symbol(char *name);
LOCAL datum *datum_make_list_5(datum *head,datum *second,datum *third,datum *fourth,datum *fifth);
LOCAL datum *datum_make_list_4(datum *head,datum *second,datum *third,datum *fourth);
LOCAL datum *datum_make_list_3(datum *head,datum *second,datum *third);
datum *datum_make_list_2(datum *head,datum *second);
datum *datum_make_list_1(datum *head);
datum *datum_make_list(datum *head,datum *tail);
datum *datum_make_nil();
bool datum_is_bytestring(datum *e);
bool datum_is_integer(datum *e);
bool datum_is_nil(datum *e);
bool datum_is_list(datum *e);
int list_length(datum *seq);
bool datum_is_symbol(datum *e);
enum datum_type {
  DATUM_NIL,
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
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
  };
};
bool datum_is_the_symbol(datum *d,char *val);
#define EXPORT
#define INTERFACE 0