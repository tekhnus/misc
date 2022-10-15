/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#define LOCAL static
typedef struct prog_slice prog_slice;
#include <inttypes.h>
#include <stdio.h>
typedef struct datum datum;
struct prog_slice {
  datum *begin;
  size_t length;
  size_t capacity;
};
typedef struct routine_0 routine_0;
typedef struct fdatum fdatum;
struct fdatum {
  int type;
  struct datum *ok_value;
  char *panic_message;
};
LOCAL char *routine_0_step(prog_slice sl,routine_0 *r,fdatum(*perform_host_instruction)(datum *,datum *));
typedef struct routine_1 routine_1;
LOCAL char *routine_1_step(prog_slice sl,routine_1 *r,fdatum(*perform_host_instruction)(datum *,datum *));
LOCAL char *datum_to_routine_1(routine_1 *res,prog_slice sl,datum *fns);
LOCAL char *datum_to_routine_0(routine_0 *res,datum *fn);
LOCAL datum *routine_1_to_datum(prog_slice sl,routine_1 r);
LOCAL datum *routine_0_to_datum(routine_0 r);
LOCAL routine_0 routine_1_pop_frame(routine_1 *r);
LOCAL void routine_1_push_frame(routine_1 *r,routine_0 sub);
typedef struct routine_2 routine_2;
LOCAL routine_1 routine_2_pop_frame(routine_2 *r);
LOCAL void routine_2_push_frame(routine_2 *r,routine_1 sub);
LOCAL char *routine_2_step(prog_slice sl,routine_2 *r,fdatum(*perform_host_instruction)(datum *,datum *));
typedef struct prog prog;
LOCAL prog datum_to_prog(datum *d);
void print_backtrace(prog_slice sl,routine_2 *r);
LOCAL char *routine_2_run(prog_slice sl,routine_2 *r,fdatum(*perform_host_instruction)(datum *,datum *));
typedef struct state state;
fdatum routine_run_and_get_value(prog_slice sl,state **ctxt,ptrdiff_t prg,fdatum(*perform_host_instruction)(datum *,datum *));
LOCAL datum *list_append(datum *x,datum *y);
LOCAL char *get_varname(datum *dep_and_sym);
struct state {
  struct datum *vars;
};
LOCAL char *prog_build_dep(datum **state,prog_slice *sl,size_t *p,datum *dep_and_sym,fdatum(*module_source)(prog_slice *sl,size_t *p,char *),datum **compdata);
LOCAL void prog_put_deps(prog_slice *sl,size_t *p,datum *deps,datum **compdata);
LOCAL char *prog_build_deps(datum **state,prog_slice *sl,size_t *p,datum *deps,fdatum(*module_source)(prog_slice *sl,size_t *p,char *),datum **compdata);
char *prog_build_one(prog_slice *sl,size_t ep,datum *stmt_or_spec,fdatum(*module_source)(prog_slice *sl,size_t *p,char *),datum **compdata);
LOCAL char *prog_build_deps_isolated(prog_slice *sl,size_t *p,datum *deps,fdatum(*module_source)(prog_slice *sl,size_t *p,char *),datum **compdata);
char *prog_build(prog_slice *sl,size_t ep,datum *source,fdatum(*module_source)(prog_slice *sl,size_t *p,char *),datum **compdata);
datum *datum_make_void();
datum *compdata_make();
LOCAL int compdata_get_index(datum *compdata,datum *var);
void prog_append_call(prog_slice *sl,size_t *begin,bool hat,int return_count,datum **compdata);
void prog_append_collect(prog_slice *sl,size_t count,size_t *begin,datum **compdata);
LOCAL void prog_append_host(prog_slice *sl,size_t *begin,datum *name);
LOCAL char *prog_append_backquoted_statement(prog_slice *sl,size_t *begin,datum *stmt,datum **compdata);
void prog_append_yield(prog_slice *sl,size_t *begin,bool hat,size_t count,size_t recieve_count,datum *meta,datum **compdata);
void prog_append_put_prog(prog_slice *sl,size_t *begin,size_t val,int capture,datum **compdata);
LOCAL void prog_append_set_closures(prog_slice *sl,size_t *begin,size_t p,bool hat);
LOCAL char *prog_init_routine(prog_slice *sl,size_t s,datum *stmt,datum **compdata,datum *info);
LOCAL bool datum_is_the_symbol_pair(datum *d,char *val1,char *val2);
void prog_append_pop(prog_slice *sl,size_t *begin,datum *var,datum **compdata);
LOCAL void prog_join(prog_slice *sl,size_t a,size_t b,size_t e);
LOCAL datum *compdata_del(datum *compdata);
void prog_append_put_var(prog_slice *sl,size_t *begin,datum *val,datum **compdata);
void prog_append_put_const(prog_slice *sl,size_t *begin,datum *val,datum **compdata);
LOCAL fdatum prog_read_exports(datum *spec);
void prog_append_recieve(prog_slice *sl,size_t *begin,datum *args,datum **compdata);
LOCAL datum *compdata_put(datum *compdata,datum *var);
LOCAL fdatum prog_read_usages(datum *spec);
LOCAL char *prog_append_statement(prog_slice *sl,size_t *begin,datum *stmt,datum **compdata,datum *info);
void prog_append_nop(prog_slice *sl,size_t *begin,datum *info);
LOCAL fdatum prog_append_exports(prog_slice *sl,size_t *begin,datum *spec,datum **compdata);
LOCAL fdatum prog_append_usages(prog_slice *sl,size_t *begin,datum *spec,datum **compdata);
fdatum prog_init_submodule(prog_slice *sl,size_t *off,datum *source,datum **compdata,datum *info);
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
datum *prog_slice_to_datum(prog_slice sl);
size_t prog_slice_length(prog_slice s);
datum *prog_slice_datum_at(prog_slice s,size_t index);
size_t prog_slice_append_new(prog_slice *s);
prog_slice prog_slice_make(size_t capacity);
datum *state_stack_collect(state **s,size_t count);
datum *state_stack_top(state **s);
datum *state_stack_pop(state **s);
void state_stack_put_all(state **ns,datum *list);
void state_stack_put(state **ns,datum *value);
datum *list_at(datum *list,unsigned index);
fdatum state_stack_at(state *ns,int offset);
bool datum_is_constant(datum *d);
bool datum_eq(datum *x,datum *y);
state *state_make_fresh();
state *state_make(datum *vars);
char *fdatum_get_panic_message(fdatum result);
fdatum fdatum_make_panic(char *message);
fdatum fdatum_make_ok(datum *v);
bool fdatum_is_panic(fdatum result);
char *datum_repr(datum *e);
datum *datum_make_int(int64_t value);
datum *datum_make_bytestring(char *text);
datum *datum_make_symbol(char *name);
datum *datum_make_list_6(datum *head,datum *second,datum *third,datum *fourth,datum *fifth,datum *sixth);
datum *datum_make_list_5(datum *head,datum *second,datum *third,datum *fourth,datum *fifth);
datum *datum_make_list_4(datum *head,datum *second,datum *third,datum *fourth);
datum *datum_make_list_3(datum *head,datum *second,datum *third);
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
