/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#define LOCAL static
typedef struct datum datum;
LOCAL datum *list_cut(datum *xs,size_t rest_length);
void state_stack_put(datum **ns,datum *value);
datum *state_stack_pop(datum **s);
datum *state_stack_collect(datum **s,size_t count);
typedef struct fdatum fdatum;
#include <inttypes.h>
#include <stdio.h>
struct fdatum {
  int type;
  struct datum *ok_value;
  char *panic_message;
};
fdatum state_stack_at(datum *ns,int offset);
void state_stack_put_all(datum **ns,datum *list);
datum *state_stack_top(datum **s);
typedef struct prog_slice prog_slice;
struct prog_slice {
  datum *begin;
  size_t length;
  size_t capacity;
};
typedef struct routine routine;
LOCAL fdatum routine_run(prog_slice sl,routine *r,datum *args);
void print_backtrace_new(prog_slice sl,routine *r);
typedef struct prog prog;
LOCAL prog datum_to_prog(datum *d);
LOCAL routine *topmost_routine(routine *r);
LOCAL char *datum_to_routine(datum *d,routine *r);
fdatum routine_run_new(prog_slice sl,datum **r0d,fdatum(*perform_host_instruction)(datum *,datum *));
LOCAL datum *routine_to_datum(routine *r);
datum *routine_make_new(ptrdiff_t prg);
LOCAL void prog_append_uncollect(prog_slice *sl,size_t count,size_t *begin,datum **compdata);
LOCAL void prog_append_collect(prog_slice *sl,size_t count,size_t *begin,datum **compdata);
LOCAL fdatum prog_read_exports(datum *spec);
LOCAL void prog_append_recieve(prog_slice *sl,size_t *begin,datum *args,datum *meta,datum **compdata);
LOCAL fdatum prog_read_usages(datum *spec);
LOCAL char *prog_append_backquoted_statement(prog_slice *sl,size_t *begin,datum *stmt,datum **compdata);
LOCAL bool datum_is_the_symbol_pair(datum *d,char *val1,char *val2);
void prog_append_resolve(prog_slice *sl,size_t *begin);
LOCAL char *prog_init_routine(prog_slice *sl,size_t s,datum *args,datum *stmt,datum **routine_compdata,datum *info);
LOCAL void prog_join(prog_slice *sl,size_t a,size_t b,size_t e);
LOCAL char *prog_append_exports(prog_slice *sl,size_t *begin,datum *spec,datum **compdata);
LOCAL char *prog_append_usages(prog_slice *sl,size_t *begin,datum *spec,datum **compdata);
LOCAL void prog_append_put_const(prog_slice *sl,size_t *begin,datum *val,datum **compdata);
LOCAL char *prog_append_statement(prog_slice *sl,size_t *begin,datum *stmt,datum **compdata,datum *info);
LOCAL void compdata_validate(datum *compdata);
bool compdata_has_value(datum *compdata);
datum *compdata_make();
void prog_append_yield(prog_slice *sl,size_t *begin,datum *type,size_t count,size_t recieve_count,datum *meta,datum **compdata);
int prog_append_put_prog(prog_slice *sl,size_t *begin,size_t val,int capture,datum **compdata);
void prog_append_pop(prog_slice *sl,size_t *begin,size_t idx,datum **compdata);
void compdata_give_names(datum *var,datum **compdata);
LOCAL int compdata_get_top_index(datum *compdata);
void prog_append_nop(prog_slice *sl,size_t *begin,datum *info);
LOCAL int compdata_get_index(datum *compdata,datum *var);
int prog_append_put_var(prog_slice *sl,size_t *begin,datum *val,datum **compdata);
LOCAL datum *compdata_put(datum *compdata,datum *var);
LOCAL datum *compdata_del(datum *compdata,int index);
void prog_append_call(prog_slice *sl,size_t *begin,int fn_index,datum *type,int arg_count,int return_count,datum **compdata);
LOCAL char *prog_append_statements(prog_slice *sl,size_t *off,datum *source,datum **compdata,datum *info);
fdatum prog_compile(datum *source,datum **compdata,datum *info);
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
int list_index_of(datum *xs,datum *x);
datum *list_chop_last(datum *list);
datum *list_append(datum *list,datum *value);
datum *prog_slice_to_datum(prog_slice sl);
size_t prog_slice_length(prog_slice s);
datum *prog_slice_datum_at(prog_slice s,size_t index);
void prog_slice_extend(prog_slice *s,datum *instructions);
size_t prog_slice_append_new(prog_slice *s);
prog_slice prog_slice_make(size_t capacity);
bool datum_is_constant(datum *d);
bool datum_eq(datum *x,datum *y);
fdatum fdatum_get_panic_message(datum *args);
fdatum fdatum_repr_datum_pointer(datum *args);
datum *list_at(datum *list,unsigned index);
fdatum fdatum_get_value(datum *args);
fdatum fdatum_make_panic(char *message);
fdatum fdatum_make_ok(datum *v);
bool fdatum_is_panic(fdatum result);
char *datum_repr_bounded(datum *e,size_t depth);
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
datum *list_tail(datum *list);
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
