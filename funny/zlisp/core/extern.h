/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>
#define LOCAL static
typedef struct routine routine;
LOCAL void routine_copy(routine *dst,routine *src);
LOCAL size_t routine_get_stack_size(routine *r);
typedef struct datum datum;
LOCAL datum *datum_copy(datum *d);
void state_stack_put(routine *r,datum *value);
LOCAL size_t routine_get_count(routine *r);
datum *state_stack_collect(routine *r,size_t count);
void state_stack_put_all(routine *r,datum *list);
datum *state_stack_pop(routine *r);
LOCAL routine *routine_merge(routine *r,routine *rt_tail);
datum *state_stack_at_poly(routine *r,datum *offset);
LOCAL ptrdiff_t *routine_offset(routine *r);
typedef struct prog prog;
LOCAL prog datum_to_prog(datum *d);
LOCAL datum *routine_get_shape(routine *r);
typedef struct prog_slice prog_slice;
#include <inttypes.h>
#include <stdio.h>
struct prog_slice {
  datum *begin;
  size_t length;
  size_t capacity;
};
LOCAL routine *get_child(prog_slice sl,routine *r);
LOCAL void print_backtrace(prog_slice sl,routine *r);
typedef struct fdatum fdatum;
struct fdatum {
  int type;
  struct datum *ok_value;
  char *panic_message;
};
LOCAL fdatum routine_run(prog_slice sl,routine *r,datum *args);
LOCAL routine *get_routine_from_datum(datum *d);
fdatum routine_run_with_handler(prog_slice sl,datum **r0d,fdatum(*yield_handler)(datum *,datum *));
LOCAL datum *datum_make_frame(routine *r);
LOCAL routine *routine_make_empty(ptrdiff_t prg);
datum *routine_make(ptrdiff_t prg);
datum *compdata_get_shape(datum *compdata);
int compdata_get_top_index(datum *compdata);
LOCAL void compdata_validate(datum *compdata);
bool compdata_has_value(datum *compdata);
datum *compdata_make();
LOCAL void prog_append_collect(prog_slice *sl,size_t count,size_t *begin,datum **compdata);
LOCAL fdatum prog_read_exports(datum *spec);
LOCAL void prog_append_recieve(prog_slice *sl,size_t *begin,datum *args,datum *meta,datum **compdata);
LOCAL fdatum prog_read_usages(datum *spec);
void prog_append_call(prog_slice *sl,size_t *begin,datum *fn_index,datum *subfn_index,bool pop_one,datum *type,int arg_count,int return_count,datum **compdata);
datum *compdata_get_top_polyindex(datum *compdata);
datum *compdata_get_polyindex(datum *compdata,datum *var);
LOCAL char *prog_append_backquoted_statement(prog_slice *sl,size_t *begin,datum *stmt,datum **compdata);
void prog_append_put_prog(prog_slice *sl,size_t *begin,size_t val,int capture,datum **compdata);
LOCAL char *prog_init_routine(prog_slice *sl,size_t s,datum *args,datum *stmt,datum **routine_compdata,datum *info);
LOCAL datum *compdata_start_new_section(datum *compdata);
void compdata_give_names(datum *var,datum **compdata);
LOCAL void prog_join(prog_slice *sl,size_t a,size_t b,size_t e);
LOCAL datum *compdata_put(datum *compdata,datum *var);
LOCAL datum *compdata_del(datum *compdata);
LOCAL char *prog_append_exports(prog_slice *sl,size_t *begin,datum *spec,datum **compdata);
LOCAL char *prog_append_usages(prog_slice *sl,size_t *begin,datum *spec,datum **compdata);
void prog_append_put_var(prog_slice *sl,size_t *begin,datum *val,datum **compdata);
void prog_append_yield(prog_slice *sl,size_t *begin,datum *type,size_t count,size_t recieve_count,datum *meta,datum **compdata);
LOCAL void prog_append_put_const(prog_slice *sl,size_t *begin,datum *val,datum **compdata);
LOCAL char *prog_append_statement(prog_slice *sl,size_t *begin,datum *stmt,datum **compdata,datum *info);
void prog_append_nop(prog_slice *sl,size_t *begin,datum *info);
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
int list_length(datum *seq);
datum *prog_slice_to_datum(prog_slice sl);
size_t prog_slice_length(prog_slice s);
datum *prog_slice_datum_at(prog_slice s,size_t index);
void prog_slice_extend(prog_slice *s,datum *instructions);
size_t prog_slice_append_new(prog_slice *s);
prog_slice prog_slice_make(size_t capacity);
bool datum_is_the_symbol(datum *d,char *val);
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
datum *list_append(datum *list,datum *value);
datum *datum_make_list_of(size_t count,...);
datum *list_tail(datum *list);
datum *datum_make_list(datum *head,datum *tail);
datum *datum_make_nil();
bool datum_is_frame(datum *e);
bool datum_is_bytestring(datum *e);
bool datum_is_integer(datum *e);
bool datum_is_symbol(datum *e);
bool datum_is_list(datum *e);
enum datum_type {
  DATUM_NIL,
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
  DATUM_FRAME,
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
    void *frame_value;
  };
};
bool datum_is_nil(datum *e);
#define EXPORT
#define INTERFACE 0
