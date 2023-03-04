/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdarg.h>
#define LOCAL static
typedef struct datum datum;
typedef struct routine routine;
LOCAL datum *datum_make_frame(routine *r);
LOCAL routine *routine_make_empty(ptrdiff_t prg,routine *context);
typedef struct vec vec;
LOCAL void vec_copy(vec *dst,vec *src);
typedef struct frame frame;
LOCAL void frame_copy(frame *dst,frame *src);
LOCAL void routine_copy(routine *dst,routine *src);
LOCAL size_t routine_get_stack_size(routine *r);
#include <inttypes.h>
#include <stdio.h>
enum datum_type {
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
  DATUM_FRAME,
};
typedef enum datum_type datum_type;
struct vec {
  datum *begin;
  size_t length;
  size_t capacity;
};
struct datum {
  enum datum_type type;
  union {
    vec list_value;
    char *symbol_value;
    char *bytestring_value;
    int64_t integer_value;
    void *frame_value;
  };
};
void state_stack_put(routine *r,datum value);
datum *routine_make(ptrdiff_t prg,routine *context);
LOCAL datum state_stack_collect(routine *r,size_t count);
void state_stack_put_all(routine *r,datum list);
LOCAL datum state_stack_pop(routine *r);
LOCAL size_t routine_get_count(routine *r);
LOCAL ptrdiff_t *routine_offset(routine *r);
typedef struct prog prog;
LOCAL prog datum_to_prog(datum *d);
LOCAL routine *routine_merge(routine *r,routine *rt_tail);
datum *state_stack_at(routine *r,datum *offset);
LOCAL routine *routine_get_prefix(routine *r,size_t capture_count);
LOCAL routine *make_routine_from_indices(routine *r,size_t capture_count,datum *call_indices);
LOCAL void print_backtrace(vec sl,routine *r);
LOCAL datum *routine_get_shape(routine *r);
LOCAL routine *get_child(vec sl,routine *r);
typedef struct fdatum fdatum;
struct fdatum {
  int type;
  struct datum ok_value;
  char *panic_message;
};
LOCAL fdatum routine_run(vec sl,routine *r,datum args);
LOCAL routine *get_routine_from_datum(datum *d);
fdatum routine_run_with_handler(vec sl,datum *r0d,fdatum(*yield_handler)(datum *,datum *));
LOCAL datum *list_copy_and_append(datum *list,datum *value);
datum *datum_copy(datum *d);
LOCAL void compdata_validate(datum *compdata);
bool compdata_has_value(datum *compdata);
datum *compdata_make();
LOCAL void prog_append_collect(vec *sl,size_t count,size_t *begin,datum **compdata);
void prog_append_nop(vec *sl,size_t *begin);
LOCAL fdatum prog_read_exports(datum *spec);
LOCAL void prog_append_recieve(vec *sl,size_t *begin,datum *args,datum *meta,datum **compdata);
LOCAL fdatum prog_read_usages(datum *spec);
void prog_append_call(vec *sl,size_t *begin,datum *indices,bool pop_one,datum *type,int arg_count,int return_count,datum **compdata);
datum *compdata_get_top_polyindex(datum *compdata);
datum *compdata_get_polyindex(datum *compdata,datum *var);
datum *compdata_get_shape(datum *compdata);
LOCAL char *prog_append_backquoted_statement(vec *sl,size_t *begin,datum *stmt,datum **compdata);
void prog_append_put_prog(vec *sl,size_t *begin,size_t val,int capture,datum **compdata);
LOCAL char *prog_init_routine(vec *sl,size_t s,datum *args,datum *stmt,datum **routine_compdata);
LOCAL datum *compdata_start_new_section(datum *compdata);
void compdata_give_names(datum *var,datum **compdata);
LOCAL void prog_join(vec *sl,size_t a,size_t b,size_t e);
LOCAL datum *compdata_put(datum *compdata,datum *var);
LOCAL datum *compdata_del(datum *compdata);
LOCAL char *prog_append_exports(vec *sl,size_t *begin,datum *spec,datum **compdata);
LOCAL char *prog_append_usages(vec *sl,size_t *begin,datum *spec,datum **compdata);
void prog_append_put_var(vec *sl,size_t *begin,datum *val,datum **compdata);
LOCAL void prog_append_put_const(vec *sl,size_t *begin,datum *val,datum **compdata);
LOCAL char *prog_append_statement(vec *sl,size_t *begin,datum *stmt,datum **compdata);
void prog_append_yield(vec *sl,size_t *begin,datum *type,size_t count,size_t recieve_count,datum *meta,datum **compdata);
LOCAL char *prog_append_statements(vec *sl,size_t *off,datum *source,datum **compdata,bool skip_first_debug);
fdatum prog_compile(datum *source,datum **compdata);
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
datum list_pop(datum *list);
datum *list_get_tail(datum *list);
datum *list_get_last(datum *list);
bool datum_is_nil(datum *e);
datum *datum_make_list(datum *head,datum *tail);
datum vec_pop(vec *v);
void list_append(datum *list,datum *value);
datum *datum_make_nil();
datum *vec_to_datum(vec *sl);
size_t vec_length(vec *s);
datum *vec_at(vec *s,size_t index);
void vec_extend(vec *s,datum *instructions);
size_t vec_append_new(vec *s);
size_t vec_append(vec *s,datum x);
vec vec_make(size_t capacity);
bool datum_is_the_symbol(datum *d,char *val);
bool datum_is_constant(datum *d);
bool datum_eq(datum *x,datum *y);
fdatum fdatum_get_panic_message(datum *args);
fdatum fdatum_repr_datum_pointer(datum *args);
datum *datum_make_list_of(size_t count,...);
fdatum fdatum_get_value(datum *args);
fdatum fdatum_make_panic(char *message);
fdatum fdatum_make_ok(datum v);
bool fdatum_is_panic(fdatum result);
datum *list_at(datum *list,unsigned index);
int list_length(datum *seq);
bool datum_is_list(datum *e);
char *datum_repr_bounded(datum *e,size_t depth);
char *datum_repr(datum *e);
datum *datum_make_int(int64_t value);
datum *datum_make_bytestring(char *text);
datum *datum_make_symbol(char *name);
bool datum_is_frame(datum *e);
bool datum_is_bytestring(datum *e);
bool datum_is_integer(datum *e);
bool datum_is_symbol(datum *e);
#define EXPORT
#define INTERFACE 0
