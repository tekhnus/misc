/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#define LOCAL static
typedef struct datum datum;
#include <inttypes.h>
#include <stdio.h>
enum datum_type {
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
};
typedef enum datum_type datum_type;
typedef struct array array;
struct array {
  datum *begin;
  size_t length;
};
struct datum {
  enum datum_type type;
  union {
    array list_value;
    char *symbol_value;
    char *bytestring_value;
    int64_t integer_value;
  };
};
typedef struct context context;
struct context {
  bool aborted;
  char error[1024];
};
LOCAL datum prog_read_exports(datum *spec,context *ctxt);
LOCAL datum prog_read_usages(datum *spec,context *ctxt);
typedef struct vec vec;
typedef struct extension extension;
struct extension {
  void (*call)(extension *self, vec *sl, datum *stmt, int *i, datum *compdata,
               context *ctxt);
};
LOCAL void prog_append_exports(vec *sl,datum *spec,datum *compdata,extension *ext,context *ctxt);
LOCAL void prog_append_usages(vec *sl,datum *spec,datum *compdata,extension *ext,context *ctxt);
typedef struct lisp_extension lisp_extension;
struct vec {
  array storage;
  size_t length;
};
typedef struct result result;
struct result {
  datum type;
  datum value;
};
struct lisp_extension {
  extension base;
  vec program;
  datum routine_;
  datum compdata;
  result (*runner)(vec *, datum *, datum, context *); 
};
LOCAL datum lisp_extension_run(datum *e,lisp_extension *est,context *ctxt);
LOCAL void null_extension_call(extension *self,vec *sl,datum *source,int *i,datum *compdata,context *ctxt);
extension null_extension_make();
LOCAL void lisp_extension_call(extension *self_,vec *sl,datum *source,int *i,datum *compdata,context *ctxt);
lisp_extension lisp_extension_make(vec program,datum routine_,datum compdata,result(*runner)(vec *,datum *,datum,context *));
LOCAL struct frame get_frame_from_datum(datum *d);
typedef struct routine routine;
datum *routine_make_alloc(ptrdiff_t prg,routine *context);
LOCAL datum datum_make_frame(vec state,int type_id,int parent_type_id);
LOCAL void routine_merge(routine *r,routine *rt_tail);
LOCAL routine routine_get_prefix(routine *r,size_t capture_count);
LOCAL datum *state_stack_at(routine *r,datum *offset);
LOCAL bool state_stack_has(routine *r,datum *offset);
LOCAL datum state_stack_invalidate(routine *r,datum polyindex);
LOCAL void state_stack_set(routine *r,datum *target,datum value);
datum routine_make(ptrdiff_t prg,routine *context);
LOCAL datum state_stack_invalidate_many(routine *r,size_t count,datum top_polyindex);
LOCAL void state_stack_set_many(routine *r,datum idx,datum list);
LOCAL size_t routine_get_count(routine *r);
LOCAL routine make_routine_from_indices(routine *r,size_t capture_count,datum *call_indices,context *ctxt);
LOCAL void print_frame(vec *sl,routine *r);
LOCAL ptrdiff_t *routine_offset(routine *r);
typedef struct prog prog;
LOCAL prog datum_to_prog(datum *d,context *ctxt);
LOCAL result routine_run_impl(vec *sl,routine *r,datum args,context *ctxt);
LOCAL routine get_routine_from_datum(datum *e,context *ctxt);
result routine_run(vec *sl,datum *r,datum args,context *ctxt);
vec vec_create_slice();
LOCAL datum *compdata_get_top_section(datum *compdata);
LOCAL datum list_pop_slow(datum *list);
LOCAL void list_append_slow(datum *list,datum value);
datum *compdata_alloc_make();
datum compdata_make();
ptrdiff_t *prog_get_jmp_delta(vec *sl,size_t offset);
ptrdiff_t *prog_append_jmp(vec *sl);
ptrdiff_t *prog_define_routine(vec *sl,datum name,datum *compdata,context *ctxt);
LOCAL void prog_append_move(vec *sl,datum *target,datum *source,datum *compdata);
void prog_append_bytecode(vec *sl,vec *src_sl);
LOCAL void prog_append_call(vec *sl,size_t capture_size,datum indices,bool pop_one,datum type,int arg_count,int return_count,datum top_arg_polyindex,datum *compdata);
datum compdata_get_polyindex(datum *compdata,datum *var);
LOCAL size_t compdata_get_frame_count(datum *compdata);
LOCAL datum prog_append_copy(vec *sl,datum *val,datum *compdata,context *ctxt);
LOCAL void prog_append_collect(vec *sl,size_t count,datum top_idx,datum *compdata);
LOCAL void prog_append_apply(vec *sl,datum *s_expr,datum *compdata,extension *ext,context *ctxt);
LOCAL size_t compdata_get_length(datum *compdata);
LOCAL datum prog_get_put_prog(datum *target,ptrdiff_t delta,int capture);
LOCAL datum compdata_put(datum *compdata,datum var);
LOCAL void prog_append_put_const(vec *sl,datum *val,datum *compdata);
LOCAL void compdata_start_new_section(datum *compdata);
LOCAL void move_values_to_variables(vec *sl,datum *var,datum *compdata,context *ctxt);
LOCAL void compdata_give_names(datum *compdata,datum *var,context *ctxt);
LOCAL datum prog_get_jmp(ptrdiff_t delta);
size_t prog_get_next_index(vec *sl);
LOCAL datum prog_get_if(ptrdiff_t delta,datum index);
LOCAL void compdata_del(datum *compdata);
LOCAL size_t prog_append_something(vec *sl);
LOCAL datum compdata_get_top_polyindex(datum *compdata);
LOCAL datum compdata_get_next_polyindex(datum *compdata);
LOCAL void prog_append_yield(vec *sl,datum type,datum yield_val_index,size_t count,size_t recieve_count,datum meta,datum *compdata);
LOCAL void prog_append_consume_expression(vec *sl,datum *source,int *i,datum *compdata,extension *ext,context *ctxt);
void prog_compile(vec *sl,datum *source,datum *compdata,extension *ext,context *ctxt);
char *context_abort_reason(context *ctxt);
context *context_alloc_make();
void abortf(context *ctxt,char *format,...);
LOCAL struct token token_read(FILE *strm);
LOCAL bool consume_control_sequence(char c,datum *form);
LOCAL bool is_allowed_inside_symbol(char c);
LOCAL bool is_whitespace(char c);
typedef struct read_result read_result;
enum read_result_type {
  READ_RESULT_OK,
};
typedef enum read_result_type read_result_type;
struct read_result {
  enum read_result_type type;
  struct datum ok_value;
};
LOCAL read_result read_result_make_eof(void);
LOCAL read_result read_result_make_ok(datum e);
datum datum_read_one(datum *args,context *ctxt);
LOCAL bool read_result_is_eof(read_result x);
enum token_type {
  TOKEN_DATUM,
  TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_PAREN,
  TOKEN_RIGHT_SQUARE,
  TOKEN_LEFT_SQUARE,
  TOKEN_RIGHT_CURLY,
  TOKEN_LEFT_CURLY,
  TOKEN_CONTROL_SEQUENCE,
  TOKEN_ERROR,
  TOKEN_EOF,
};
typedef enum token_type token_type;
LOCAL read_result datum_read(FILE *strm,context *ctxt,enum token_type terminator);
bool read_result_is_ok(read_result x);
vec datum_read_all(FILE *stre,context *ctxt);
vec list_to_vec(datum *val);
LOCAL array array_copy(array *arr);
LOCAL vec array_to_vec(array arr);
vec vec_copy(vec *src);
int list_index_of(datum *xs,datum *x);
void vec_extend(vec *list,datum *another);
datum list_get_tail(datum *list);
datum *list_get_last(datum *list);
datum list_copy(datum *list,int from,int to);
size_t array_length(array *arr);
datum datum_make_nil();
datum *array_at(array *arr,size_t i);
LOCAL array vec_to_array(vec v);
size_t vec_length(vec *s);
datum *vec_at(vec *s,size_t index);
datum datum_copy(datum *d);
datum *vec_append(vec *s,datum x);
vec vec_make_copies(size_t length,datum val);
datum datum_make_list(vec v);
datum list_make_copies(size_t length,datum val);
vec vec_make(size_t capacity);
array array_make_uninitialized(size_t length);
bool datum_is_constant(datum *d);
bool datum_eq(datum *x,datum *y);
bool datum_is_nil(datum *e);
datum *list_at(datum *list,unsigned index);
bool datum_is_the_symbol(datum *d,char *val);
int list_length(datum *seq);
bool datum_is_list(datum *e);
LOCAL char *escape_string(char *s);
char *datum_repr_pretty(datum *e,extension *ext);
LOCAL size_t datum_repr_impl(FILE *buf,datum *e,size_t depth,size_t start,bool pretty,int flat,char *spacing);
char *datum_repr(datum *e);
datum datum_make_int(int64_t value);
datum datum_make_bytestring(char *text);
datum datum_make_symbol(char *name);
bool datum_is_bytestring(datum *e);
bool datum_is_integer(datum *e);
bool datum_is_symbol(datum *e);
#define EXPORT
vec vec_make_of_impl(size_t count,datum *values);
#define vec_make_of(...)                                                       \
  vec_make_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),             \
                   (datum[]){__VA_ARGS__})
datum datum_make_list_of_impl(size_t count,datum *values);
#define datum_make_list_of(...)                                                \
  datum_make_list_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),      \
                          (datum[]){__VA_ARGS__})
extern const int FLAT;
extern const int FLAT;
extern const int FLAT_CHILDREN;
extern const int FLAT_CHILDREN;
extern const int NON_FLAT;
extern const int NON_FLAT;
#define EXPORT_INTERFACE 0
#define INTERFACE 0
