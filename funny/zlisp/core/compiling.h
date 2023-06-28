/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
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
datum list_make_copies(size_t length,datum val);
typedef struct vec vec;
size_t vec_length(vec *s);
struct vec {
  array storage;
  size_t length;
};
vec vec_create_slice();
int list_index_of(datum *xs,datum *x);
#define LOCAL static
LOCAL datum list_pop_slow(datum *list);
LOCAL void list_append_slow(datum *list,datum value);
datum *list_get_last(datum *list);
datum *compdata_alloc_make();
datum compdata_make();
ptrdiff_t *prog_get_jmp_delta(vec *sl,size_t offset);
ptrdiff_t *prog_append_jmp(vec *sl);
typedef struct context context;
struct context {
  bool aborted;
  char error[1024];
};
ptrdiff_t *prog_define_routine(vec *sl,datum name,datum *compdata,context *ctxt);
LOCAL void prog_append_move(vec *sl,datum *target,datum *source,datum *compdata);
datum datum_make_int(int64_t value);
void prog_append_bytecode(vec *sl,vec *src_sl);
datum datum_make_list(vec v);
LOCAL void prog_append_call(vec *sl,size_t capture_size,datum indices,bool pop_one,datum type,int arg_count,int return_count,datum top_arg_polyindex,datum *compdata);
datum *vec_append(vec *s,datum x);
bool datum_is_nil(datum *e);
datum compdata_get_polyindex(datum *compdata,datum *var);
LOCAL size_t compdata_get_frame_count(datum *compdata);
vec vec_make(size_t capacity);
datum list_get_tail(datum *list);
LOCAL datum prog_append_copy(vec *sl,datum *val,datum *compdata,context *ctxt);
bool datum_is_symbol(datum *e);
bool datum_is_constant(datum *d);
LOCAL void prog_append_collect(vec *sl,size_t count,datum top_idx,datum *compdata);
typedef struct extension extension;
struct extension {
  void (*call)(extension *self, vec *sl, datum *stmt, int *i, datum *compdata,
               context *ctxt);
};
LOCAL void prog_append_apply(vec *sl,datum *s_expr,datum *compdata,extension *ext,context *ctxt);
char *datum_repr(datum *e);
LOCAL size_t compdata_get_length(datum *compdata);
bool datum_is_integer(datum *e);
LOCAL datum prog_get_put_prog(datum *target,ptrdiff_t delta,int capture);
LOCAL datum compdata_put(datum *compdata,datum var);
LOCAL void prog_append_put_const(vec *sl,datum *val,datum *compdata);
datum datum_make_bytestring(char *text);
LOCAL void compdata_start_new_section(datum *compdata);
LOCAL void move_values_to_variables(vec *sl,datum *var,datum *compdata,context *ctxt);
LOCAL void compdata_give_names(datum *compdata,datum *var,context *ctxt);
LOCAL datum prog_get_jmp(ptrdiff_t delta);
bool datum_eq(datum *x,datum *y);
size_t prog_get_next_index(vec *sl);
LOCAL datum prog_get_if(ptrdiff_t delta,datum index);
datum *vec_at(vec *s,size_t index);
datum datum_copy(datum *d);
LOCAL void compdata_del(datum *compdata);
LOCAL size_t prog_append_something(vec *sl);
LOCAL datum compdata_get_top_polyindex(datum *compdata);
bool datum_is_the_symbol(datum *d,char *val);
datum *list_at(datum *list,unsigned index);
datum datum_make_nil();
LOCAL datum compdata_get_next_polyindex(datum *compdata);
datum list_copy(datum *list,int from,int to);
datum datum_make_symbol(char *name);
datum datum_make_list_of_impl(size_t count,datum *values);
#define datum_make_list_of(...)                                                \
  datum_make_list_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),      \
                          (datum[]){__VA_ARGS__})
LOCAL void prog_append_yield(vec *sl,datum type,datum yield_val_index,size_t count,size_t recieve_count,datum meta,datum *compdata);
LOCAL void prog_append_expression(vec *sl,datum *source,int *i,datum *compdata,extension *ext,context *ctxt);
int list_length(datum *seq);
bool datum_is_list(datum *e);
void prog_compile(vec *sl,datum *source,datum *compdata,extension *ext,context *ctxt);
void abortf(context *ctxt,char *format,...);
char *context_abort_reason(context *ctxt);
context *context_alloc_make();
#define EXPORT
#define INTERFACE 0
