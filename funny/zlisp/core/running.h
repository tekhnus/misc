/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#define LOCAL static
typedef struct datum datum;
LOCAL struct frame get_frame_from_datum(datum *d);
typedef struct routine routine;
typedef struct context context;
#include <stdbool.h>
#include <stddef.h>
struct context {
  bool aborted;
  char error[1024];
};
datum *routine_make_alloc(ptrdiff_t prg,routine *context);
#include <stdarg.h>
#include <stdint.h>
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
datum datum_make_list_of_impl(size_t count,datum *values);
#define datum_make_list_of(...)                                                \
  datum_make_list_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),      \
                          (datum[]){__VA_ARGS__})
datum datum_make_int(int64_t value);
typedef struct vec vec;
struct vec {
  array storage;
  size_t length;
};
vec vec_make_of_impl(size_t count,datum *values);
#define vec_make_of(...)                                                       \
  vec_make_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),             \
                   (datum[]){__VA_ARGS__})
LOCAL datum datum_make_frame(vec state,int type_id,int parent_type_id);
datum datum_make_symbol(char *name);
datum datum_make_nil();
vec vec_make_copies(size_t length,datum val);
datum datum_make_list(vec v);
datum *array_at(array *arr,size_t i);
size_t array_length(array *arr);
bool datum_is_integer(datum *e);
char *datum_repr(datum *e);
bool datum_is_symbol(datum *e);
bool datum_is_list(datum *e);
LOCAL void routine_merge(routine *r,routine *rt_tail);
LOCAL routine routine_get_prefix(routine *r,size_t capture_count);
LOCAL datum *state_stack_at(routine *r,datum *offset,context *ctxt);
bool datum_is_nil(datum *e);
LOCAL datum state_stack_invalidate(routine *r,datum polyindex,context *ctxt);
LOCAL datum state_stack_set(routine *r,datum *target,datum value,context *ctxt);
datum routine_make(ptrdiff_t prg,routine *context);
LOCAL datum state_stack_invalidate_many(routine *r,size_t count,datum top_polyindex,context *ctxt);
size_t vec_length(vec *s);
LOCAL datum state_stack_set_many(routine *r,datum idx,datum list,context *ctxt);
datum *list_at(datum *list,unsigned index);
datum datum_copy(datum *d);
int list_length(datum *seq);
bool datum_is_the_symbol(datum *d,char *val);
bool datum_eq(datum *x,datum *y);
void abortf(context *ctxt,char *format,...);
LOCAL size_t routine_get_count(routine *r);
LOCAL routine make_routine_from_indices(routine *r,size_t capture_count,datum *call_indices,context *ctxt);
LOCAL void print_frame(vec *sl,routine *r);
LOCAL ptrdiff_t *routine_offset(routine *r);
datum *vec_at(vec *s,size_t index);
typedef struct prog prog;
LOCAL prog datum_to_prog(datum *d,context *ctxt);
typedef struct result result;
struct result {
  datum type;
  datum value;
};
LOCAL result routine_run_impl(vec *sl,routine *r,datum args,context *ctxt);
LOCAL routine get_routine_from_datum(datum *e,context *ctxt);
result routine_run(vec *sl,datum *r,datum args,context *ctxt);
#define EXPORT
#define INTERFACE 0
