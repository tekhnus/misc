#pragma once
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
  DATUM_FRAME,
};
typedef enum datum_type datum_type;
typedef struct vec vec;
struct vec {
  datum *begin;
  size_t length;
  size_t capacity;
};
typedef struct frame frame;
struct frame {
  vec state;
  int type_id;
  int parent_type_id;
};
struct datum {
  enum datum_type type;
  union {
    vec list_value;
    char *symbol_value;
    char *bytestring_value;
    int64_t integer_value;
    frame frame_value;
  };
};
datum datum_make_list_of_impl(size_t count,datum *values);
#define datum_make_list_of(...)                                                \
  datum_make_list_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),      \
                          (datum[]){__VA_ARGS__})
bool datum_is_symbol(datum *e);
bool datum_is_integer(datum *e);
bool datum_is_bytestring(datum *e);
bool datum_is_frame(datum *e);
datum datum_make_symbol(char *name);
datum datum_make_bytestring(char *text);
datum datum_make_int(int64_t value);
datum datum_make_frame(frame fr);
char *datum_repr(datum *e);
char *datum_repr_bounded(datum *e,size_t depth);
char *datum_format_bounded(datum *e,size_t depth,size_t start,bool pretty,bool flat,char *spacing);
typedef struct fdatum fdatum;
struct fdatum {
  int type;
  struct datum ok_value;
  char *panic_message;
};
typedef struct result result;
struct result {
  datum type;
  datum value;
};
bool fdatum_is_panic(fdatum result);
fdatum fdatum_make_ok(datum v);
fdatum fdatum_make_panic(char *message);
bool datum_eq(datum *x,datum *y);
bool datum_is_constant(datum *d);
bool datum_is_the_symbol(datum *d,char *val);
vec vec_make(size_t capacity);
datum *vec_append(vec *s,datum x);
vec vec_make_of(size_t count,...);
datum *vec_at(vec *s,size_t index);
size_t vec_length(vec *s);
datum vec_to_datum(vec *v);
datum vec_pop(vec *v);
datum datum_make_nil();
bool datum_is_list(datum *e);
bool datum_is_nil(datum *e);
int list_length(datum *seq);
datum *list_at(datum *list,unsigned index);
datum list_copy(datum *list,int from,int to);
datum *list_get_last(datum *list);
datum list_get_tail(datum *list);
void list_append(datum *list,datum value);
datum list_pop(datum *list);
int list_index_of(datum *xs,datum *x);
datum datum_copy(datum *d);
vec vec_copy(vec *src);
vec *list_to_vec(datum *val);
typedef struct read_result read_result;
enum read_result_type {
  READ_RESULT_OK,
  READ_RESULT_PANIC,
  READ_RESULT_EOF,
  READ_RESULT_RIGHT_PAREN,
  READ_RESULT_RIGHT_BRACKET,
};
typedef enum read_result_type read_result_type;
struct read_result {
  enum read_result_type type;
  union {
    struct datum ok_value;
    char *panic_message;
  };
};
bool read_result_is_ok(read_result x);
bool read_result_is_panic(read_result x);
read_result datum_read(FILE *strm);
read_result datum_read_all(FILE *stre);
fdatum datum_read_one(FILE *stre);
typedef struct extension extension;
struct extension {
  char *(*call)(extension *self, vec *sl, datum *stmt, int *i, datum *compdata);
};
char *prog_compile_and_relocate(vec *sl,datum *source,datum *compdata,extension *ext);
fdatum prog_compile(datum *source,datum *compdata,extension *ext);
char *prog_append_expressions(vec *sl,datum *source,datum *compdata,extension *ext);
void prog_append_bytecode(vec *sl,vec *src_sl);
void prog_append_call(vec *sl,size_t capture_size,datum indices,bool pop_one,datum type,int arg_count,int return_count,datum *compdata);
void prog_append_copy(vec *sl,datum *val,datum *compdata);
void prog_append_yield(vec *sl,datum type,size_t count,size_t recieve_count,datum meta,datum *compdata);
size_t prog_append_something(vec *sl);
void prog_append_put_const(vec *sl,datum *val,datum *compdata);
datum prog_get_put_prog(ptrdiff_t delta,int capture);
datum prog_get_jmp(ptrdiff_t delta);
datum compdata_make();
datum *compdata_alloc_make();
bool compdata_has_value(datum *compdata);
void compdata_put(datum *compdata,datum var);
datum compdata_get_polyindex(datum *compdata,datum *var);
datum compdata_get_top_polyindex(datum *compdata);
datum compdata_get_shape(datum *compdata);
void store_values_to_variables(vec *sl,datum *var,datum *compdata);
vec vec_create_slice();
size_t prog_get_next_index(vec *sl);
result routine_run_with_handler(vec sl,datum *r0d,fdatum(*yield_handler)(datum *,datum *));
typedef struct routine routine;
datum routine_make(ptrdiff_t prg,routine *context);
datum *routine_make_alloc(ptrdiff_t prg,routine *context);
typedef struct lisp_extension lisp_extension;
struct lisp_extension {
  extension base;
  vec program;
  datum routine_;
  datum compdata;
  fdatum (*yield_handler)(datum *, datum *);
};
lisp_extension lisp_extension_make(vec program,datum routine_,datum compdata,fdatum(*yield_handler)(datum *,datum *));
extension null_extension_make();
