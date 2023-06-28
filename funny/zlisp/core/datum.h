/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
typedef struct vec vec;
#include <inttypes.h>
#include <stdio.h>
typedef struct array array;
typedef struct datum datum;
struct array {
  datum *begin;
  size_t length;
};
struct vec {
  array storage;
  size_t length;
};
vec list_to_vec(datum *val);
#define LOCAL static
LOCAL array array_copy(array *arr);
LOCAL vec array_to_vec(array arr);
vec vec_copy(vec *src);
int list_index_of(datum *xs,datum *x);
void vec_extend(vec *list,datum *another);
enum datum_type {
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
};
typedef enum datum_type datum_type;
struct datum {
  enum datum_type type;
  union {
    array list_value;
    char *symbol_value;
    char *bytestring_value;
    int64_t integer_value;
  };
};
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
typedef struct extension extension;
typedef struct context context;
struct context {
  bool aborted;
  char error[1024];
};
struct extension {
  void (*call)(extension *self, vec *sl, datum *stmt, int *i, datum *compdata,
               context *ctxt);
};
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
