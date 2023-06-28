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
vec vec_make_of_impl(size_t count,datum *values);
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
#define vec_make_of(...)                                                       \
  vec_make_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),             \
                   (datum[]){__VA_ARGS__})
#define LOCAL static
typedef struct context context;
struct context {
  bool aborted;
  char error[1024];
};
LOCAL datum prog_read_exports(datum *spec,context *ctxt);
LOCAL datum prog_read_usages(datum *spec,context *ctxt);
typedef struct extension extension;
struct extension {
  void (*call)(extension *self, vec *sl, datum *stmt, int *i, datum *compdata,
               context *ctxt);
};
LOCAL void prog_append_exports(vec *sl,datum *spec,datum *compdata,extension *ext,context *ctxt);
LOCAL void prog_append_usages(vec *sl,datum *spec,datum *compdata,extension *ext,context *ctxt);
char *datum_repr(datum *e);
void abortf(context *ctxt,char *format,...);
bool datum_is_the_symbol(datum *d,char *val);
datum datum_make_nil();
datum datum_make_int(int64_t value);
void prog_compile(vec *sl,datum *source,datum *compdata,extension *ext,context *ctxt);
datum datum_make_list(vec v);
datum datum_make_list_of_impl(size_t count,datum *values);
#define datum_make_list_of(...)                                                \
  datum_make_list_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),      \
                          (datum[]){__VA_ARGS__})
datum datum_copy(datum *d);
datum *vec_append(vec *s,datum x);
vec vec_make(size_t capacity);
datum list_copy(datum *list,int from,int to);
bool datum_is_integer(datum *e);
int list_length(datum *seq);
bool datum_is_list(datum *e);
typedef struct lisp_extension lisp_extension;
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
bool datum_is_nil(datum *e);
datum compdata_get_polyindex(datum *compdata,datum *var);
datum datum_make_symbol(char *name);
bool datum_is_symbol(datum *e);
datum *list_at(datum *list,unsigned index);
LOCAL void null_extension_call(extension *self,vec *sl,datum *source,int *i,datum *compdata,context *ctxt);
extension null_extension_make();
LOCAL void lisp_extension_call(extension *self_,vec *sl,datum *source,int *i,datum *compdata,context *ctxt);
lisp_extension lisp_extension_make(vec program,datum routine_,datum compdata,result(*runner)(vec *,datum *,datum,context *));
#define EXPORT
#define EXPORT_INTERFACE 0
