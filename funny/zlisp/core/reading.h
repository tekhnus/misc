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
datum datum_make_bytestring(char *text);
typedef struct vec vec;
struct vec {
  array storage;
  size_t length;
};
datum datum_make_list(vec v);
datum *vec_append(vec *s,datum x);
typedef struct context context;
struct context {
  bool aborted;
  char error[1024];
};
void abortf(context *ctxt,char *format,...);
#define LOCAL static
LOCAL struct token token_read(FILE *strm,context *ctxt);
LOCAL bool consume_control_sequence(char c,datum *form);
LOCAL bool is_allowed_inside_symbol(char c);
LOCAL bool is_whitespace(char c);
datum datum_make_int(int64_t value);
datum datum_make_symbol(char *name);
datum datum_make_list_of_impl(size_t count,datum *values);
#define datum_make_list_of(...)                                                \
  datum_make_list_of_impl(sizeof((datum[]){__VA_ARGS__}) / sizeof(datum),      \
                          (datum[]){__VA_ARGS__})
datum *list_at(datum *list,unsigned index);
bool datum_is_integer(datum *e);
int list_length(datum *seq);
bool datum_is_list(datum *e);
datum datum_read_one(datum *args,context *ctxt);
void vec_extend(vec *list,datum *another);
bool datum_is_nil(datum *e);
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
LOCAL datum datum_read(FILE *strm,context *ctxt,enum token_type terminator);
vec vec_make(size_t capacity);
vec datum_read_all(FILE *stre,context *ctxt);
#define EXPORT
#define EXPORT_INTERFACE 0
#define INTERFACE 0
