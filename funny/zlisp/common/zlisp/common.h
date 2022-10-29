#pragma once
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
typedef struct datum datum;
bool datum_is_the_symbol(datum *d,char *val);
int list_length(datum *seq);
bool datum_is_nil(datum *e);
bool datum_is_list(datum *e);
bool datum_is_symbol(datum *e);
bool datum_is_integer(datum *e);
bool datum_is_bytestring(datum *e);
datum *datum_make_nil();
datum *datum_make_list(datum *head,datum *tail);
datum *datum_make_list_1(datum *head);
datum *datum_make_list_2(datum *head,datum *second);
datum *datum_make_list_3(datum *head,datum *second,datum *third);
datum *datum_make_list_4(datum *head,datum *second,datum *third,datum *fourth);
datum *datum_make_list_5(datum *head,datum *second,datum *third,datum *fourth,datum *fifth);
datum *datum_make_list_6(datum *head,datum *second,datum *third,datum *fourth,datum *fifth,datum *sixth);
datum *datum_make_symbol(char *name);
datum *datum_make_bytestring(char *text);
datum *datum_make_int(int64_t value);
char *datum_repr(datum *e);
char *datum_repr_bounded(datum *e,size_t depth);
typedef struct fdatum fdatum;
#include <inttypes.h>
#include <stdio.h>
struct fdatum {
  int type;
  struct datum *ok_value;
  char *panic_message;
};
bool fdatum_is_panic(fdatum result);
fdatum fdatum_make_ok(datum *v);
fdatum fdatum_make_panic(char *message);
bool datum_eq(datum *x,datum *y);
bool datum_is_constant(datum *d);
typedef struct prog_slice prog_slice;
struct prog_slice {
  datum *begin;
  size_t length;
  size_t capacity;
};
prog_slice prog_slice_make(size_t capacity);
size_t prog_slice_append_new(prog_slice *s);
datum *prog_slice_datum_at(prog_slice s,size_t index);
size_t prog_slice_length(prog_slice s);
datum *prog_slice_to_datum(prog_slice sl);
datum *list_at(datum *list,unsigned index);
datum *list_tail(datum *list);
datum *list_append(datum *list,datum *value);
datum *list_chop_last(datum *list);
int list_index_of(datum *xs,datum *x);
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
bool read_result_is_ok(read_result x);
bool read_result_is_panic(read_result x);
bool read_result_is_right_paren(read_result x);
read_result datum_read(FILE *strm);
fdatum prog_compile(datum *source,datum **compdata,datum *info);
void prog_append_call(prog_slice *sl,size_t *begin,datum *type,int arg_count,int return_count,datum **compdata);
void prog_append_put_var(prog_slice *sl,size_t *begin,datum *val,datum **compdata);
void prog_append_pop(prog_slice *sl,size_t *begin,datum *var,datum **compdata);
void prog_append_put_prog(prog_slice *sl,size_t *begin,size_t val,int capture,datum **compdata);
void prog_append_yield(prog_slice *sl,size_t *begin,datum *type,size_t count,size_t recieve_count,datum *meta,datum **compdata);
datum *compdata_make();
bool compdata_has_value(datum *compdata);
void prog_append_nop(prog_slice *sl,size_t *begin,datum *info);
size_t prog_build_init(prog_slice *sl,size_t *ep,size_t *bdr_p,datum **compdata,datum **builder_compdata);
char *prog_link_deps(prog_slice *sl,size_t *bdr_p,datum **builder_compdata,size_t p,fdatum(*module_bytecode)(char *,datum *),datum *settings);
char *prog_slice_relocate(prog_slice *dst,size_t *p,datum *src);
datum *routine_2_make(ptrdiff_t prg);
ptrdiff_t routine_2_get_offset(datum *r0d);
fdatum routine_2_run(prog_slice sl,datum **r0d,fdatum(*perform_host_instruction)(datum *,datum *));
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
