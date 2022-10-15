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
typedef struct state state;
state *state_make_fresh();
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
size_t prog_slice_length(prog_slice s);
datum *prog_slice_to_datum(prog_slice sl);
datum *list_at(datum *list,unsigned index);
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
fdatum prog_init_submodule(prog_slice *sl,size_t *off,datum *source,datum **compdata,datum *info);
void prog_append_call(prog_slice *sl,size_t *begin,bool hat,int return_count,datum **compdata);
void prog_append_put_const(prog_slice *sl,size_t *begin,datum *val,datum **compdata);
void prog_append_put_var(prog_slice *sl,size_t *begin,datum *val,datum **compdata);
void prog_append_collect(prog_slice *sl,size_t count,size_t *begin,datum **compdata);
void prog_append_pop(prog_slice *sl,size_t *begin,datum *var,datum **compdata);
void prog_append_put_prog(prog_slice *sl,size_t *begin,size_t val,int capture,datum **compdata);
void prog_append_yield(prog_slice *sl,size_t *begin,bool hat,size_t count,size_t recieve_count,datum *meta,datum **compdata);
void prog_append_nop(prog_slice *sl,size_t *begin,datum *info);
void prog_append_recieve(prog_slice *sl,size_t *begin,datum *args,datum **compdata);
datum *compdata_make();
datum *datum_make_void();
char *prog_build(prog_slice *sl,size_t ep,datum *source,fdatum(*module_source)(prog_slice *sl,size_t *p,char *),datum **compdata);
char *prog_build_one(prog_slice *sl,size_t ep,datum *stmt_or_spec,fdatum(*module_source)(prog_slice *sl,size_t *p,char *),datum **compdata);
fdatum routine_run_and_get_value(prog_slice sl,state **ctxt,ptrdiff_t prg,fdatum(*perform_host_instruction)(datum *,datum *));
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
struct state {
  struct datum *vars;
};
