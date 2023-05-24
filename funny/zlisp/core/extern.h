/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#define LOCAL static
typedef struct fdatum fdatum;
#include <inttypes.h>
#include <stdio.h>
typedef struct datum datum;
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
struct fdatum {
  int type;
  struct datum ok_value;
  char *panic_message;
};
LOCAL fdatum prog_read_exports(datum *spec);
LOCAL fdatum prog_read_usages(datum *spec);
LOCAL datum list_to_brackets(datum *list);
typedef struct extension extension;
struct extension {
  char *(*call)(extension *self, vec *sl, datum *stmt, int *i, datum *compdata);
};
LOCAL char *prog_append_exports(vec *sl,datum *spec,datum *compdata,extension *ext);
LOCAL char *prog_append_usages(vec *sl,datum *spec,datum *compdata,extension *ext);
typedef struct lisp_extension lisp_extension;
struct lisp_extension {
  extension base;
  vec program;
  datum routine_;
  datum compdata;
  fdatum (*yield_handler)(datum *, datum *);
};
LOCAL fdatum lisp_extension_run(datum *e,lisp_extension *est);
LOCAL char *null_extension_call(extension *self,vec *sl,datum *source,int *i,datum *compdata);
extension null_extension_make();
LOCAL char *lisp_extension_call(extension *self_,vec *sl,datum *source,int *i,datum *compdata);
lisp_extension lisp_extension_make(vec program,datum routine_,datum compdata,fdatum(*yield_handler)(datum *,datum *));
typedef struct routine routine;
datum *routine_make_alloc(ptrdiff_t prg,routine *context);
LOCAL size_t routine_get_stack_size(routine *r);
LOCAL void state_stack_set(routine *r,datum *target,datum value);
datum routine_make(ptrdiff_t prg,routine *context);
LOCAL datum state_stack_collect(routine *r,size_t count);
LOCAL void state_stack_put_all(routine *r,datum list);
LOCAL datum state_stack_pop(routine *r);
LOCAL void state_stack_put(routine *r,datum value);
LOCAL size_t routine_get_count(routine *r);
LOCAL ptrdiff_t *routine_offset(routine *r);
typedef struct prog prog;
LOCAL prog datum_to_prog(datum *d);
LOCAL datum *instruction_at(vec *sl,ptrdiff_t index);
extern datum error_instruction;
LOCAL void routine_merge(routine *r,routine *rt_tail);
LOCAL datum *state_stack_at(routine *r,datum *offset);
LOCAL routine routine_get_prefix(routine *r,size_t capture_count);
LOCAL routine make_routine_from_indices(routine *r,size_t capture_count,datum *call_indices);
LOCAL void print_backtrace(vec sl,routine *r);
LOCAL datum routine_get_shape(routine *r);
LOCAL bool get_child(vec sl,routine *r);
typedef struct result result;
struct result {
  datum type;
  datum value;
};
LOCAL result routine_run(vec sl,routine *r,datum args);
LOCAL routine get_routine_from_datum(datum *e);
result routine_run_with_handler(vec sl,datum *r0d,fdatum(*yield_handler)(datum *,datum *));
LOCAL void move_values_to_variables(vec *sl,datum *var,datum *compdata);
LOCAL void compdata_give_names(datum *compdata,datum *var);
LOCAL datum list_subtract(datum *a,datum *b);
LOCAL datum *compdata_get_top_section(datum *compdata);
bool compdata_has_value(datum *compdata);
datum *compdata_alloc_make();
datum compdata_make();
LOCAL void prog_append_move(vec *sl,datum *target,datum *source,datum *compdata);
void prog_append_call(vec *sl,size_t capture_size,datum indices,bool pop_one,datum type,int arg_count,int return_count,datum *compdata);
datum compdata_get_top_polyindex(datum *compdata);
datum compdata_get_polyindex(datum *compdata,datum *var);
datum compdata_get_shape(datum *compdata);
void prog_append_copy(vec *sl,datum *val,datum *compdata);
datum prog_get_put_prog(ptrdiff_t delta,int capture);
LOCAL void compdata_start_new_section(datum *compdata);
void compdata_put(datum *compdata,datum var);
void store_values_to_variables(vec *sl,datum *var,datum *compdata);
datum prog_get_jmp(ptrdiff_t delta);
LOCAL char *prog_append_merge_compdata(vec *sl,datum *compdata,datum *another_compdata);
size_t prog_get_next_index(vec *sl);
LOCAL datum prog_get_if(ptrdiff_t delta);
LOCAL void compdata_del(datum *compdata);
size_t prog_append_something(vec *sl);
LOCAL char *prog_append_apply(vec *sl,datum *s_expr,datum *compdata,extension *ext);
LOCAL void prog_append_collect(vec *sl,size_t count,datum *compdata);
LOCAL size_t compdata_get_length(datum *compdata);
void prog_append_put_const(vec *sl,datum *val,datum *compdata);
LOCAL char *prog_append_expression(vec *sl,datum *stmt,datum *compdata,extension *ext);
void prog_append_yield(vec *sl,datum type,size_t count,size_t recieve_count,datum meta,datum *compdata);
LOCAL char *prog_append_consume_expression(vec *sl,datum *source,int *i,datum *compdata,extension *ext);
char *prog_append_expressions(vec *sl,datum *source,datum *compdata,extension *ext);
vec vec_create_slice();
void prog_append_bytecode(vec *sl,vec *src_sl);
fdatum prog_compile(datum *source,datum *compdata,extension *ext);
char *prog_compile_and_relocate(vec *sl,datum *source,datum *compdata,extension *ext);
fdatum datum_read_one(FILE *stre);
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
read_result datum_read_all_new(FILE *stre);
read_result datum_read_new(FILE *strm);
read_result datum_read_impl(FILE *strm,bool new);
read_result datum_read(FILE *strm);
LOCAL struct token token_read(FILE *strm);
LOCAL bool consume_control_sequence(char c,datum *form);
LOCAL bool is_allowed_inside_symbol(char c);
LOCAL bool is_whitespace(char c);
LOCAL read_result read_result_make_right_bracket(void);
LOCAL read_result read_result_make_right_paren(void);
LOCAL read_result read_result_make_eof(void);
LOCAL read_result read_result_make_panic(char *message);
LOCAL read_result read_result_make_ok(datum e);
LOCAL bool read_result_is_right_bracket(read_result x);
LOCAL bool read_result_is_right_paren(read_result x);
LOCAL bool read_result_is_eof(read_result x);
bool read_result_is_panic(read_result x);
bool read_result_is_ok(read_result x);
LOCAL datum brackets_to_list(datum *val);
vec brackets_to_vec(datum *val);
vec *list_to_vec(datum *val);
vec vec_copy(vec *src);
LOCAL frame frame_copy(frame *src);
int list_index_of(datum *xs,datum *x);
datum list_pop(datum *list);
datum list_get_tail(datum *list);
datum *list_get_last(datum *list);
datum list_copy(datum *list,int from,int to);
bool datum_is_nil(datum *e);
datum vec_pop(vec *v);
LOCAL datum datum_to_bracket_datum(datum *v);
datum vec_to_bracket_datum(vec *v);
datum datum_copy(datum *d);
void list_append(datum *list,datum value);
datum datum_make_nil();
datum vec_to_datum(vec *v);
size_t vec_length(vec *s);
datum *vec_at(vec *s,size_t index);
vec vec_make_of(size_t count,...);
datum *vec_append(vec *s,datum x);
vec vec_make(size_t capacity);
bool datum_is_constant(datum *d);
bool datum_eq(datum *x,datum *y);
fdatum fdatum_get_panic_message(datum *args);
fdatum fdatum_repr_datum_pointer(datum *args);
fdatum fdatum_get_value(datum *args);
fdatum fdatum_make_panic(char *message);
fdatum fdatum_make_ok(datum v);
bool fdatum_is_panic(fdatum result);
datum *list_at(datum *list,unsigned index);
bool datum_is_the_symbol(datum *d,char *val);
int list_length(datum *seq);
bool datum_is_list(datum *e);
LOCAL char *escape_string(char *s);
char *datum_format_bounded(datum *e,size_t depth,size_t start,bool pretty,int flat,char *spacing);
char *datum_repr_bounded(datum *e,size_t depth);
char *datum_repr(datum *e);
datum datum_make_frame(frame fr);
datum datum_make_int(int64_t value);
datum datum_make_bytestring(char *text);
datum datum_make_symbol(char *name);
bool datum_is_frame(datum *e);
bool datum_is_bytestring(datum *e);
bool datum_is_integer(datum *e);
bool datum_is_symbol(datum *e);
#define EXPORT
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
