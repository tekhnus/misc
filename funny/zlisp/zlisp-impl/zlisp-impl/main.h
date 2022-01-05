/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#include <ctype.h>
#include <dlfcn.h>
#include <ffi.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
typedef struct state state;
typedef struct datum datum;
enum datum_type {
  DATUM_NIL,
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
  DATUM_ROUTINE,
  DATUM_POINTER,
  DATUM_VOID,
};
typedef enum datum_type datum_type;
typedef struct routine routine;
typedef struct prog prog;
enum prog_type {
  PROG_END,
  PROG_IF,
  PROG_NOP,
  PROG_PUT_CONST,
  PROG_PUT_ROUTINE,
  PROG_PUT_VAR,
  PROG_ARGS,
  PROG_CALL,
  PROG_COLLECT,
  PROG_POP,
  PROG_POP_PROG,
  PROG_RETURN,
  PROG_YIELD,
  PROG_MODULE_END,
};
typedef enum prog_type prog_type;
struct prog {
  enum prog_type type;
  union {
    struct {
      prog *if_true;
      prog *if_false;
    };
    struct {
      prog *nop_next;
    };
    struct {
      datum *put_const_value;
      prog *put_const_next;
    };
    struct {
      datum *put_routine_value;
      prog *put_routine_next;
    };
    struct {
      datum *put_var_value;
      prog *put_var_next;
    };
    struct prog *args_next;
    struct {
      bool call_hat;
      prog *call_next;
    };
    struct prog *collect_next;
    struct {
      datum *pop_var;
      prog *pop_next;
    };
    struct {
      datum *pop_prog_var;
      prog *pop_prog_next;
    };
    bool return_hat;
    struct {
      bool yield_hat;
      prog *yield_next;
    };
  };
};
struct routine {
  prog *prog;
  state *state;
};
typedef struct routine routine_t;
struct datum {
  enum datum_type type;
  union {
    struct {
      datum *list_head;
      datum *list_tail;
    };
    char *symbol_value;
    char *bytestring_value;
    int64_t integer_value;
    routine_t routine_value;
    struct {
      void *pointer_value;
      datum *pointer_descriptor;
    };
  };
};
typedef struct datum datum_t;
struct state {
  datum_t *vars;
  datum_t *stack;
  routine_t parent;
  routine_t hat_parent;
};
typedef struct state state_t;
typedef struct fdatum fdatum;
struct fdatum {
  int type;
  datum_t *ok_value;
  char *panic_message;
};
typedef struct fdatum fdatum_t;
void namespace_def_extern_fn(state_t **ctxt,char *name,fdatum_t(*fn)(),int cnt);
fdatum_t builtin_panic(datum_t *arg_value);
fdatum_t builtin_is_constant(datum_t *arg_value);
fdatum_t builtin_annotate(datum_t *arg_value);
fdatum_t builtin_eq(datum_t *x,datum_t *y);
bool datum_eq(datum_t *x,datum_t *y);
fdatum_t builtin_repr(datum_t *v);
fdatum_t builtin_extern_pointer(datum_t *shared_library,datum_t *name,datum_t *descriptor);
fdatum_t builtin_shared_library(datum_t *library_name);
fdatum_t builtin_tail(datum_t *list);
fdatum_t builtin_head(datum_t *list);
fdatum_t builtin_cons(datum_t *head,datum_t *tail);
fdatum_t builtin_add(datum_t *x,datum_t *y);
fdatum_t builtin_concat_bytestrings(datum_t *x,datum_t *y);
datum_t *state_value_pop(state_t **ctxt);
void state_value_put(state_t **ctxt,datum_t *v);
char *state_value_eval(state_t **ctxt,datum_t *v,fdatum_t(*module_source)(char *module));
fdatum_t pointer_ffi_call(datum_t *f,ffi_cif *cif,void **cargs);
char *pointer_ffi_serialize_args(datum_t *f,datum_t *args,void **cargs);
char *pointer_ffi_init_cif(datum_t *f,ffi_cif *cif);
bool ffi_type_init(ffi_type **type,datum_t *definition);
fdatum_t list_map(fdatum_t(*fn)(datum_t *,state_t *),datum_t *items,state_t *ctxt);
datum_t *namespace_cell_get_value(datum_t *cell,state_t *ns);
state_t *state_make_fresh();
typedef struct fstate fstate;
struct fstate {
  int type;
  state_t *ok_value;
  char *panic_message;
};
typedef struct fstate fstate_t;
bool fstate_is_panic(fstate_t result);
bool fstate_is_ok(fstate_t result);
fdatum_t fdatum_make_panic(char *message);
fdatum_t fdatum_make_ok(datum_t *v);
bool fdatum_is_ok(fdatum_t result);
char *datum_repr(datum_t *e);
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
    datum_t *ok_value;
    char *panic_message;
  };
};
typedef struct read_result read_result_t;
read_result_t datum_read(FILE *strm);
bool consume_control_sequence(char c,datum_t **form);
bool is_allowed_inside_symbol(char c);
bool is_whitespace(char c);
read_result_t read_result_make_right_paren(void);
read_result_t read_result_make_eof(void);
read_result_t read_result_make_panic(char *message);
read_result_t read_result_make_ok(datum_t *e);
bool read_result_is_right_paren(read_result_t x);
bool read_result_is_eof(read_result_t x);
bool read_result_is_panic(read_result_t x);
bool read_result_is_ok(read_result_t x);
datum_t *datum_make_pointer_to_pointer(void **ptr);
datum_t *datum_make_pointer(void *data,datum_t *signature);
datum_t *datum_make_int(int64_t value);
datum_t *datum_make_bytestring(char *text);
datum_t *datum_make_list_3(datum_t *head,datum_t *second,datum_t *third);
datum_t *datum_make_list_1(datum_t *head);
bool datum_is_void(datum_t *e);
datum_t *state_list_vars(state_t *ns);
datum_t *datum_make_list_2(datum_t *head,datum_t *second);
fdatum_t pointer_call(datum_t *f,datum_t *args);
bool datum_is_pointer(datum_t *e);
state_t *state_set_fn(state_t *ns,datum_t *symbol,datum_t *value);
state_t *state_set_var(state_t *ns,datum_t *symbol,datum_t *value);
fdatum_t state_get_var(state_t *ns,datum_t *symbol);
bool datum_is_routine(datum_t *e);
fstate_t fstate_make_ok(state_t *v);
fstate_t fstate_make_panic(char *message);
fstate_t routine_run(routine_t c);
void switch_context(routine_t *c,routine_t b,datum_t *v);
bool datum_is_integer(datum_t *e);
typedef struct prog prog_t;
char *prog_append_backquoted_statement(prog_t **begin,datum_t *stmt,fdatum_t(*module_source)(char *module));
bool fdatum_is_panic(fdatum_t result);
bool datum_is_bytestring(datum_t *e);
char *prog_init_routine(prog_t *s,datum_t *stmt,fdatum_t(*module_source)(char *module));
datum_t *datum_make_void();
#define LOCAL static
LOCAL bool datum_is_constant(datum_t *d);
state_t *state_make_builtins();
datum_t *datum_make_routine(prog_t *s,state_t *lexical_bindings);
char *prog_append_require(prog_t **begin,datum_t *src,fdatum_t(*module_source)(char *module));
void prog_append_yield(prog_t **begin,bool hat);
void prog_append_return(prog_t **begin,bool hat);
void prog_append_pop_prog(prog_t **begin,datum_t *var);
void prog_append_pop(prog_t **begin,datum_t *var);
void prog_append_collect(prog_t **begin);
void prog_append_call(prog_t **begin,bool hat);
void prog_append_args(prog_t **begin);
void prog_append_put_var(prog_t **begin,datum_t *val);
void prog_append_put_routine(prog_t **begin,datum_t *val);
void prog_append_put_const(prog_t **begin,datum_t *val);
void prog_join(prog_t *a,prog_t *b,prog_t *e);
LOCAL bool datum_is_the_symbol_pair(datum_t *d,char *val1,char *val2);
bool datum_is_list(datum_t *e);
int list_length(datum_t *seq);
state_t *state_change_parent(state_t *ns,routine_t new_parent,bool hat);
routine_t state_get_parent(state_t *ns,bool hat);
bool routine_is_null(routine_t r);
routine_t routine_make_null();
routine_t routine_make(prog_t *s,state_t *ctxt);
char *prog_append_statement(prog_t **begin,datum_t *stmt,fdatum_t(*module_source)(char *module));
bool datum_is_nil(datum_t *e);
char *prog_init_module(prog_t *s,datum_t *source,fdatum_t(*module_source)(char *module));
void prog_append_module_end(prog_t **begin);
prog_t *prog_make();
datum_t *datum_make_nil();
LOCAL datum_t *state_stack_collect(state_t **s);
datum_t *datum_make_symbol(char *name);
LOCAL void state_stack_new(state_t **s);
LOCAL datum_t *state_stack_pop(state_t **s);
datum_t *datum_make_list(datum_t *head,datum_t *tail);
state_t *state_make(datum_t *vars,datum_t *stack,routine_t parent,routine_t hat_parent);
LOCAL void state_stack_put(state_t **ns,datum_t *value);
bool datum_is_symbol(datum_t *e);
LOCAL bool datum_is_the_symbol(datum_t *d,char *val);
