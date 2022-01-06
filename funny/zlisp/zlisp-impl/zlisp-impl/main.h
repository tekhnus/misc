/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
typedef struct state state;
#include <inttypes.h>
#include <stdbool.h>
#include <stdio.h>
#include <ffi.h>
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
  prog *prog_;
  state *state_;
};
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
    routine routine_value;
    struct {
      void *pointer_value;
      datum *pointer_descriptor;
    };
  };
};
struct state {
  datum *vars;
  datum *stack;
  routine parent;
  routine hat_parent;
};
typedef struct fdatum fdatum;
struct fdatum {
  int type;
  datum *ok_value;
  char *panic_message;
};
void namespace_def_extern_fn(state **ctxt,char *name,fdatum(*fn)(),int cnt);
fdatum builtin_panic(datum *arg_value);
fdatum builtin_is_constant(datum *arg_value);
fdatum builtin_annotate(datum *arg_value);
fdatum builtin_eq(datum *x,datum *y);
bool datum_eq(datum *x,datum *y);
fdatum builtin_repr(datum *v);
fdatum builtin_extern_pointer(datum *shared_library,datum *name,datum *descriptor);
fdatum builtin_shared_library(datum *library_name);
fdatum builtin_tail(datum *list);
fdatum builtin_head(datum *list);
fdatum builtin_cons(datum *head,datum *tail);
fdatum builtin_add(datum *x,datum *y);
fdatum builtin_concat_bytestrings(datum *x,datum *y);
datum *state_value_pop(state **ctxt);
void state_value_put(state **ctxt,datum *v);
char *state_value_eval(state **ctxt,datum *v,fdatum(*module_source)(char *module));
fdatum pointer_ffi_call(datum *f,ffi_cif *cif,void **cargs);
char *pointer_ffi_serialize_args(datum *f,datum *args,void **cargs);
char *pointer_ffi_init_cif(datum *f,ffi_cif *cif);
bool ffi_type_init(ffi_type **type,datum *definition);
fdatum list_map(fdatum(*fn)(datum *,state *),datum *items,state *ctxt);
datum *namespace_cell_get_value(datum *cell,state *ns);
state *state_make_fresh();
typedef struct fstate fstate;
struct fstate {
  int type;
  state *ok_value;
  char *panic_message;
};
bool fstate_is_panic(fstate result);
bool fstate_is_ok(fstate result);
fdatum fdatum_make_panic(char *message);
fdatum fdatum_make_ok(datum *v);
bool fdatum_is_ok(fdatum result);
char *datum_repr(datum *e);
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
    datum *ok_value;
    char *panic_message;
  };
};
read_result datum_read(FILE *strm);
bool consume_control_sequence(char c,datum **form);
bool is_allowed_inside_symbol(char c);
bool is_whitespace(char c);
read_result read_result_make_right_paren(void);
read_result read_result_make_eof(void);
read_result read_result_make_panic(char *message);
read_result read_result_make_ok(datum *e);
bool read_result_is_right_paren(read_result x);
bool read_result_is_eof(read_result x);
bool read_result_is_panic(read_result x);
bool read_result_is_ok(read_result x);
datum *datum_make_pointer_to_pointer(void **ptr);
datum *datum_make_pointer(void *data,datum *signature);
datum *datum_make_int(int64_t value);
datum *datum_make_bytestring(char *text);
datum *datum_make_list_3(datum *head,datum *second,datum *third);
datum *datum_make_list_1(datum *head);
bool datum_is_void(datum *e);
datum *state_list_vars(state *ns);
datum *datum_make_list_2(datum *head,datum *second);
fdatum pointer_call(datum *f,datum *args);
bool datum_is_pointer(datum *e);
state *state_set_fn(state *ns,datum *symbol,datum *value);
state *state_set_var(state *ns,datum *symbol,datum *value);
fdatum state_get_var(state *ns,datum *symbol);
bool datum_is_routine(datum *e);
fstate fstate_make_ok(state *v);
fstate fstate_make_panic(char *message);
fstate routine_run(routine c);
void switch_context(routine *c,routine b,datum *v);
bool datum_is_integer(datum *e);
char *prog_append_backquoted_statement(prog **begin,datum *stmt,fdatum(*module_source)(char *module));
bool fdatum_is_panic(fdatum result);
bool datum_is_bytestring(datum *e);
char *prog_init_routine(prog *s,datum *stmt,fdatum(*module_source)(char *module));
datum *datum_make_void();
#define LOCAL static
LOCAL bool datum_is_constant(datum *d);
state *state_make_builtins();
datum *datum_make_routine(prog *s,state *lexical_bindings);
char *prog_append_require(prog **begin,datum *src,fdatum(*module_source)(char *module));
void prog_append_yield(prog **begin,bool hat);
void prog_append_return(prog **begin,bool hat);
void prog_append_pop_prog(prog **begin,datum *var);
void prog_append_pop(prog **begin,datum *var);
void prog_append_collect(prog **begin);
void prog_append_call(prog **begin,bool hat);
void prog_append_args(prog **begin);
void prog_append_put_var(prog **begin,datum *val);
void prog_append_put_routine(prog **begin,datum *val);
void prog_append_put_const(prog **begin,datum *val);
void prog_join(prog *a,prog *b,prog *e);
LOCAL bool datum_is_the_symbol_pair(datum *d,char *val1,char *val2);
bool datum_is_list(datum *e);
int list_length(datum *seq);
state *state_change_parent(state *ns,routine new_parent,bool hat);
routine state_get_parent(state *ns,bool hat);
bool routine_is_null(routine r);
routine routine_make_null();
routine routine_make(prog *s,state *ctxt);
char *prog_append_statement(prog **begin,datum *stmt,fdatum(*module_source)(char *module));
bool datum_is_nil(datum *e);
char *prog_init_module(prog *s,datum *source,fdatum(*module_source)(char *module));
void prog_append_module_end(prog **begin);
prog *prog_make();
datum *datum_make_nil();
LOCAL datum *state_stack_collect(state **s);
datum *datum_make_symbol(char *name);
LOCAL void state_stack_new(state **s);
LOCAL datum *state_stack_pop(state **s);
datum *datum_make_list(datum *head,datum *tail);
state *state_make(datum *vars,datum *stack,routine parent,routine hat_parent);
LOCAL void state_stack_put(state **ns,datum *value);
bool datum_is_symbol(datum *e);
LOCAL bool datum_is_the_symbol(datum *d,char *val);
