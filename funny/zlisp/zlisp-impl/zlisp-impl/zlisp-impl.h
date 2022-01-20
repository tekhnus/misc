#include <stdint.h>
#include <stdbool.h>
typedef struct datum datum;
bool datum_is_the_symbol(datum *d,char *val);
typedef struct routine routine;
#include <inttypes.h>
#include <stdio.h>
#include <ffi.h>
typedef struct prog prog;
typedef struct state state;
struct routine {
  struct prog *prog_;
  struct state *state_;
};
routine routine_make(prog *s,state *ctxt);
routine routine_make_null();
bool routine_is_null(routine r);
int list_length(datum *seq);
bool datum_is_nil(datum *e);
bool datum_is_list(datum *e);
bool datum_is_symbol(datum *e);
bool datum_is_integer(datum *e);
bool datum_is_bytestring(datum *e);
bool datum_is_routine(datum *e);
bool datum_is_pointer(datum *e);
bool datum_is_void(datum *e);
datum *datum_make_nil();
datum *datum_make_list(datum *head,datum *tail);
datum *datum_make_list_1(datum *head);
datum *datum_make_list_2(datum *head,datum *second);
datum *datum_make_list_3(datum *head,datum *second,datum *third);
datum *datum_make_symbol(char *name);
datum *datum_make_bytestring(char *text);
datum *datum_make_int(int64_t value);
datum *datum_make_routine(prog *s,state *lexical_bindings);
datum *datum_make_pointer(void *data,datum *signature);
datum *datum_make_pointer_to_pointer(void **ptr);
datum *datum_make_void();
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
bool read_result_is_eof(read_result x);
bool read_result_is_right_paren(read_result x);
read_result read_result_make_ok(datum *e);
read_result read_result_make_panic(char *message);
read_result read_result_make_eof(void);
read_result read_result_make_right_paren(void);
bool is_whitespace(char c);
bool is_allowed_inside_symbol(char c);
bool consume_control_sequence(char c,datum **form);
struct token token_read(FILE *strm);
read_result datum_read(FILE *strm);
typedef struct fdatum fdatum;
struct fdatum {
  int type;
  struct datum *ok_value;
  char *panic_message;
};
fdatum datum_read_one(FILE *stre);
fdatum datum_read_all(FILE *stre);
char *datum_repr(datum *e);
bool fdatum_is_ok(fdatum result);
bool fdatum_is_panic(fdatum result);
fdatum fdatum_make_ok(datum *v);
fdatum fdatum_make_panic(char *message);
fdatum fdatum_get_value(fdatum result);
char *fdatum_get_panic_message(fdatum result);
typedef struct fstate fstate;
struct fstate {
  int type;
  struct state *ok_value;
  char *panic_message;
};
bool fstate_is_ok(fstate result);
bool fstate_is_panic(fstate result);
fstate fstate_make_ok(state *v);
fstate fstate_make_panic(char *message);
state *state_make(datum *vars,datum *stack,routine parent,routine hat_parent);
state *state_make_fresh();
state *state_set_var(state *ns,datum *symbol,datum *value);
state *state_set_fn(state *ns,datum *symbol,datum *value);
datum *namespace_cell_get_value(datum *cell,state *ns);
fdatum state_get_var(state *ns,datum *symbol);
fdatum list_map(fdatum(*fn)(datum *,state *),datum *items,state *ctxt);
bool ffi_type_init(ffi_type **type,datum *definition);
char *pointer_ffi_init_cif(datum *f,ffi_cif *cif);
char *pointer_ffi_serialize_args(datum *f,datum *args,void **cargs);
fdatum pointer_ffi_call(datum *f,ffi_cif *cif,void **cargs);
fdatum pointer_call(datum *f,datum *args);
datum *state_list_vars(state *ns);
bool datum_eq(datum *x,datum *y);
bool datum_is_constant(datum *d);
void state_stack_put(state **ns,datum *value);
datum *state_stack_pop(state **s);
void state_stack_new(state **s);
datum *state_stack_collect(state **s);
routine state_get_parent(state *ns,bool hat);
state *state_change_parent(state *ns,routine new_parent,bool hat);
prog *prog_make();
char *prog_init_module(prog *s,datum *source,routine(*module_source)(char *));
char *prog_init_submodule(prog *s,datum *source,routine(*module_source)(char *));
fdatum routine_run_and_get_value(state **ctxt,prog *p,fdatum(*perform_host_instruction)(datum *,datum *));
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
    struct routine routine_value;
    struct {
      void *pointer_value;
      struct datum *pointer_descriptor;
    };
  };
};
struct state {
  struct datum *vars;
  struct datum *stack;
  struct routine parent;
  struct routine hat_parent;
};
enum prog_type {
  PROG_END,
  PROG_IF,
  PROG_NOP,
  PROG_PUT_CONST,
  PROG_PUT_VAR,
  PROG_ARGS,
  PROG_CALL,
  PROG_HOST,
  PROG_COLLECT,
  PROG_POP,
  PROG_POP_PROG,
  PROG_SET_CLOSURES,
  PROG_RETURN,
  PROG_YIELD,
  PROG_IMPORT,
};
typedef enum prog_type prog_type;
struct prog {
  enum prog_type type;
  union {
    struct {
      struct prog *if_true;
      struct prog *if_false;
    };
    struct {
      struct prog *nop_next;
    };
    struct {
      struct datum *put_const_value;
      struct prog *put_const_next;
    };
    struct {
      struct datum *put_var_value;
      struct prog *put_var_next;
    };
    struct prog *args_next;
    struct {
      bool call_hat;
      struct prog *call_next;
    };
    struct {
      struct datum *host_instruction;
      struct prog *host_next;
    };
    struct prog *collect_next;
    struct {
      struct datum *pop_var;
      struct prog *pop_next;
    };
    struct {
      struct datum *pop_prog_var;
      bool pop_prog_hat;
      struct prog *pop_prog_next;
    };
    struct {
      struct prog *set_closures_prog;
      struct datum *set_closures_name;
      bool set_closures_hat;
      struct prog *set_closures_next;
    };
    bool return_hat;
    struct {
      bool yield_hat;
      struct prog *yield_next;
    };
    struct prog *import_next;
  };
};
#define INTERFACE 0
#define EXPORT_INTERFACE 0
#define LOCAL_INTERFACE 0
#define EXPORT
#define LOCAL static
#define PUBLIC
#define PRIVATE
#define PROTECTED
