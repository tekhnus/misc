// zlisp interpreter headers.
#include <ctype.h>
#include <dlfcn.h>
#include <ffi.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct datum datum_t;
typedef struct read_result read_result_t;
typedef struct datum namespace_t;
typedef struct eval_result eval_result_t;

enum datum_type {
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
  DATUM_SPECIAL,
  DATUM_OPERATOR,
  DATUM_POINTER,
};

struct datum {
  enum datum_type type;
  union {
    struct {
      datum_t *list_head;
      datum_t *list_tail;
    };
    char *symbol_value;
    char *bytestring_value;
    int64_t integer_value;
    eval_result_t (*special_call)(datum_t *, namespace_t *);
    struct {
      datum_t *operator_body;
      bool operator_eval_args;
      bool operator_eval_value;
      namespace_t *operator_context;
    };
    struct {
      void *pointer_value;
      datum_t *pointer_descriptor;
    };
  };
};

enum read_result_type {
  READ_RESULT_OK,
  READ_RESULT_PANIC,
  READ_RESULT_EOF,
  READ_RESULT_RIGHT_PAREN,
};

struct read_result {
  enum read_result_type type;
  union {
    datum_t *ok_value;
    char *panic_message;
  };
};

enum eval_result_type {
  EVAL_RESULT_OK,
  EVAL_RESULT_CONTEXT,
  EVAL_RESULT_PANIC,
};

struct eval_result {
  enum eval_result_type type;
  union {
    datum_t *ok_value;
    namespace_t *context_value;
    char *panic_message;
  };
};

bool datum_is_nil(datum_t *e);

bool datum_is_list(datum_t *e);

bool datum_is_symbol(datum_t *e);

bool datum_is_integer(datum_t *e);

bool datum_is_bytestring(datum_t *e);

bool datum_is_operator(datum_t *e);

bool datum_is_special(datum_t *e);

bool datum_is_pointer(datum_t *e);

datum_t *datum_make_nil();

datum_t *datum_make_list(datum_t *head, datum_t *tail);

datum_t *datum_make_list_1(datum_t *head);

datum_t *datum_make_list_2(datum_t *head, datum_t *second);

datum_t *datum_make_list_3(datum_t *head, datum_t *second, datum_t *third);

datum_t *datum_make_symbol(char *name);

datum_t *datum_make_bytestring(char *text);

datum_t *datum_make_int(int64_t value);

datum_t *datum_make_special(eval_result_t (*call)(datum_t *, namespace_t *));

datum_t *datum_make_operator(datum_t *body, namespace_t *lexical_bindings,
                             bool pre_eval, bool post_eval);

datum_t *datum_make_pointer(void *data, datum_t *signature);

datum_t *datum_make_pointer_to_pointer(void **ptr);

bool read_result_is_ok(read_result_t x);

bool read_result_is_panic(read_result_t x);

bool read_result_is_eof(read_result_t x);

bool read_result_is_right_paren(read_result_t x);

read_result_t read_result_make_ok(datum_t *e);

read_result_t read_result_make_panic(char *message);

read_result_t read_result_make_eof(void);

read_result_t read_result_make_right_paren(void);

read_result_t datum_read(FILE *strm);

char *datum_repr(datum_t *e);

bool eval_result_is_ok(eval_result_t result);

bool eval_result_is_context(eval_result_t result);

bool eval_result_is_panic(eval_result_t result);

eval_result_t eval_result_make_ok(datum_t *e);

eval_result_t eval_result_make_context(namespace_t *ns);

eval_result_t eval_result_make_panic(char *message);

namespace_t *namespace_make();

eval_result_t datum_eval(datum_t *e, namespace_t *ctxt);

eval_result_t namespace_make_prelude();

extern unsigned char zlisp_impl_prelude_lisp[];

extern unsigned int zlisp_impl_prelude_lisp_len;
