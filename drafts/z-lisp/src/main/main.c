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
typedef struct eval_result eval_result_t;
typedef struct datum namespace_t;
typedef struct read_result read_result_t;
typedef struct datum flat_namespace_t;

enum datum_type {
  DATUM_LIST,
  DATUM_SYMBOL,
  DATUM_BYTESTRING,
  DATUM_INTEGER,
  DATUM_BUILTIN,
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
    eval_result_t (*builtin_call)(datum_t *, namespace_t *);
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

enum eval_result_type {
  EVAL_RESULT_EXPR,
  EVAL_RESULT_ERR,
};

struct eval_result {
  enum eval_result_type type;
  union {
    datum_t *expr;
    char *err_message;
  };
};

enum read_result_type {
  READ_RESULT_EXPR,
  READ_RESULT_ERR,
  READ_RESULT_EOF,
  READ_RESULT_RIGHT_PAREN,
};

struct read_result {
  enum read_result_type type;
  union {
    datum_t *expr;
    char *err_message;
  };
};

eval_result_t eval(datum_t *e, namespace_t *ctxt);
char *fmt(datum_t *e);
void namespace_def_builtins(namespace_t *ns);

bool is_whitespace(char c) { return isspace(c) || c == ','; }

bool is_allowed_inside_symbol(char c) {
  return isalnum(c) || c == '.' || c == '-' || c == ':';
}

read_result_t read_result_make_eof(void) {
  read_result_t result = {.type = READ_RESULT_EOF};
  return result;
}

read_result_t read_result_make_closing(void) {
  read_result_t result = {.type = READ_RESULT_RIGHT_PAREN};
  return result;
}

bool read_result_is_expr(read_result_t x) { return x.type == READ_RESULT_EXPR; }

bool read_result_is_eof(read_result_t x) { return x.type == READ_RESULT_EOF; }

bool read_result_is_err(read_result_t x) { return x.type == READ_RESULT_ERR; }

bool read_result_is_closing(read_result_t x) {
  return x.type == READ_RESULT_RIGHT_PAREN;
}

read_result_t read_result_make_err(char *message) {
  read_result_t result = {.type = READ_RESULT_ERR, .err_message = message};
  return result;
}

read_result_t read_result_make_expr(datum_t *e) {
  read_result_t result = {.type = READ_RESULT_EXPR, .expr = e};
  return result;
}

datum_t *datum_make_list(datum_t *car) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_LIST;
  e->list_head = car;
  e->list_tail = NULL;
  return e;
}

datum_t *datum_make_nil() { return NULL; }

datum_t *datum_make_symbol(char *name) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_SYMBOL;
  size_t length = strlen(name);
  e->symbol_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->symbol_value[i] = name[i];
  }
  return e;
}

datum_t *datum_make_bytestring(char *text) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_BYTESTRING;
  size_t length = strlen(text);
  e->bytestring_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->bytestring_value[i] = text[i];
  }
  return e;
}

datum_t *datum_make_int(int64_t value) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_INTEGER;
  e->integer_value = value;
  return e;
}

datum_t *datum_make_builtin(eval_result_t (*call)(datum_t *, namespace_t *)) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_BUILTIN;
  e->builtin_call = call;
  return e;
}

datum_t *datum_make_operator(datum_t *body, namespace_t *lexical_bindings,
                             bool pre_eval, bool post_eval) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_OPERATOR;
  e->operator_body = body;
  e->operator_context = lexical_bindings;
  e->operator_eval_args = pre_eval;
  e->operator_eval_value = post_eval;
  return e;
}

datum_t *datum_make_pointer(void *data, datum_t *signature) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_POINTER;
  e->pointer_descriptor = signature;
  e->pointer_value = data;
  return e;
}

datum_t *datum_make_pointer_to_pointer(void **ptr) {
  return datum_make_pointer(ptr, datum_make_symbol("pointer"));
}

bool datum_is_nil(datum_t *e) { return e == NULL; }

bool datum_is_list(datum_t *e) { return e == NULL || e->type == DATUM_LIST; }

bool datum_is_symbol(datum_t *e) {
  return e != NULL && e->type == DATUM_SYMBOL;
}

bool datum_is_integer(datum_t *e) {
  return e != NULL && e->type == DATUM_INTEGER;
}

bool datum_is_bytestring(datum_t *e) {
  return e != NULL && e->type == DATUM_BYTESTRING;
}

bool datum_is_operator(datum_t *e) {
  return e != NULL && e->type == DATUM_OPERATOR;
}

bool datum_is_builtin(datum_t *e) {
  return e != NULL && e->type == DATUM_BUILTIN;
}

bool datum_is_pointer(datum_t *e) {
  return e != NULL && e->type == DATUM_POINTER;
}

flat_namespace_t *flat_namespace_make() { return NULL; }

flat_namespace_t *flat_namespace_set(flat_namespace_t *ns, datum_t *symbol,
                                     datum_t *value) {
  datum_t *kv = datum_make_list(symbol);
  kv->list_tail = datum_make_list(value);
  datum_t *new_ns = datum_make_list(kv);
  new_ns->list_tail = ns;
  return new_ns;
}

namespace_t *namespace_make_child(namespace_t *parent_namespace) {
  namespace_t *res = datum_make_list(flat_namespace_make());
  res->list_tail = parent_namespace;
  return res;
}

namespace_t *namespace_make_new() { return namespace_make_child(NULL); }

namespace_t *namespace_get_parent(namespace_t *ns) { return ns->list_tail; }

flat_namespace_t *namespace_get_own_bindings(namespace_t *ns) {
  return ns->list_head;
}

void namespace_set_own_bindings(namespace_t *ns, flat_namespace_t *fns) {
  ns->list_head = fns;
}

bool namespace_is_nil(namespace_t *ns) { return datum_is_nil(ns); }

bool consume_control_sequence(char c, datum_t **form) {
  if (c == '\'') {
    *form = datum_make_symbol("quote");
    return true;
  }
  if (c == '`') {
    *form = datum_make_symbol("backquote");
    return true;
  }
  if (c == '~') {
    *form = datum_make_symbol("tilde");
    return true;
  }
  return false;
}

read_result_t datum_read(FILE *strm) {
  char c;
  for (; !feof(strm) && is_whitespace(c = getc(strm));)
    ;
  if (feof(strm)) {
    return read_result_make_eof();
  }
  if (c == ')') {
    return read_result_make_closing();
  }
  if (c == '(') {
    read_result_t elem;
    datum_t *list = datum_make_nil();
    datum_t **end_marker = &list;
    for (;;) {
      while (read_result_is_expr(elem = datum_read(strm))) {
        *end_marker = datum_make_list(elem.expr);
        end_marker = &((*end_marker)->list_tail);
      }
      if (read_result_is_closing(elem)) {
        return read_result_make_expr(list);
      }
      if (read_result_is_eof(elem)) {
        return read_result_make_err("expected ')', got EOS");
      } else {
        break;
      }
    }
    return elem;
  }
  if (isdigit(c) || c == '-') {
    int64_t sign = 1;
    char h;
    if (c == '-') {
      sign = -1;
      c = getc(strm);
      if (!isdigit(c)) {
        return read_result_make_err("expected a number after unary minus");
      }
    }
    int val = c - '0';
    for (; !feof(strm) && isdigit(h = getc(strm));) {
      val *= 10;
      val += h - '0';
    }
    if (!feof(strm)) {
      ungetc(h, strm);
    }
    return read_result_make_expr(datum_make_int(sign * val));
  }
  if (is_allowed_inside_symbol(c)) {
    char *nm = malloc(128);
    nm[0] = c;
    int i;
    char x;
    for (i = 1; !feof(strm) && is_allowed_inside_symbol(x = getc(strm));
         nm[i++] = x)
      ;
    if (!feof(strm)) {
      ungetc(x, strm);
    }
    nm[i] = '\0';
    datum_t *sym = datum_make_symbol(nm);
    return read_result_make_expr(sym);
  }
  if (c == '"') {
    char literal[256];
    char x;
    size_t i;
    for (i = 0; (x = getc(strm)) != '"'; ++i) {
      literal[i] = x;
    }
    literal[i] = '\0';
    return read_result_make_expr(datum_make_bytestring(literal));
  }
  datum_t *form;
  if (consume_control_sequence(c, &form)) {
    read_result_t v = datum_read(strm);
    if (read_result_is_err(v)) {
      return v;
    }
    if (!read_result_is_expr(v)) {
      return read_result_make_err(
          "expected an expression after a control character");
    }
    datum_t *res = datum_make_list(form);
    res->list_tail = datum_make_list(v.expr);
    return read_result_make_expr(res);
  }
  char err[1024];
  sprintf(err, "unexpected symbol: 0x%x", c);
  return read_result_make_err(err);
}

eval_result_t eval_result_make_err(char *message) {
  eval_result_t result = {.type = EVAL_RESULT_ERR, .err_message = message};
  return result;
}

eval_result_t eval_result_make_expr(datum_t *e) {
  eval_result_t result = {.type = EVAL_RESULT_EXPR, .expr = e};
  return result;
}

bool eval_result_is_expr(eval_result_t result) {
  return result.type == EVAL_RESULT_EXPR;
}

bool eval_result_is_err(eval_result_t result) {
  return result.type == EVAL_RESULT_ERR;
}

eval_result_t flat_namespace_get(flat_namespace_t *ns, datum_t *symbol) {
  for (datum_t *cur = ns; !datum_is_nil(cur); cur = cur->list_tail) {
    datum_t *kv = cur->list_head;
    if (!strcmp(kv->list_head->symbol_value, symbol->symbol_value)) {
      return eval_result_make_expr(kv->list_tail->list_head);
    }
  }
  char *msg = malloc(1024);
  sprintf(msg, "unbound symbol: %s", symbol->symbol_value);
  return eval_result_make_err(msg);
}

eval_result_t namespace_get(namespace_t *ctxt, datum_t *symbol) {
  eval_result_t v;
  namespace_t *bindings;
  for (bindings = ctxt; !namespace_is_nil(bindings);
       bindings = namespace_get_parent(bindings)) {
    v = flat_namespace_get(namespace_get_own_bindings(bindings), symbol);
    if (!eval_result_is_err(v)) {
      return v;
    }
  }
  return v;
}

void namespace_set(namespace_t *ctxt, datum_t *symbol, datum_t *value) {
  flat_namespace_t *locals = namespace_get_own_bindings(ctxt);
  namespace_set_own_bindings(ctxt, flat_namespace_set(locals, symbol, value));
}

datum_t *symbol_make_args(void) { return datum_make_symbol("args"); }

eval_result_t operator_apply(datum_t *f, datum_t *args, namespace_t *ctxt) {
  datum_t *passed_args = NULL;
  if (!f->operator_eval_args) {
    passed_args = args;
  } else {
    datum_t **tail = &passed_args;
    for (datum_t *arg = args; !datum_is_nil(arg); arg = arg->list_tail) {
      eval_result_t evaled_arg = eval(arg->list_head, ctxt);
      if (eval_result_is_err(evaled_arg)) {
        return evaled_arg;
      }
      *tail = datum_make_list(evaled_arg.expr);
      tail = &((*tail)->list_tail);
    }
  }
  namespace_t *datum_ctxt = namespace_make_child(f->operator_context);
  namespace_set(datum_ctxt, symbol_make_args(), passed_args);
  eval_result_t expansion = eval(f->operator_body, datum_ctxt);
  if (eval_result_is_err(expansion)) {
    return expansion;
  }
  if (!f->operator_eval_value) {
    return expansion;
  }
  expansion = eval(expansion.expr, ctxt);
  return expansion;
}

bool ffi_type_init(ffi_type **type, datum_t *definition) {
  if (!datum_is_symbol(definition)) {
    return false;
  }
  if (!strcmp(definition->symbol_value, "string")) {
    *type = &ffi_type_pointer;
    return true;
  }
  if (!strcmp(definition->symbol_value, "pointer")) {
    *type = &ffi_type_pointer;
    return true;
  }
  if (!strcmp(definition->symbol_value, "sizet")) {
    *type = &ffi_type_uint64;
    return true;
  }
  if (!strcmp(definition->symbol_value, "int")) {
    *type = &ffi_type_sint;
    return true;
  }
  return false;
}

char *pointer_to_function_init_cif(datum_t *f, ffi_cif *cif) {
  datum_t *sig = f->pointer_descriptor;
  if (!datum_is_list(sig) || datum_is_nil(sig) ||
      datum_is_nil(sig->list_tail) ||
      !datum_is_nil(sig->list_tail->list_tail)) {
    return "the signature should be a two-item list";
  }
  ffi_type **arg_types = malloc(sizeof(ffi_type *) * 32);
  int arg_count = 0;
  datum_t *arg_def;
  for (arg_def = f->pointer_descriptor->list_head; !datum_is_nil(arg_def);
       arg_def = arg_def->list_tail) {
    if (!ffi_type_init(arg_types + arg_count, arg_def->list_head)) {
      return "something wrong with the argument type signature";
    }
    ++arg_count;
  }
  ffi_type *ret_type;
  if (!ffi_type_init(&ret_type, sig->list_tail->list_head)) {
    return "something wrong with the return type signature";
  }
  ffi_status status;
  if ((status = ffi_prep_cif(cif, FFI_DEFAULT_ABI, arg_count, ret_type,
                             arg_types)) != FFI_OK) {
    return "something went wrong during ffi_prep_cif";
  }
  return NULL;
}

char *pointer_to_function_serialize_args(datum_t *f, datum_t *args,
                                         void **cargs) {
  int arg_cnt = 0;
  datum_t *arg = args;
  for (datum_t *argt = f->pointer_descriptor->list_head; !datum_is_nil(argt);
       argt = argt->list_tail) {
    if (datum_is_nil(arg)) {
      return "too few arguments";
    }
    if (!strcmp(argt->list_head->symbol_value, "string")) {
      if (!datum_is_bytestring(arg->list_head)) {
        return "string expected, got something else";
      }
      cargs[arg_cnt] = &arg->list_head->symbol_value;
    } else if (!strcmp(argt->list_head->symbol_value, "sizet")) {
      if (!datum_is_integer(arg->list_head)) {
        return "int expected, got something else";
      }
      cargs[arg_cnt] = &arg->list_head->integer_value;
    } else if (!strcmp(argt->list_head->symbol_value, "pointer")) {
      datum_t *sig;
      if (!datum_is_pointer(arg->list_head) ||
          !datum_is_symbol(sig = arg->list_head->pointer_descriptor) ||
          strcmp(sig->symbol_value, "pointer")) {
        return "pointer expected, got something else";
      }
      cargs[arg_cnt] = arg->list_head->pointer_value;
    } else {
      return "cannot load an argument";
    }
    arg = arg->list_tail;
    ++arg_cnt;
  }
  if (!datum_is_nil(arg)) {
    return "too much arguments";
  }
  return NULL;
}

eval_result_t pointer_to_function_call(datum_t *f, ffi_cif *cif, void **cargs) {
  void (*fn_ptr)(void) = (void (*)(void))(f->pointer_value);
  char *rettype = f->pointer_descriptor->list_tail->list_head->symbol_value;

  if (!strcmp(rettype, "pointer")) {
    void *res = malloc(sizeof(void *));
    ffi_call(cif, fn_ptr, res, cargs);
    return eval_result_make_expr(datum_make_pointer_to_pointer(res));
  }
  if (!strcmp(rettype, "sizet")) {
    void *res = malloc(sizeof(size_t));
    ffi_call(cif, fn_ptr, res, cargs);
    return eval_result_make_expr(datum_make_int(*(int64_t *)res));
  }
  if (!strcmp(rettype, "int")) {
    void *res = malloc(sizeof(int));
    ffi_call(cif, fn_ptr, res, cargs);
    return eval_result_make_expr(datum_make_int(*(int64_t *)res));
  }
  return eval_result_make_err("unknown return type for extern func");
}

eval_result_t pointer_to_function_apply(datum_t *f, datum_t *args,
                                        namespace_t *ctxt) {
  datum_t *passed_args = NULL;
  datum_t **tail = &passed_args;
  for (datum_t *arg = args; !datum_is_nil(arg); arg = arg->list_tail) {
    eval_result_t evaled_arg = eval(arg->list_head, ctxt);
    if (eval_result_is_err(evaled_arg)) {
      return evaled_arg;
    }
    *tail = datum_make_list(evaled_arg.expr);
    tail = &((*tail)->list_tail);
  }

  ffi_cif cif;
  char *err = NULL;
  err = pointer_to_function_init_cif(f, &cif);
  if (err != NULL) {
    return eval_result_make_err(err);
  }
  void *cargs[32];
  err = pointer_to_function_serialize_args(f, passed_args, cargs);
  if (err != NULL) {
    return eval_result_make_err(err);
  }
  return pointer_to_function_call(f, &cif, cargs);
}

eval_result_t datum_apply(datum_t *f, datum_t *args, namespace_t *ctxt) {
  if (!datum_is_list(args)) {
    return eval_result_make_err("args should be list");
  }
  if (datum_is_builtin(f)) {
    return (f->builtin_call)(args, ctxt);
  }
  if (datum_is_operator(f)) {
    return operator_apply(f, args, ctxt);
  }
  if (datum_is_pointer(f)) {
    return pointer_to_function_apply(f, args, ctxt);
  }
  return eval_result_make_err("car should be callable");
}

eval_result_t builtin_add(datum_t *args, namespace_t *ctxt) {
  int64_t res = 0;
  for (datum_t *arg = args; !datum_is_nil(arg); arg = arg->list_tail) {
    eval_result_t x = eval(arg->list_head, ctxt);
    if (eval_result_is_err(x)) {
      return x;
    }
    if (!datum_is_integer(x.expr)) {
      return eval_result_make_err("expected integers");
    }
    res += x.expr->integer_value;
  }
  return eval_result_make_expr(datum_make_int(res));
}

eval_result_t builtin_eval(datum_t *e, namespace_t *ctxt) {
  if (datum_is_nil(e) || !datum_is_nil(e->list_tail)) {
    return eval_result_make_err("eval expects exactly one argument");
  }
  eval_result_t v = eval(e->list_head, ctxt);
  if (eval_result_is_err(v)) {
    return v;
  }
  v = eval(v.expr, ctxt);
  return v;
}

eval_result_t builtin_eval_in(datum_t *e, namespace_t *ctxt) {
  if (datum_is_nil(e) || datum_is_nil(e->list_tail) ||
      !datum_is_nil(e->list_tail->list_tail)) {
    return eval_result_make_err("evalinns expects exactly two arguments");
  }
  eval_result_t ns = eval(e->list_head, ctxt);
  if (eval_result_is_err(ns)) {
    return ns;
  }
  eval_result_t v = eval(e->list_tail->list_head, ctxt);
  if (eval_result_is_err(v)) {
    return v;
  }
  eval_result_t r = eval(v.expr, ns.expr);
  return r;
}

eval_result_t builtin_cons(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail)) {
    return eval_result_make_err("cons expects exactly two arguments");
  }
  if (!datum_is_list(args->list_tail->list_head)) {
    return eval_result_make_err("cons requires a list as a second argument");
  }
  eval_result_t er = eval(args->list_head, ctxt);
  if (eval_result_is_err(er)) {
    return er;
  }
  datum_t *result = datum_make_list(er.expr);
  er = eval(args->list_tail->list_head, ctxt);
  if (eval_result_is_err(er)) {
    return er;
  }
  result->list_tail = er.expr;
  return eval_result_make_expr(result);
}

eval_result_t builtin_car(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_err("car expects exactly one argument");
  }
  eval_result_t er = eval(args->list_head, ctxt);
  if (eval_result_is_err(er)) {
    return er;
  }
  if (!datum_is_list(er.expr) || datum_is_nil(er.expr)) {
    return eval_result_make_err("car expects a nonempty list");
  }
  return eval_result_make_expr(er.expr->list_head);
}

eval_result_t builtin_cdr(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_err("cdr expects exactly one argument");
  }
  eval_result_t er = eval(args->list_head, ctxt);
  if (eval_result_is_err(er)) {
    return er;
  }
  if (!datum_is_list(er.expr) || datum_is_nil(er.expr)) {
    return eval_result_make_err("cdr expects a nonempty list");
  }
  return eval_result_make_expr(er.expr->list_tail);
}

eval_result_t builtin_macro(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_err("macro expects a single argument");
  }
  return eval_result_make_expr(
      datum_make_operator(args->list_head, ctxt, false, true));
}

eval_result_t builtin_fn(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_err("fn expects a single argument");
  }
  return eval_result_make_expr(
      datum_make_operator(args->list_head, ctxt, true, false));
}

eval_result_t builtin_operator(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_err("form expects a single argument");
  }
  return eval_result_make_expr(
      datum_make_operator(args->list_head, ctxt, false, false));
}

eval_result_t builtin_def(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail)) {
    return eval_result_make_err("def expects exactly two arguments");
  }
  if (!datum_is_symbol(args->list_head)) {
    return eval_result_make_err("def requires a symbol as a first argument");
  }
  eval_result_t er = eval(args->list_tail->list_head, ctxt);
  if (!eval_result_is_err(er)) {
    namespace_set(ctxt, args->list_head, er.expr);
  }
  return er;
}

eval_result_t builtin_backquote(datum_t *args, namespace_t *ctxt) {
  if (!datum_is_list(args->list_head) || datum_is_nil(args->list_head)) {
    return eval_result_make_expr(args->list_head);
  }
  if (datum_is_symbol(args->list_head->list_head) &&
      !strcmp(args->list_head->list_head->symbol_value, "tilde")) {
    return eval(args->list_head->list_tail->list_head, ctxt);
  }
  datum_t *processed = datum_make_nil();
  datum_t **tail = &processed;
  for (datum_t *elem = args->list_head; !datum_is_nil(elem);
       elem = elem->list_tail) {
    eval_result_t inner =
        builtin_backquote(datum_make_list(elem->list_head), ctxt);
    if (eval_result_is_err(inner)) {
      return inner;
    }
    *tail = datum_make_list(inner.expr);
    tail = &((*tail)->list_tail);
  }

  return eval_result_make_expr(processed);
}

eval_result_t builtin_if(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      datum_is_nil(args->list_tail->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail->list_tail)) {
    return eval_result_make_err("if expects exactly three arguments");
  }
  eval_result_t condition = eval(args->list_head, ctxt);
  if (eval_result_is_err(condition)) {
    return condition;
  }
  if (!datum_is_nil(condition.expr)) {
    return eval(args->list_tail->list_head, ctxt);
  }
  return eval(args->list_tail->list_tail->list_head, ctxt);
}

eval_result_t builtin_extern_pointer(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      datum_is_nil(args->list_tail->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail->list_tail)) {
    return eval_result_make_err("externcdata expects exactly three arguments");
  }
  if (!datum_is_bytestring(args->list_head) ||
      !datum_is_bytestring(args->list_tail->list_head)) {
    return eval_result_make_err("wrong externcdata usage");
  }
  void *handle = dlopen(args->list_head->bytestring_value, RTLD_LAZY);
  char *err = dlerror();
  if (!handle) {
    return eval_result_make_err(err);
  }
  void *call_ptr = dlsym(handle, args->list_tail->list_head->bytestring_value);
  err = dlerror();
  if (err != NULL) {
    return eval_result_make_err(err);
  }
  return eval_result_make_expr(
      datum_make_pointer(call_ptr, args->list_tail->list_tail->list_head));
}

eval_result_t eval(datum_t *e, namespace_t *ctxt) {
  if (datum_is_integer(e) || datum_is_bytestring(e)) {
    return eval_result_make_expr(e);
  }
  if (datum_is_symbol(e)) {
    if (e->symbol_value[0] == ':') {
      return eval_result_make_expr(e);
    }
    return namespace_get(ctxt, e);
  }
  if (datum_is_nil(e)) {
    return eval_result_make_err("cannot eval an empty list");
  }
  if (datum_is_list(e)) {
    eval_result_t f = eval(e->list_head, ctxt);
    if (!eval_result_is_expr(f)) {
      return f;
    }
    eval_result_t app = datum_apply(f.expr, e->list_tail, ctxt);
    return app;
  }
  return eval_result_make_err("non-evalable expression");
}

char *fmt(datum_t *e) {
  char *buf = malloc(1024 * sizeof(char));
  char *end = buf;
  if (datum_is_integer(e)) {
    sprintf(buf, "%lld", e->integer_value);
  } else if (datum_is_list(e)) {
    end += sprintf(end, "(");
    for (datum_t *item = e; !datum_is_nil(item); item = item->list_tail) {
      end += sprintf(end, "%s ", fmt(item->list_head));
    }
    end += sprintf(end, ")");
  } else if (datum_is_symbol(e)) {
    end += sprintf(end, "%s", e->symbol_value);
  } else if (datum_is_bytestring(e)) {
    end += sprintf(end, "\"%s\"", e->bytestring_value);
  } else if (datum_is_operator(e)) {
    end += sprintf(end, "<form>");
  } else if (datum_is_builtin(e)) {
    end += sprintf(end, "<native form>");
  } else if (datum_is_pointer(e)) {
    end += sprintf(end, "<externcdata %p %s>", e->pointer_value,
                   fmt(e->pointer_descriptor));
  } else {
    sprintf(buf, "<fmt not implemented>");
  }
  return buf;
}

eval_result_t builtin_read(datum_t *e, namespace_t *ctxt) {
  if (datum_is_nil(e) || !datum_is_nil(e->list_tail)) {
    return eval_result_make_err("read expects exactly one argument");
  }
  eval_result_t v = eval(e->list_head, ctxt);
  if (eval_result_is_err(v)) {
    return v;
  }
  datum_t *sptr = v.expr;
  if (!datum_is_pointer(sptr) || !datum_is_symbol(sptr->pointer_descriptor) ||
      strcmp(sptr->pointer_descriptor->symbol_value, "pointer")) {
    return eval_result_make_err("read expects a pointer argument");
  }
  read_result_t r = datum_read(*(FILE **)sptr->pointer_value);
  if (read_result_is_err(r)) {
    return eval_result_make_err(r.err_message);
  } else if (!read_result_is_expr(r)) {
    return eval_result_make_err("read error");
  }
  return eval_result_make_expr(r.expr);
}

eval_result_t builtin_print(datum_t *e, namespace_t *ctxt) {
  for (datum_t *arg = e; !datum_is_nil(arg); arg = arg->list_tail) {
    eval_result_t v = eval(arg->list_head, ctxt);
    if (eval_result_is_err(v)) {
      return v;
    }
    printf("%s ", fmt(v.expr));
  }
  printf("\n");
  return eval_result_make_expr(datum_make_nil());
}

eval_result_t builtin_make_namespace(datum_t *args, namespace_t *ctxt) {
  if (!datum_is_nil(args)) {
    return eval_result_make_err("makeemptyns takes no arguments");
  }
  namespace_t *ns = namespace_make_new();
  namespace_def_builtins(ns);
  return eval_result_make_expr(ns);
}

void namespace_def_builtin(namespace_t *ctxt, char *name,
                           eval_result_t (*form)(datum_t *, namespace_t *)) {
  namespace_set(ctxt, datum_make_symbol(name), datum_make_builtin(form));
}

void namespace_def_builtins(namespace_t *ns) {
  namespace_def_builtin(ns, "add", builtin_add);
  namespace_def_builtin(ns, "eval", builtin_eval);
  namespace_def_builtin(ns, "eval-in", builtin_eval_in);
  namespace_def_builtin(ns, "read", builtin_read);
  namespace_def_builtin(ns, "print", builtin_print);
  namespace_def_builtin(ns, "cons", builtin_cons);
  namespace_def_builtin(ns, "car", builtin_car);
  namespace_def_builtin(ns, "cdr", builtin_cdr);
  namespace_def_builtin(ns, "builtin.macro", builtin_macro);
  namespace_def_builtin(ns, "builtin.fn", builtin_fn);
  namespace_def_builtin(ns, "builtin.form", builtin_operator);
  namespace_def_builtin(ns, "def", builtin_def);
  namespace_def_builtin(ns, "if", builtin_if);
  namespace_def_builtin(ns, "backquote", builtin_backquote);
  namespace_def_builtin(ns, "extern-pointer", builtin_extern_pointer);
  namespace_def_builtin(ns, "make-namespace", builtin_make_namespace);
}

int main(int argc, char **argv) {
  if (argc != 2) {
    printf("usage: %s <filename>\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  FILE *f = fopen(argv[1], "r");
  if (f == NULL) {
    perror("error while opening the script file");
    exit(EXIT_FAILURE);
  }
  namespace_t *ns = namespace_make_new();
  namespace_def_builtins(ns);

  read_result_t rr;

  for (; read_result_is_expr(rr = datum_read(f));) {
    eval_result_t val = eval(rr.expr, ns);
    if (eval_result_is_err(val)) {
      printf("%s\n", val.err_message);
      exit(EXIT_FAILURE);
    }
  }
  if (read_result_is_closing(rr)) {
    printf("unmatched closing bracket\n");
    exit(EXIT_FAILURE);
  } else if (read_result_is_err(rr)) {
    printf("%s\n", rr.err_message);
    exit(EXIT_FAILURE);
  }
  return 0;
}
