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

eval_result_t datum_eval(datum_t *e, namespace_t *ctxt);
void namespace_def_builtins(namespace_t *ns);

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
  EVAL_RESULT_PANIC,
};

struct eval_result {
  enum eval_result_type type;
  union {
    datum_t *ok_value;
    char *panic_message;
  };
};

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

datum_t *datum_make_nil() { return NULL; }

datum_t *datum_make_list(datum_t *head, datum_t *tail) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_LIST;
  e->list_head = head;
  e->list_tail = tail;
  return e;
}

datum_t *datum_make_list_1(datum_t *head) {
  return datum_make_list(head, datum_make_nil());
}

datum_t *datum_make_list_2(datum_t *head, datum_t *second) {
  return datum_make_list(head, datum_make_list_1(second));
}

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

bool read_result_is_ok(read_result_t x) { return x.type == READ_RESULT_OK; }

bool read_result_is_panic(read_result_t x) {
  return x.type == READ_RESULT_PANIC;
}

bool read_result_is_eof(read_result_t x) { return x.type == READ_RESULT_EOF; }

bool read_result_is_right_paren(read_result_t x) {
  return x.type == READ_RESULT_RIGHT_PAREN;
}

read_result_t read_result_make_ok(datum_t *e) {
  read_result_t result = {.type = READ_RESULT_OK, .ok_value = e};
  return result;
}

read_result_t read_result_make_panic(char *message) {
  read_result_t result = {.type = READ_RESULT_PANIC, .panic_message = message};
  return result;
}

read_result_t read_result_make_eof(void) {
  read_result_t result = {.type = READ_RESULT_EOF};
  return result;
}

read_result_t read_result_make_right_paren(void) {
  read_result_t result = {.type = READ_RESULT_RIGHT_PAREN};
  return result;
}

bool is_whitespace(char c) { return isspace(c) || c == ','; }

bool is_allowed_inside_symbol(char c) {
  return isalnum(c) || c == '.' || c == '-' || c == ':';
}

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
    return read_result_make_right_paren();
  }
  if (c == '(') {
    read_result_t elem;
    datum_t *list = datum_make_nil();
    datum_t **end_marker = &list;
    for (;;) {
      while (read_result_is_ok(elem = datum_read(strm))) {
        *end_marker = datum_make_list_1(elem.ok_value);
        end_marker = &((*end_marker)->list_tail);
      }
      if (read_result_is_right_paren(elem)) {
        return read_result_make_ok(list);
      }
      if (read_result_is_eof(elem)) {
        return read_result_make_panic("expected ')', got EOS");
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
        return read_result_make_panic("expected a number after unary minus");
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
    return read_result_make_ok(datum_make_int(sign * val));
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
    return read_result_make_ok(sym);
  }
  if (c == '"') {
    char literal[256];
    char x;
    size_t i;
    for (i = 0; (x = getc(strm)) != '"'; ++i) {
      if (x == '\\') {
        x = getc(strm);
        if (x == 'n') {
          literal[i] = '\n';
          continue;
        }
        return read_result_make_panic("unknown escape code");
      }
      literal[i] = x;
    }
    literal[i] = '\0';
    return read_result_make_ok(datum_make_bytestring(literal));
  }
  datum_t *form;
  if (consume_control_sequence(c, &form)) {
    read_result_t v = datum_read(strm);
    if (read_result_is_panic(v)) {
      return v;
    }
    if (!read_result_is_ok(v)) {
      return read_result_make_panic(
          "expected an expression after a control character");
    }
    datum_t *res = datum_make_list_1(form);
    res->list_tail = datum_make_list_1(v.ok_value);
    return read_result_make_ok(res);
  }
  char err[1024];
  sprintf(err, "unexpected symbol: 0x%x", c);
  return read_result_make_panic(err);
}

char *datum_repr(datum_t *e) {
  char *buf = malloc(1024 * sizeof(char));
  char *end = buf;
  if (datum_is_integer(e)) {
    sprintf(buf, "%ld", e->integer_value);
  } else if (datum_is_list(e)) {
    end += sprintf(end, "(");
    for (datum_t *item = e; !datum_is_nil(item); item = item->list_tail) {
      end += sprintf(end, "%s ", datum_repr(item->list_head));
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
                   datum_repr(e->pointer_descriptor));
  } else {
    sprintf(buf, "<fmt not implemented>");
  }
  return buf;
}

bool eval_result_is_ok(eval_result_t result) {
  return result.type == EVAL_RESULT_OK;
}

bool eval_result_is_panic(eval_result_t result) {
  return result.type == EVAL_RESULT_PANIC;
}

eval_result_t eval_result_make_ok(datum_t *e) {
  eval_result_t result = {.type = EVAL_RESULT_OK, .ok_value = e};
  return result;
}

eval_result_t eval_result_make_panic(char *message) {
  eval_result_t result = {.type = EVAL_RESULT_PANIC, .panic_message = message};
  return result;
}

flat_namespace_t *flat_namespace_make() { return NULL; }

flat_namespace_t *flat_namespace_set(flat_namespace_t *ns, datum_t *symbol,
                                     datum_t *value) {
  datum_t *kv = datum_make_list_1(symbol);
  kv->list_tail = datum_make_list_1(value);
  datum_t *new_ns = datum_make_list_1(kv);
  new_ns->list_tail = ns;
  return new_ns;
}

namespace_t *namespace_make_child(namespace_t *parent_namespace) {
  namespace_t *res = datum_make_list_1(flat_namespace_make());
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

eval_result_t flat_namespace_get(flat_namespace_t *ns, datum_t *symbol) {
  for (datum_t *cur = ns; !datum_is_nil(cur); cur = cur->list_tail) {
    datum_t *kv = cur->list_head;
    if (!strcmp(kv->list_head->symbol_value, symbol->symbol_value)) {
      return eval_result_make_ok(kv->list_tail->list_head);
    }
  }
  char *msg = malloc(1024);
  sprintf(msg, "unbound symbol: %s", symbol->symbol_value);
  return eval_result_make_panic(msg);
}

eval_result_t namespace_get(namespace_t *ctxt, datum_t *symbol) {
  eval_result_t v;
  namespace_t *bindings;
  for (bindings = ctxt; !namespace_is_nil(bindings);
       bindings = namespace_get_parent(bindings)) {
    v = flat_namespace_get(namespace_get_own_bindings(bindings), symbol);
    if (!eval_result_is_panic(v)) {
      return v;
    }
  }
  return v;
}

void namespace_set(namespace_t *ctxt, datum_t *symbol, datum_t *value) {
  flat_namespace_t *locals = namespace_get_own_bindings(ctxt);
  namespace_set_own_bindings(ctxt, flat_namespace_set(locals, symbol, value));
}

eval_result_t list_map(eval_result_t (*fn)(datum_t *, namespace_t *),
                       datum_t *items, namespace_t *ctxt) {
  if (!datum_is_list(items)) {
    return eval_result_make_panic("expected a list");
  }
  datum_t *evaled_items = datum_make_nil();
  datum_t **tail = &evaled_items;
  for (datum_t *arg = items; !datum_is_nil(arg); arg = arg->list_tail) {
    eval_result_t evaled_arg = fn(arg->list_head, ctxt);
    if (eval_result_is_panic(evaled_arg)) {
      return evaled_arg;
    }
    *tail = datum_make_list_1(evaled_arg.ok_value);
    tail = &((*tail)->list_tail);
  }
  return eval_result_make_ok(evaled_items);
}

eval_result_t operator_call(datum_t *f, datum_t *args, namespace_t *ctxt) {
  datum_t *passed_args = NULL;
  if (!f->operator_eval_args) {
    passed_args = args;
  } else {
    eval_result_t evaled_items = list_map(datum_eval, args, ctxt);
    if (eval_result_is_panic(evaled_items)) {
      return evaled_items;
    }
    passed_args = evaled_items.ok_value;
  }
  namespace_t *datum_ctxt = namespace_make_child(f->operator_context);
  namespace_set(datum_ctxt, datum_make_symbol("args"), passed_args);
  eval_result_t expansion = datum_eval(f->operator_body, datum_ctxt);
  if (eval_result_is_panic(expansion)) {
    return expansion;
  }
  if (!f->operator_eval_value) {
    return expansion;
  }
  expansion = datum_eval(expansion.ok_value, ctxt);
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

char *pointer_ffi_init_cif(datum_t *f, ffi_cif *cif) {
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

char *pointer_ffi_serialize_args(datum_t *f, datum_t *args, void **cargs) {
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

eval_result_t pointer_ffi_call(datum_t *f, ffi_cif *cif, void **cargs) {
  void (*fn_ptr)(void) = (void (*)(void))(f->pointer_value);
  char *rettype = f->pointer_descriptor->list_tail->list_head->symbol_value;

  if (!strcmp(rettype, "pointer")) {
    void *res = malloc(sizeof(void *));
    ffi_call(cif, fn_ptr, res, cargs);
    return eval_result_make_ok(datum_make_pointer_to_pointer(res));
  }
  if (!strcmp(rettype, "sizet")) {
    void *res = malloc(sizeof(size_t));
    ffi_call(cif, fn_ptr, res, cargs);
    return eval_result_make_ok(datum_make_int(*(int64_t *)res));
  }
  if (!strcmp(rettype, "int")) {
    void *res = malloc(sizeof(int));
    ffi_call(cif, fn_ptr, res, cargs);
    return eval_result_make_ok(datum_make_int(*(int64_t *)res));
  }
  return eval_result_make_panic("unknown return type for extern func");
}

eval_result_t pointer_call(datum_t *f, datum_t *args, namespace_t *ctxt) {
  eval_result_t passed_args = list_map(datum_eval, args, ctxt);
  if (eval_result_is_panic(passed_args)) {
    return passed_args;
  }

  ffi_cif cif;
  char *err = NULL;
  err = pointer_ffi_init_cif(f, &cif);
  if (err != NULL) {
    return eval_result_make_panic(err);
  }
  void *cargs[32];
  err = pointer_ffi_serialize_args(f, passed_args.ok_value, cargs);
  if (err != NULL) {
    return eval_result_make_panic(err);
  }
  return pointer_ffi_call(f, &cif, cargs);
}

eval_result_t datum_call(datum_t *f, datum_t *args, namespace_t *ctxt) {
  if (!datum_is_list(args)) {
    return eval_result_make_panic("args should be list");
  }
  if (datum_is_builtin(f)) {
    return (f->builtin_call)(args, ctxt);
  }
  if (datum_is_operator(f)) {
    return operator_call(f, args, ctxt);
  }
  if (datum_is_pointer(f)) {
    return pointer_call(f, args, ctxt);
  }
  return eval_result_make_panic("car should be callable");
}

eval_result_t datum_eval(datum_t *e, namespace_t *ctxt) {
  if (datum_is_integer(e) || datum_is_bytestring(e)) {
    return eval_result_make_ok(e);
  }
  if (datum_is_symbol(e)) {
    if (e->symbol_value[0] == ':') {
      return eval_result_make_ok(e);
    }
    return namespace_get(ctxt, e);
  }
  if (datum_is_nil(e)) {
    return eval_result_make_panic("cannot eval an empty list");
  }
  if (datum_is_list(e)) {
    eval_result_t f = datum_eval(e->list_head, ctxt);
    if (!eval_result_is_ok(f)) {
      return f;
    }
    eval_result_t app = datum_call(f.ok_value, e->list_tail, ctxt);
    return app;
  }
  return eval_result_make_panic("non-evalable expression");
}

eval_result_t datum_backquote(datum_t *d, namespace_t *ctxt) {
  if (!datum_is_list(d) || datum_is_nil(d)) {
    return eval_result_make_ok(d);
  }
  if (datum_is_symbol(d->list_head) &&
      !strcmp(d->list_head->symbol_value, "tilde")) {
    return datum_eval(d->list_tail->list_head, ctxt);
  }
  return list_map(datum_backquote, d, ctxt);
}

eval_result_t builtin_add(datum_t *args, namespace_t *ctxt) {
  int64_t res = 0;
  for (datum_t *arg = args; !datum_is_nil(arg); arg = arg->list_tail) {
    eval_result_t x = datum_eval(arg->list_head, ctxt);
    if (eval_result_is_panic(x)) {
      return x;
    }
    if (!datum_is_integer(x.ok_value)) {
      return eval_result_make_panic("expected integers");
    }
    res += x.ok_value->integer_value;
  }
  return eval_result_make_ok(datum_make_int(res));
}

eval_result_t builtin_eval_in(datum_t *e, namespace_t *ctxt) {
  if (datum_is_nil(e) || datum_is_nil(e->list_tail) ||
      !datum_is_nil(e->list_tail->list_tail)) {
    return eval_result_make_panic("evalinns expects exactly two arguments");
  }
  eval_result_t ns = datum_eval(e->list_head, ctxt);
  if (eval_result_is_panic(ns)) {
    return ns;
  }
  eval_result_t v = datum_eval(e->list_tail->list_head, ctxt);
  if (eval_result_is_panic(v)) {
    return v;
  }
  eval_result_t r = datum_eval(v.ok_value, ns.ok_value);
  if (eval_result_is_panic(r)) {
    return eval_result_make_ok(datum_make_list_2(
        datum_make_symbol(":err"), datum_make_bytestring(r.panic_message)));
  }
  return eval_result_make_ok(
      datum_make_list_2(datum_make_symbol(":ok"), r.ok_value));
}

eval_result_t builtin_cons(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail)) {
    return eval_result_make_panic("cons expects exactly two arguments");
  }
  eval_result_t er = datum_eval(args->list_head, ctxt);
  if (eval_result_is_panic(er)) {
    return er;
  }
  datum_t *result = datum_make_list_1(er.ok_value);
  er = datum_eval(args->list_tail->list_head, ctxt);
  if (eval_result_is_panic(er)) {
    return er;
  }
  if (!datum_is_list(er.ok_value)) {
    return eval_result_make_panic("cons requires a list as a second argument");
  }
  result->list_tail = er.ok_value;
  return eval_result_make_ok(result);
}

eval_result_t builtin_head(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("car expects exactly one argument");
  }
  eval_result_t er = datum_eval(args->list_head, ctxt);
  if (eval_result_is_panic(er)) {
    return er;
  }
  if (!datum_is_list(er.ok_value) || datum_is_nil(er.ok_value)) {
    return eval_result_make_panic("car expects a nonempty list");
  }
  return eval_result_make_ok(er.ok_value->list_head);
}

eval_result_t builtin_tail(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("cdr expects exactly one argument");
  }
  eval_result_t er = datum_eval(args->list_head, ctxt);
  if (eval_result_is_panic(er)) {
    return er;
  }
  if (!datum_is_list(er.ok_value) || datum_is_nil(er.ok_value)) {
    return eval_result_make_panic("cdr expects a nonempty list");
  }
  return eval_result_make_ok(er.ok_value->list_tail);
}

eval_result_t builtin_macro(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("macro expects a single argument");
  }
  return eval_result_make_ok(
      datum_make_operator(args->list_head, ctxt, false, true));
}

eval_result_t builtin_fn(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("fn expects a single argument");
  }
  return eval_result_make_ok(
      datum_make_operator(args->list_head, ctxt, true, false));
}

eval_result_t builtin_operator(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("form expects a single argument");
  }
  return eval_result_make_ok(
      datum_make_operator(args->list_head, ctxt, false, false));
}

eval_result_t builtin_def(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail)) {
    return eval_result_make_panic("def expects exactly two arguments");
  }
  if (!datum_is_symbol(args->list_head)) {
    return eval_result_make_panic("def requires a symbol as a first argument");
  }
  eval_result_t er = datum_eval(args->list_tail->list_head, ctxt);
  if (!eval_result_is_panic(er)) {
    namespace_set(ctxt, args->list_head, er.ok_value);
  }
  return er;
}

eval_result_t builtin_backquote(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("backquote expects a single argument");
  }
  return datum_backquote(args->list_head, ctxt);
}

eval_result_t builtin_if(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      datum_is_nil(args->list_tail->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail->list_tail)) {
    return eval_result_make_panic("if expects exactly three arguments");
  }
  eval_result_t condition = datum_eval(args->list_head, ctxt);
  if (eval_result_is_panic(condition)) {
    return condition;
  }
  if (!datum_is_nil(condition.ok_value)) {
    return datum_eval(args->list_tail->list_head, ctxt);
  }
  return datum_eval(args->list_tail->list_tail->list_head, ctxt);
}

eval_result_t builtin_load_shared_library(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic(
        "load-shared-library expects exactly one argument");
  }
  datum_t *library_name = args->list_head;
  if (!datum_is_bytestring(library_name)) {
    return eval_result_make_panic("load-shared-library expects a bytestring");
  }
  void **handle = malloc(sizeof(void *));
  *handle = dlopen(library_name->bytestring_value, RTLD_LAZY);
  char *err = dlerror();
  if (!handle) {
    return eval_result_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                                 datum_make_bytestring(err)));
  }
  return eval_result_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"), datum_make_pointer_to_pointer(handle)));
}

eval_result_t builtin_extern_pointer(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      datum_is_nil(args->list_tail->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail->list_tail)) {
    return eval_result_make_panic(
        "externcdata expects exactly three arguments");
  }
  eval_result_t shared_library = datum_eval(args->list_head, ctxt);
  if (eval_result_is_panic(shared_library)) {
    return shared_library;
  }
  if (!datum_is_pointer(shared_library.ok_value) ||
      !datum_is_symbol(shared_library.ok_value->pointer_descriptor) ||
      strcmp(shared_library.ok_value->pointer_descriptor->symbol_value,
             "pointer") ||
      !datum_is_bytestring(args->list_tail->list_head)) {
    return eval_result_make_panic("wrong externcdata usage");
  }
  void *handle = *(void **)shared_library.ok_value->pointer_value;
  void *call_ptr = dlsym(handle, args->list_tail->list_head->bytestring_value);
  char *err = dlerror();
  if (err != NULL) {
    return eval_result_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                                 datum_make_bytestring(err)));
  }
  return eval_result_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"),
      datum_make_pointer(call_ptr, args->list_tail->list_tail->list_head)));
}

eval_result_t builtin_read(datum_t *e, namespace_t *ctxt) {
  if (datum_is_nil(e) || !datum_is_nil(e->list_tail)) {
    return eval_result_make_panic("read expects exactly one argument");
  }
  eval_result_t v = datum_eval(e->list_head, ctxt);
  if (eval_result_is_panic(v)) {
    return v;
  }
  datum_t *sptr = v.ok_value;
  if (!datum_is_pointer(sptr) || !datum_is_symbol(sptr->pointer_descriptor) ||
      strcmp(sptr->pointer_descriptor->symbol_value, "pointer")) {
    return eval_result_make_panic("read expects a pointer argument");
  }
  read_result_t r = datum_read(*(FILE **)sptr->pointer_value);
  if (read_result_is_eof(r)) {
    return eval_result_make_ok(datum_make_list_1(datum_make_symbol(":eof")));
  }
  if (!read_result_is_ok(r)) {
    char *err_message;
    if (read_result_is_panic(r)) {
      err_message = r.panic_message;
    } else {
      err_message = "unknown read error";
    }
    datum_t *err = datum_make_list_2(datum_make_symbol(":err"),
                                     datum_make_bytestring(err_message));
    return eval_result_make_ok(err);
  }
  datum_t *ok = datum_make_list_2(datum_make_symbol(":ok"), r.ok_value);
  return eval_result_make_ok(ok);
}

eval_result_t builtin_print(datum_t *e, namespace_t *ctxt) {
  for (datum_t *arg = e; !datum_is_nil(arg); arg = arg->list_tail) {
    eval_result_t v = datum_eval(arg->list_head, ctxt);
    if (eval_result_is_panic(v)) {
      return v;
    }
    printf("%s ", datum_repr(v.ok_value));
  }
  printf("\n");
  return eval_result_make_ok(datum_make_nil());
}

eval_result_t builtin_eq(datum_t *args, namespace_t *ctxt) {
  eval_result_t evaled_args = list_map(datum_eval, args, ctxt);
  if (eval_result_is_panic(evaled_args)) {
    return evaled_args;
  }
  if (datum_is_nil(evaled_args.ok_value) ||
      datum_is_nil(evaled_args.ok_value->list_tail) ||
      !datum_is_nil(evaled_args.ok_value->list_tail->list_tail)) {
    return eval_result_make_panic("symbol-equals requires two arguments");
  }
  datum_t *t = datum_make_list_1(datum_make_nil());
  datum_t *f = datum_make_nil();
  if (datum_is_symbol(evaled_args.ok_value->list_head) &&
      datum_is_symbol(evaled_args.ok_value->list_tail->list_head)) {
    if (!strcmp(evaled_args.ok_value->list_head->symbol_value,
                evaled_args.ok_value->list_tail->list_head->symbol_value)) {
      return eval_result_make_ok(t);
    }
    return eval_result_make_ok(f);
  }
  if (datum_is_integer(evaled_args.ok_value->list_head) &&
      datum_is_integer(evaled_args.ok_value->list_tail->list_head)) {
    if (evaled_args.ok_value->list_head->integer_value ==
        evaled_args.ok_value->list_tail->list_head->integer_value) {
      return eval_result_make_ok(t);
    }
    return eval_result_make_ok(f);
  }

  return eval_result_make_panic("eq can't compare those things");
}

eval_result_t builtin_annotate(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("type expects exactly one argument");
  }
  eval_result_t arg_eval = datum_eval(args->list_head, ctxt);
  if (eval_result_is_panic(arg_eval)) {
    return arg_eval;
  }
  datum_t *arg_value = arg_eval.ok_value;
  char *type;
  if (datum_is_list(arg_value)) {
    type = ":list";
  } else if (datum_is_symbol(arg_value)) {
    type = ":symbol";
  } else if (datum_is_bytestring(arg_value)) {
    type = ":bytestring";
  } else if (datum_is_integer(arg_value)) {
    type = ":integer";
  } else if (datum_is_builtin(arg_value)) {
    type = ":builtin";
  } else if (datum_is_operator(arg_value)) {
    type = ":operator";
  } else if (datum_is_pointer(arg_value)) {
    type = ":pointer";
  } else {
    return eval_result_make_panic("incomplete implementation of type");
  }
  return eval_result_make_ok(
      datum_make_list_2(datum_make_symbol(type), arg_value));
}

eval_result_t builtin_is_constant(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("type expects exactly one argument");
  }
  eval_result_t arg_eval = datum_eval(args->list_head, ctxt);
  if (eval_result_is_panic(arg_eval)) {
    return arg_eval;
  }
  datum_t *arg_value = arg_eval.ok_value;
  if (datum_is_integer(arg_value) || datum_is_bytestring(arg_value) ||
      (datum_is_symbol(arg_value) && arg_value->symbol_value[0] == ':')) {
    return eval_result_make_ok(datum_make_list_1(datum_make_nil()));
  }
  return eval_result_make_ok(datum_make_nil());
}

eval_result_t builtin_panic(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("panic expects exactly one argument");
  }
  eval_result_t arg_eval = datum_eval(args->list_head, ctxt);
  if (eval_result_is_panic(arg_eval)) {
    return arg_eval;
  }
  datum_t *arg_value = arg_eval.ok_value;
  if (!datum_is_bytestring(arg_value)) {
    return eval_result_make_panic("panic expects a bytestring");
  }
  return eval_result_make_panic(arg_value->bytestring_value);
}

eval_result_t builtin_make_namespace(datum_t *args, namespace_t *ctxt) {
  if (!datum_is_nil(args)) {
    return eval_result_make_panic("makeemptyns takes no arguments");
  }
  namespace_t *ns = namespace_make_new();
  namespace_def_builtins(ns);
  return eval_result_make_ok(ns);
}

void namespace_def_builtin(namespace_t *ctxt, char *name,
                           eval_result_t (*form)(datum_t *, namespace_t *)) {
  namespace_set(ctxt, datum_make_symbol(name), datum_make_builtin(form));
}

void namespace_def_builtins(namespace_t *ns) {
  namespace_def_builtin(ns, "add", builtin_add);
  namespace_def_builtin(ns, "eval-in", builtin_eval_in);
  namespace_def_builtin(ns, "read", builtin_read);
  namespace_def_builtin(ns, "print", builtin_print);
  namespace_def_builtin(ns, "cons", builtin_cons);
  namespace_def_builtin(ns, "head", builtin_head);
  namespace_def_builtin(ns, "tail", builtin_tail);
  namespace_def_builtin(ns, "builtin.macro", builtin_macro);
  namespace_def_builtin(ns, "builtin.fn", builtin_fn);
  namespace_def_builtin(ns, "builtin.operator", builtin_operator);
  namespace_def_builtin(ns, "def", builtin_def);
  namespace_def_builtin(ns, "if", builtin_if);
  namespace_def_builtin(ns, "backquote", builtin_backquote);
  namespace_def_builtin(ns, "load-shared-library", builtin_load_shared_library);
  namespace_def_builtin(ns, "extern-pointer", builtin_extern_pointer);
  namespace_def_builtin(ns, "eq", builtin_eq);
  namespace_def_builtin(ns, "annotate", builtin_annotate);
  namespace_def_builtin(ns, "is-constant", builtin_is_constant);
  namespace_def_builtin(ns, "panic", builtin_panic);
  namespace_def_builtin(ns, "make-namespace", builtin_make_namespace);
  namespace_set(ns, datum_make_symbol("argstack"), datum_make_nil());
}

int main(int argc, char **argv) {
  if (argc < 2) {
    printf("usage: %s <prelude> <script> ...\n", argv[0]);
    exit(EXIT_FAILURE);
  }
  namespace_t *ns = namespace_make_new();
  namespace_def_builtins(ns);

  for (int i = 1; i < argc; ++i) {
    FILE *f = fopen(argv[i], "r");
    if (f == NULL) {
      perror("error while opening the script file");
      exit(EXIT_FAILURE);
    }

    read_result_t rr;

    for (; read_result_is_ok(rr = datum_read(f));) {
      eval_result_t val = datum_eval(rr.ok_value, ns);
      if (eval_result_is_panic(val)) {
	printf("%s\n", val.panic_message);
	exit(EXIT_FAILURE);
      }
    }
    if (read_result_is_right_paren(rr)) {
      printf("unmatched closing bracket\n");
      exit(EXIT_FAILURE);
    } else if (read_result_is_panic(rr)) {
      printf("%s\n", rr.panic_message);
      exit(EXIT_FAILURE);
    }
  }
  return EXIT_SUCCESS;
}
