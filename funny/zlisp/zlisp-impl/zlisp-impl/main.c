// zlisp interpreter.
#include <zlisp-impl/main.h>

#include <ctype.h>
#include <dlfcn.h>
#include <ffi.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

bool datum_is_special(datum_t *e) {
  return e != NULL && e->type == DATUM_SPECIAL;
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

datum_t *datum_make_list_3(datum_t *head, datum_t *second, datum_t *third) {
  return datum_make_list(head, datum_make_list_2(second, third));
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

datum_t *datum_make_special(eval_result_t (*call)(datum_t *, namespace_t *)) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_SPECIAL;
  e->special_call = call;
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
  return isalnum(c) || c == '.' || c == '-' || c == '_' || c == ':' || c == '+';
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
  } else if (datum_is_special(e)) {
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

bool eval_result_is_context(eval_result_t result) {
  return result.type == EVAL_RESULT_CONTEXT;
}

bool eval_result_is_panic(eval_result_t result) {
  return result.type == EVAL_RESULT_PANIC;
}

eval_result_t eval_result_make_ok(datum_t *e) {
  eval_result_t result = {.type = EVAL_RESULT_OK, .ok_value = e};
  return result;
}

eval_result_t eval_result_make_context(namespace_t *ns) {
  eval_result_t result = {.type = EVAL_RESULT_CONTEXT, .context_value = ns};
  return result;
}

eval_result_t eval_result_make_panic(char *message) {
  eval_result_t result = {.type = EVAL_RESULT_PANIC, .panic_message = message};
  return result;
}

namespace_t *namespace_make() { return datum_make_nil(); }

namespace_t *namespace_set(namespace_t *ns, datum_t *symbol, datum_t *value) {
  datum_t *kv = datum_make_list_3(symbol, datum_make_symbol(":value"), value);
  return datum_make_list(kv, ns);
}

namespace_t *namespace_set_fn(namespace_t *ns, datum_t *symbol,
                              datum_t *value) {
  datum_t *kv = datum_make_list_3(symbol, datum_make_symbol(":fn"), value);
  return datum_make_list(kv, ns);
}

eval_result_t namespace_get(namespace_t *ns, datum_t *symbol) {
  for (datum_t *cur = ns; !datum_is_nil(cur); cur = cur->list_tail) {
    datum_t *kv = cur->list_head;
    if (!strcmp(kv->list_head->symbol_value, symbol->symbol_value)) {
      datum_t *tag_and_value = kv->list_tail;
      if (!strcmp(tag_and_value->list_head->symbol_value, ":value")) {
        return eval_result_make_ok(tag_and_value->list_tail->list_head);
      }
      if (!strcmp(tag_and_value->list_head->symbol_value, ":fn")) {
        datum_t *body = tag_and_value->list_tail->list_head;
        datum_t *fn = datum_make_operator(body, ns, true, false);
        return eval_result_make_ok(fn);
      }
      return eval_result_make_panic("namespace implementation error");
    }
  }
  char *msg = malloc(1024);
  sprintf(msg, "unbound symbol: %s", symbol->symbol_value);
  return eval_result_make_panic(msg);
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
    if (eval_result_is_context(evaled_arg)) {
      return eval_result_make_panic("context not expected in list_map");
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
    if (eval_result_is_context(evaled_items)) {
      return eval_result_make_panic("context not expected in operator_call");
    }
    passed_args = evaled_items.ok_value;
  }
  namespace_t *datum_ctxt = f->operator_context;
  datum_ctxt =
      namespace_set(datum_ctxt, datum_make_symbol("args"), passed_args);
  eval_result_t expansion = datum_eval(f->operator_body, datum_ctxt);
  if (eval_result_is_panic(expansion)) {
    return expansion;
  }
  if (eval_result_is_context(expansion)) {
    return eval_result_make_panic("an operator should not return a context");
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
  if (!strcmp(definition->symbol_value, "datum")) {
    *type = &ffi_type_pointer;
    return true;
  }
  if (!strcmp(definition->symbol_value, "eval_result")) {
    *type = malloc(sizeof(ffi_type));
    (*type)->type = FFI_TYPE_STRUCT;
    ffi_type **elements = malloc(3 * sizeof(ffi_type *));
    elements[0] = &ffi_type_pointer;
    elements[1] = &ffi_type_pointer;
    elements[2] = NULL;
    (*type)->elements = elements;
    return type;
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
      cargs[arg_cnt] = &arg->list_head->bytestring_value;
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
    } else if (!strcmp(argt->list_head->symbol_value, "datum")) {
      cargs[arg_cnt] = &arg->list_head;
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
  void (*fn_ptr)(void) = __extension__(void (*)(void))(f->pointer_value);
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
  if (!strcmp(rettype, "eval_result")) {
    eval_result_t res;
    ffi_call(cif, fn_ptr, &res, cargs);
    return res;
  }
  return eval_result_make_panic("unknown return type for extern func");
}

eval_result_t pointer_call(datum_t *f, datum_t *args, namespace_t *ctxt) {
  eval_result_t passed_args = list_map(datum_eval, args, ctxt);
  if (eval_result_is_panic(passed_args)) {
    return passed_args;
  }
  if (eval_result_is_context(passed_args)) {
    return eval_result_make_panic("context was not expected in pointer_call");
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
  if (datum_is_special(f)) {
    return (f->special_call)(args, ctxt);
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
    if (eval_result_is_panic(f)) {
      return f;
    }
    if (eval_result_is_context(f)) {
      return eval_result_make_panic("expected a callable, got a context");
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

eval_result_t builtin_add(datum_t *x, datum_t *y) {
  if (!datum_is_integer(x) || !datum_is_integer(y)) {
    return eval_result_make_panic("expected integers");
  }
  return eval_result_make_ok(
      datum_make_int(x->integer_value + y->integer_value));
}

eval_result_t builtin_cons(datum_t *head, datum_t *tail) {
  if (!datum_is_list(tail)) {
    return eval_result_make_panic("cons requires a list as a second argument");
  }
  return eval_result_make_ok(datum_make_list(head, tail));
}

eval_result_t builtin_head(datum_t *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return eval_result_make_panic("car expects a nonempty list");
  }
  return eval_result_make_ok(list->list_head);
}

eval_result_t builtin_tail(datum_t *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return eval_result_make_panic("cdr expects a nonempty list");
  }
  return eval_result_make_ok(list->list_tail);
}

eval_result_t special_macro(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("macro expects a single argument");
  }
  return eval_result_make_ok(
      datum_make_operator(args->list_head, ctxt, false, true));
}

eval_result_t special_fn(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("fn expects a single argument");
  }
  return eval_result_make_ok(
      datum_make_operator(args->list_head, ctxt, true, false));
}

eval_result_t special_operator(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("form expects a single argument");
  }
  return eval_result_make_ok(
      datum_make_operator(args->list_head, ctxt, false, false));
}

eval_result_t special_switch(datum_t *args, namespace_t *ctxt) {
  for (datum_t *branch = args; !datum_is_nil(branch);
       branch = branch->list_tail) {
    datum_t *b = branch->list_head;
    if (!datum_is_list(b) || datum_is_nil(b) || datum_is_nil(b->list_tail) ||
        !datum_is_nil(b->list_tail->list_tail)) {
      return eval_result_make_panic("builtin.switch requires pairs");
    }
    eval_result_t cond = datum_eval(b->list_head, ctxt);
    if (eval_result_is_panic(cond)) {
      return cond;
    }
    if (eval_result_is_context(cond)) {
      return eval_result_make_panic("switch expected a value, got a context");
    }
    datum_t *c = cond.ok_value;
    if (!datum_is_list(c) || datum_is_nil(c) ||
        !datum_is_symbol(c->list_head)) {
      return eval_result_make_panic(
          "builtin.switch condition should return a tuple");
    }
    char *tag = c->list_head->symbol_value;
    if (!strcmp(tag, ":err")) {
      continue;
    }
    if (strcmp(tag, ":ok") || datum_is_nil(c->list_tail) ||
        !datum_is_nil(c->list_tail->list_tail)) {
      return eval_result_make_panic("wrong switch");
    }
    datum_t *a = c->list_tail->list_head;
    namespace_t *new_ctxt = namespace_set(ctxt, datum_make_symbol("args"), a);
    eval_result_t branch_val = datum_eval(b->list_tail->list_head, new_ctxt);
    return branch_val;
  }
  return eval_result_make_panic("nothing matched");
}

eval_result_t special_def(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail)) {
    return eval_result_make_panic("def expects exactly two arguments");
  }
  if (!datum_is_symbol(args->list_head)) {
    return eval_result_make_panic("def requires a symbol as a first argument");
  }
  eval_result_t er = datum_eval(args->list_tail->list_head, ctxt);
  if (eval_result_is_panic(er)) {
    return er;
  }
  if (eval_result_is_context(er)) {
    return eval_result_make_panic("def expected a value, got a context");
  }
  return eval_result_make_context(
      namespace_set(ctxt, args->list_head, er.ok_value));
}

eval_result_t special_defn(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail)) {
    return eval_result_make_panic("defun expects exactly two arguments");
  }
  if (!datum_is_symbol(args->list_head)) {
    return eval_result_make_panic(
        "defun requires a symbol as a first argument");
  }
  return eval_result_make_context(
      namespace_set_fn(ctxt, args->list_head, args->list_tail->list_head));
}

eval_result_t special_backquote(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("backquote expects a single argument");
  }
  return datum_backquote(args->list_head, ctxt);
}

eval_result_t special_if(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      datum_is_nil(args->list_tail->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail->list_tail)) {
    return eval_result_make_panic("if expects exactly three arguments");
  }
  eval_result_t condition = datum_eval(args->list_head, ctxt);
  if (eval_result_is_panic(condition)) {
    return condition;
  }
  if (eval_result_is_context(condition)) {
    return eval_result_make_panic("if expected a value, got a context");
  }
  if (!datum_is_nil(condition.ok_value)) {
    return datum_eval(args->list_tail->list_head, ctxt);
  }
  return datum_eval(args->list_tail->list_tail->list_head, ctxt);
}

datum_t *namespace_list(namespace_t *ns) {
  datum_t *result = datum_make_nil();
  datum_t **nil = &result;
  for (datum_t *cur = ns; !datum_is_nil(cur); cur = cur->list_tail) {
    datum_t *kv = cur->list_head;
    datum_t *key = kv->list_head;
    datum_t *tag_and_value = kv->list_tail;
    datum_t *tag = tag_and_value->list_head;
    datum_t *value = tag_and_value->list_tail->list_head;
    datum_t *keyval;
    if (!strcmp(tag_and_value->list_head->symbol_value, ":value")) {
      keyval = datum_make_list_2(key, value);
    } else if (!strcmp(tag_and_value->list_head->symbol_value, ":fn")) {
      datum_t *body = tag_and_value->list_tail->list_head;
      datum_t *fn = datum_make_operator(body, ns, true, false);
      keyval = datum_make_list_2(key, fn);
    } else {
      fprintf(stderr, "namespace implementation error");
      exit(EXIT_FAILURE);
    }
    *nil = datum_make_list_1(keyval);
    nil = &((*nil)->list_tail);
  }
  return result;
}

eval_result_t special_require(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("require expects a single argument");
  }
  eval_result_t filename = datum_eval(args->list_head, ctxt);
  if (eval_result_is_panic(filename)) {
    return filename;
  }
  if (eval_result_is_context(filename)) {
    return eval_result_make_panic("require expected a value, got a context");
  }
  if (!datum_is_bytestring(filename.ok_value)) {
    return eval_result_make_panic("require expected a string");
  }
  FILE *module = fopen(filename.ok_value->bytestring_value, "r");
  if (module == NULL) {
    return eval_result_make_panic("error while opening the required file");
  }

  eval_result_t prelude = namespace_make_prelude();
  if (eval_result_is_panic(prelude)) {
    return filename;
  }
  if (eval_result_is_ok(prelude)) {
    return eval_result_make_panic("prelude was expected to be a context");
  }
  namespace_t *ns = prelude.context_value;
  read_result_t rr;
  for (; read_result_is_ok(rr = datum_read(module));) {
    eval_result_t val = datum_eval(rr.ok_value, ns);
    if (eval_result_is_panic(val)) {
      return val;
    }
    if (eval_result_is_ok(val)) {
      return eval_result_make_panic(
          "the program should consist of statements\n");
    }
    ns = val.context_value;
  }
  datum_t *imported_bindings = namespace_list(ns);
  for (; !datum_is_nil(imported_bindings);
       imported_bindings = imported_bindings->list_tail) {
    datum_t *sym = imported_bindings->list_head->list_head;
    datum_t *val = imported_bindings->list_head->list_tail->list_head;

    ctxt = namespace_set(ctxt, sym, val);
  }
  return eval_result_make_context(ctxt);
}

eval_result_t builtin_shared_library(datum_t *library_name) {
  if (!datum_is_bytestring(library_name)) {
    return eval_result_make_panic("load-shared-library expects a bytestring");
  }
  void **handle = malloc(sizeof(void *));
  *handle = dlopen(library_name->bytestring_value, RTLD_LAZY);
  char *err = dlerror();
  if (!*handle) {
    return eval_result_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                                 datum_make_bytestring(err)));
  }
  return eval_result_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"), datum_make_pointer_to_pointer(handle)));
}

eval_result_t builtin_extern_pointer(datum_t *shared_library, datum_t *name,
                                     datum_t *descriptor) {
  if (!datum_is_pointer(shared_library) ||
      !datum_is_symbol(shared_library->pointer_descriptor) ||
      strcmp(shared_library->pointer_descriptor->symbol_value, "pointer")) {
    return eval_result_make_panic("wrong externcdata usage");
  }
  void *handle = *(void **)shared_library->pointer_value;
  if (!datum_is_bytestring(name)) {
    return eval_result_make_panic("externcdata expected a string");
  }
  void *call_ptr = dlsym(handle, name->bytestring_value);
  char *err = dlerror();
  if (err != NULL) {
    return eval_result_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                                 datum_make_bytestring(err)));
  }
  return eval_result_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"), datum_make_pointer(call_ptr, descriptor)));
}

eval_result_t builtin_repr(datum_t *v) {
  return eval_result_make_ok(datum_make_bytestring(datum_repr(v)));
}

eval_result_t builtin_eq(datum_t *x, datum_t *y) {
  datum_t *t = datum_make_list_1(datum_make_nil());
  datum_t *f = datum_make_nil();
  if (datum_is_symbol(x) && datum_is_symbol(y)) {
    if (!strcmp(x->symbol_value, y->symbol_value)) {
      return eval_result_make_ok(t);
    }
    return eval_result_make_ok(f);
  }
  if (datum_is_integer(x) && datum_is_integer(y)) {
    if (x->integer_value == y->integer_value) {
      return eval_result_make_ok(t);
    }
    return eval_result_make_ok(f);
  }

  return eval_result_make_panic("eq can't compare those things");
}

eval_result_t builtin_annotate(datum_t *arg_value) {
  char *type;
  if (datum_is_list(arg_value)) {
    type = ":list";
  } else if (datum_is_symbol(arg_value)) {
    type = ":symbol";
  } else if (datum_is_bytestring(arg_value)) {
    type = ":bytestring";
  } else if (datum_is_integer(arg_value)) {
    type = ":integer";
  } else if (datum_is_special(arg_value)) {
    type = ":special";
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

eval_result_t builtin_is_constant(datum_t *arg_value) {
  if (datum_is_integer(arg_value) || datum_is_bytestring(arg_value) ||
      (datum_is_symbol(arg_value) && arg_value->symbol_value[0] == ':')) {
    return eval_result_make_ok(datum_make_list_1(datum_make_nil()));
  }
  return eval_result_make_ok(datum_make_nil());
}

eval_result_t special_panic(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return eval_result_make_panic("panic expects exactly one argument");
  }
  eval_result_t arg_eval = datum_eval(args->list_head, ctxt);
  if (eval_result_is_panic(arg_eval)) {
    return arg_eval;
  }
  if (eval_result_is_context(arg_eval)) {
    return eval_result_make_panic("panic expected a value, got a context");
  }
  datum_t *arg_value = arg_eval.ok_value;
  if (!datum_is_bytestring(arg_value)) {
    return eval_result_make_panic("panic expects a bytestring");
  }
  return eval_result_make_panic(arg_value->bytestring_value);
}

eval_result_t special_progn(datum_t *args, namespace_t *ctxt) {
  eval_result_t val = eval_result_make_context(ctxt);
  for (; !datum_is_nil(args); args = args->list_tail) {
    if (eval_result_is_ok(val)) {
      return eval_result_make_panic(
          "only the last element in progn can be an expression");
    }
    datum_t *arg = args->list_head;
    val = datum_eval(arg, val.context_value);
    if (eval_result_is_panic(val)) {
      return val;
    }
  }
  return val;
}

void namespace_def_special(namespace_t **ctxt, char *name,
                           eval_result_t (*form)(datum_t *, namespace_t *)) {
  *ctxt =
      namespace_set(*ctxt, datum_make_symbol(name), datum_make_special(form));
}

void namespace_def_extern_fn(namespace_t **ctxt, char *name,
                             eval_result_t (*fn)(), int cnt) {
  datum_t *sig = datum_make_nil();
  for (int i = 0; i < cnt; ++i) {
    sig = datum_make_list(datum_make_symbol("datum"), sig);
  }
  datum_t *wrapped_fn = datum_make_pointer(
      __extension__(void *) fn,
      datum_make_list_2(sig, datum_make_symbol("eval_result")));
  *ctxt = namespace_set(*ctxt, datum_make_symbol(name), wrapped_fn);
}

static eval_result_t namespace_eval_prelude(namespace_t *ns) {
  FILE *prelude =
      fmemopen(zlisp_impl_prelude_lisp, zlisp_impl_prelude_lisp_len, "r");
  if (prelude == NULL) {
    return eval_result_make_panic("error while reading the prelude source");
  }

  read_result_t rr;
  for (; read_result_is_ok(rr = datum_read(prelude));) {
    eval_result_t val = datum_eval(rr.ok_value, ns);
    if (eval_result_is_panic(val)) {
      return val;
    }
    if (eval_result_is_ok(val)) {
      return eval_result_make_panic(
          "the program should consist of statements\n");
    }
    ns = val.context_value;
  }

  return eval_result_make_context(ns);
}

eval_result_t namespace_make_prelude() {
  namespace_t *ns = namespace_make();

  namespace_def_special(&ns, "builtin.macro", special_macro);
  namespace_def_special(&ns, "builtin.fn", special_fn);
  namespace_def_special(&ns, "builtin.operator", special_operator);
  namespace_def_special(&ns, "builtin.switch", special_switch);
  namespace_def_special(&ns, "def", special_def);
  namespace_def_special(&ns, "builtin.defn", special_defn);
  namespace_def_special(&ns, "if", special_if);
  namespace_def_special(&ns, "backquote", special_backquote);
  namespace_def_special(&ns, "panic", special_panic);
  namespace_def_special(&ns, "progn", special_progn);
  namespace_def_special(&ns, "require", special_require);

  namespace_def_extern_fn(&ns, "shared-library", builtin_shared_library, 1);
  namespace_def_extern_fn(&ns, "extern-pointer", builtin_extern_pointer, 3);
  namespace_def_extern_fn(&ns, "cons", builtin_cons, 2);
  namespace_def_extern_fn(&ns, "head", builtin_head, 1);
  namespace_def_extern_fn(&ns, "tail", builtin_tail, 1);
  namespace_def_extern_fn(&ns, "eq", builtin_eq, 2);
  namespace_def_extern_fn(&ns, "annotate", builtin_annotate, 1);
  namespace_def_extern_fn(&ns, "is-constant", builtin_is_constant, 1);
  namespace_def_extern_fn(&ns, "repr", builtin_repr, 1);
  namespace_def_extern_fn(&ns, "+", builtin_add, 2);

  return namespace_eval_prelude(ns);
}
