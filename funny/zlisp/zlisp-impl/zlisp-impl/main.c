// zlisp interpreter.
#include <zlisp-impl/main.h>

#include <ctype.h>
#include <dlfcn.h>
#include <ffi.h>
#include <inttypes.h>
#include <libgen.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static bool datum_is_the_symbol(datum_t *d, char *val) {
  return datum_is_symbol(d) && !strcmp(d->symbol_value, val);
}

state_t *state_make() {
  state_t *res = malloc(sizeof(state_t));
  res->type = STATE_END;
  return res;
}

int list_length(datum_t *seq) {
  if (!datum_is_list(seq)) {
    return -1;
  }
  int res;
  for (res = 0; !datum_is_nil(seq); seq = seq->list_tail, ++res)
    ;
  return res;
}

void state_join(state_t *a, state_t *b, state_t *e) {
  if (a->type != STATE_END || b->type != STATE_END) {
    fprintf(stderr, "wrong usage\n");
    exit(1);
  }
  a->type = STATE_NOP;
  a->nop_next = e;
  b->type = STATE_NOP;
  b->nop_next = e;
}

void state_put_const(state_t **begin, datum_t *val) {
  (*begin)->type = STATE_PUT_CONST;
  (*begin)->put_const_value = val;
  (*begin)->put_const_next = state_make();
  *begin = (*begin)->put_const_next;
}

void state_put_var(state_t **begin, datum_t *val) {
  (*begin)->type = STATE_PUT_VAR;
  (*begin)->put_var_value = val;
  (*begin)->put_var_next = state_make();
  *begin = (*begin)->put_var_next;
}

void state_args(state_t **begin) {
  (*begin)->type = STATE_ARGS;
  (*begin)->args_next = state_make();
  *begin = (*begin)->args_next;
}

void state_call(state_t **begin) {
  (*begin)->type = STATE_CALL;
  (*begin)->call_next = state_make();
  *begin = (*begin)->call_next;
}

void state_pop(state_t **begin, datum_t *var) {
  (*begin)->type = STATE_POP;
  (*begin)->pop_var = var;
  (*begin)->pop_next = state_make();
  *begin = (*begin)->pop_next;
}

void state_call_special(state_t **begin,
                        ctx_t (*call_special_func)(datum_t *, namespace_t *)) {
  (*begin)->type = STATE_CALL_SPECIAL;
  (*begin)->call_special_func = call_special_func;
  (*begin)->call_special_next = state_make();
  *begin = (*begin)->call_special_next;
}

void state_return(state_t **begin) {
  (*begin)->type = STATE_RETURN;
  *begin = state_make();
}

ctx_t special_defn(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || datum_is_nil(args->list_tail) ||
      !datum_is_nil(args->list_tail->list_tail)) {
    return ctx_make_panic("defun expects exactly two arguments");
  }
  if (!datum_is_symbol(args->list_head)) {
    return ctx_make_panic("defun requires a symbol as a first argument");
  }
  ctxt = namespace_set_fn(ctxt, args->list_head, args->list_tail->list_head);
  ctxt = namespace_put(ctxt, datum_make_void());
  return ctx_make_ok(ctxt);
}

ctx_t special_require(datum_t *args, namespace_t *ctxt) {
  if (datum_is_nil(args) || !datum_is_nil(args->list_tail)) {
    return ctx_make_panic("require expects a single argument");
  }
  if (!datum_is_bytestring(args->list_head)) {
    return ctx_make_panic("require expected a string");
  }
  ctx_t file_ns = namespace_make_eval_file(args->list_head->bytestring_value);
  if (ctx_is_panic(file_ns)) {
    return file_ns;
  }
  namespace_t *ns = file_ns.ok_value;
  datum_t *imported_bindings = namespace_list(ns);
  for (; !datum_is_nil(imported_bindings);
       imported_bindings = imported_bindings->list_tail) {
    datum_t *sym = imported_bindings->list_head->list_head;
    datum_t *val = imported_bindings->list_head->list_tail->list_head;

    if (!strcmp("this-directory", sym->symbol_value)) {
      continue;
    }
    ctxt = namespace_set(ctxt, sym, val);
  }
  ctxt = namespace_put(ctxt, datum_make_void());
  return ctx_make_ok(ctxt);
}

ctx_t special_fn(datum_t *args, namespace_t *ctxt);

char *state_extend_backquoted(state_t **begin, datum_t *stmt);
char *state_extend(state_t **begin, datum_t *stmt) {
  if ((*begin)->type != STATE_END) {
    return "expected an end state";
  }
  if (!datum_is_list(stmt)) {
    state_put_var(begin, stmt);
    return NULL;
  }
  if (datum_is_nil(stmt)) {
    return "an empty list is not a statement";
  }

  if (datum_is_symbol(stmt->list_head)) {
    char *sym = stmt->list_head->symbol_value;
    if (!strcmp(sym, "if")) {
      if (list_length(stmt->list_tail) != 3) {
        return "if should have three args";
      }
      char *err;
      err = state_extend(begin, stmt->list_tail->list_head);
      if (err != NULL) {
        return err;
      }
      (*begin)->type = STATE_IF;

      state_t *true_end = state_make(), *false_end = state_make();
      (*begin)->if_true = true_end;
      (*begin)->if_false = false_end;
      err = state_extend(&true_end, stmt->list_tail->list_tail->list_head);
      if (err != NULL) {
        return err;
      }
      err = state_extend(&false_end,
                         stmt->list_tail->list_tail->list_tail->list_head);
      if (err != NULL) {
        return err;
      }
      *begin = state_make();
      state_join(true_end, false_end, *begin);
      return NULL;
    }
    if (!strcmp(sym, "progn")) {
      state_put_const(begin, datum_make_void());
      for (datum_t *rest = stmt->list_tail; !datum_is_nil(rest);
           rest = rest->list_tail) {
        state_pop(begin, NULL);
        datum_t *step = rest->list_head;
        char *err = state_extend(begin, step);
        if (err != NULL) {
          return err;
        }
      }
      return NULL;
    }
    if (!strcmp(sym, "quote")) {
      if (list_length(stmt->list_tail) != 1) {
        return "quote should have a single arg";
      }
      state_put_const(begin, stmt->list_tail->list_head);
      return NULL;
    }
    if (!strcmp(sym, "def")) {
      if (list_length(stmt->list_tail) != 2) {
        return "def should have two args";
      }
      char *err = state_extend(begin, stmt->list_tail->list_tail->list_head);
      if (err != NULL) {
        return err;
      }
      state_pop(begin, stmt->list_tail->list_head);
      state_put_const(begin, datum_make_void());
      return NULL;
    }
    if (!strcmp(sym, "builtin.defn")) {
      if (list_length(stmt->list_tail) != 2) {
        return "defn should have two args";
      }
      state_args(begin);
      state_put_const(begin, stmt->list_tail->list_head);
      state_put_const(begin, stmt->list_tail->list_tail->list_head);
      state_call_special(begin, special_defn);
      return NULL;
    }
    if (!strcmp(sym, "builtin.fn")) {
      if (list_length(stmt->list_tail) != 1) {
        return "fn should have one arg";
      }
      state_args(begin);
      state_put_const(begin, stmt->list_tail->list_head);
      state_call_special(begin, special_fn);
      return NULL;
    }
    if (!strcmp(sym, "require")) {
      if (list_length(stmt->list_tail) != 1) {
        return "require should have a single arg";
      }
      state_args(begin);
      char *err = state_extend(begin, stmt->list_tail->list_head);
      if (err != NULL) {
        return err;
      }
      state_call_special(begin, special_require);
      return NULL;
    }
    if (!strcmp(sym, "return")) {
      if (list_length(stmt->list_tail) != 1) {
        return "return should have a single arg";
      }
      char *err = state_extend(begin, stmt->list_tail->list_head);
      if (err != NULL) {
        return err;
      }
      state_return(begin);
    }
    if (!strcmp(sym, "backquote")) {
      if (list_length(stmt->list_tail) != 1) {
        return "backquote should have a single arg";
      }
      return state_extend_backquoted(begin, stmt->list_tail->list_head);
    }
  }

  datum_t *fn = stmt->list_head;
  datum_t *real_fn;
  bool hash;
  if (datum_is_list(fn) && !datum_is_nil(fn) &&
      datum_is_the_symbol(fn->list_head, "hash")) {
    if (list_length(fn) != 2) {
      return "hash should have a single argument";
    }
    real_fn = fn->list_tail->list_head;
    hash = true;
  } else {
    real_fn = fn;
    hash = false;
  }
  state_extend(begin, real_fn);
  state_args(begin);
  for (datum_t *rest_args = stmt->list_tail; !datum_is_nil(rest_args);
       rest_args = rest_args->list_tail) {
    datum_t *arg = rest_args->list_head;
    if (hash) {
      state_put_const(begin, arg);
    } else {
      state_extend(begin, arg);
    }
  }
  state_call(begin);
  return NULL;
}

char *state_extend_backquoted(state_t **begin, datum_t *stmt) {
  if (!datum_is_list(stmt)) {
    state_put_const(begin, stmt);
    return NULL;
  }
  state_put_var(begin, datum_make_symbol("list"));
  state_args(begin);
  for (datum_t *rest_elems = stmt; !datum_is_nil(rest_elems);
       rest_elems = rest_elems->list_tail) {
    datum_t *elem = rest_elems->list_head;
    char *err;
    if (datum_is_list(elem) && list_length(elem) == 2 &&
        datum_is_the_symbol(elem->list_head, "tilde")) {
      err = state_extend(begin, elem->list_tail->list_head);
    } else {
      err = state_extend_backquoted(begin, elem);
    }
    if (err != NULL) {
      return err;
    }
  }
  state_call(begin);
  return NULL;
}

char *state_init(state_t *s, datum_t *stmt) { return state_extend(&s, stmt); }

char *state_init_fn_body(state_t *s, datum_t *stmt) {
  state_pop(&s, datum_make_symbol("args"));
  return state_extend(&s, stmt); }

ctx_t special_fn(datum_t *args, namespace_t *ctxt) {
  if (list_length(args) != 1) {
    return ctx_make_panic("fn expects a single argument");
  }
  state_t *s = state_make();
  char *err = state_init_fn_body(s, args->list_head);
  if (err != NULL) {
    return ctx_make_panic(err);
  }
  ctxt = namespace_put(ctxt, datum_make_operator(s, ctxt));
  return ctx_make_ok(ctxt);
}

val_t datum_eval_primitive(datum_t *e, namespace_t *ctxt) {
  if (datum_is_integer(e) || datum_is_bytestring(e)) {
    return val_make_ok(e);
  }
  if (datum_is_symbol(e)) {
    if (e->symbol_value[0] == ':') {
      return val_make_ok(e);
    }
    return namespace_get(ctxt, e);
  }
  return val_make_panic("not a primitive");
}

val_t pointer_call(datum_t *f, datum_t *args, namespace_t *ctxt);
static ctx_t state_eval(state_t *s, namespace_t *ctxt) {
  state_t *sstack[1024];
  namespace_t *cstack[1024];
  int next_index = 0;
  for (;;) {
    switch (s->type) {
    case STATE_END: {
      return ctx_make_ok(ctxt);
    } break;
    case STATE_NOP: {
      s = s->nop_next;
    } break;
    case STATE_IF: {
      val_t c = namespace_peek(ctxt);
      ctxt = namespace_pop(ctxt);
      if (val_is_panic(c)) {
        return ctx_make_panic(c.panic_message);
      }
      if (!datum_is_nil(c.ok_value)) {
        s = s->if_true;
      } else {
        s = s->if_false;
      }
    } break;
    case STATE_PUT_CONST: {
      ctxt = namespace_put(ctxt, s->put_const_value);
      s = s->put_const_next;
    } break;
    case STATE_PUT_VAR: {
      val_t er = datum_eval_primitive(s->put_var_value, ctxt);
      if (val_is_panic(er)) {
        return ctx_make_panic(er.panic_message);
      }
      ctxt = namespace_put(ctxt, er.ok_value);
      s = s->put_var_next;
    } break;
    case STATE_POP: {
      val_t v = namespace_peek(ctxt);
      if (val_is_panic(v)) {
        return ctx_make_panic(v.panic_message);
      }
      ctxt = namespace_pop(ctxt);
      if (s->pop_var != NULL) {
        ctxt = namespace_set(ctxt, s->pop_var, v.ok_value);
      }
      s = s->pop_next;
    } break;
    case STATE_ARGS: {
      ctxt = namespace_put(ctxt, datum_make_symbol("__function_call"));
      s = s->args_next;
    } break;
    case STATE_CALL: {
      datum_t *args = datum_make_nil();
      val_t arg;
      for (;;) {
        arg = namespace_peek(ctxt);
        ctxt = namespace_pop(ctxt);
        if (val_is_panic(arg)) {
          return ctx_make_panic(arg.panic_message);
        }
        if (datum_is_the_symbol(arg.ok_value, "__function_call")) {
          break;
        }
        args = datum_make_list(arg.ok_value, args);
      }
      val_t fn = namespace_peek(ctxt);
      ctxt = namespace_pop(ctxt);
      if (val_is_panic(fn)) {
        return ctx_make_panic(fn.panic_message);
      }
      if (datum_is_operator(fn.ok_value)) {
        sstack[next_index] = s->call_next;
        cstack[next_index] = ctxt;
        ++next_index;
        ctxt = fn.ok_value->operator_context;
	ctxt = namespace_put(ctxt, args);
        s = fn.ok_value->operator_state;
      } else if (datum_is_pointer(fn.ok_value)) {
        val_t res = pointer_call(fn.ok_value, args, ctxt);
        if (val_is_panic(res)) {
          return ctx_make_panic(res.panic_message);
        }
        ctxt = namespace_put(ctxt, res.ok_value);
        s = s->call_next;
      } else {
        return ctx_make_panic("non-callable datum");
      }
    } break;
    case STATE_RETURN: {
      val_t res = namespace_peek(ctxt);
      if (val_is_panic(res)) {
        return ctx_make_panic(res.panic_message);
      }
      --next_index;
      s = sstack[next_index];
      ctxt = cstack[next_index];
      ctxt = namespace_put(ctxt, res.ok_value);
    } break;
    case STATE_CALL_SPECIAL: {
      datum_t *sargs = datum_make_nil();
      val_t sarg;
      while (sarg = namespace_peek(ctxt), ctxt = namespace_pop(ctxt),
             !(val_is_ok(sarg) && datum_is_symbol(sarg.ok_value) &&
               !strcmp(sarg.ok_value->symbol_value, "__function_call"))) {
        sargs = datum_make_list(sarg.ok_value, sargs);
      }
      ctx_t sres = s->call_special_func(sargs, ctxt);
      if (ctx_is_panic(sres)) {
        return sres;
      }
      ctxt = sres.ok_value;
      s = s->call_special_next;
    } break;
    default: {
      return ctx_make_panic("unhandled state type");
    } break;
    }
  }
}

bool datum_is_nil(datum_t *e) { return e->type == DATUM_NIL; }

bool datum_is_list(datum_t *e) {
  return e->type == DATUM_NIL || e->type == DATUM_LIST;
}

bool datum_is_symbol(datum_t *e) { return e->type == DATUM_SYMBOL; }

bool datum_is_integer(datum_t *e) { return e->type == DATUM_INTEGER; }

bool datum_is_bytestring(datum_t *e) { return e->type == DATUM_BYTESTRING; }

bool datum_is_operator(datum_t *e) { return e->type == DATUM_OPERATOR; }

bool datum_is_pointer(datum_t *e) { return e->type == DATUM_POINTER; }

bool datum_is_void(datum_t *e) { return e->type == DATUM_VOID; }

datum_t *datum_make_nil() {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_NIL;
  return e;
}

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

datum_t *datum_make_operator(state_t *s, namespace_t *lexical_bindings) {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_OPERATOR;
  e->operator_state = s;
  e->operator_context = lexical_bindings;
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

datum_t *datum_make_void() {
  datum_t *e = malloc(sizeof(datum_t));
  e->type = DATUM_VOID;
  return e;
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
  if (c == '!') {
    *form = datum_make_symbol("bang");
    return true;
  }
  if (c == '#') {
    *form = datum_make_symbol("hash");
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
  char *err = malloc(1024);
  sprintf(err, "unexpected symbol: 0x%x", c);
  return read_result_make_panic(err);
}

char *datum_repr(datum_t *e) {
  char *buf = malloc(1024 * sizeof(char));
  char *end = buf;
  if (datum_is_integer(e)) {
    sprintf(buf, "%" PRId64, e->integer_value);
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
  } else if (datum_is_pointer(e)) {
    end += sprintf(end, "<externcdata %p %s>", e->pointer_value,
                   datum_repr(e->pointer_descriptor));
  } else if (datum_is_void(e)) {
    end += sprintf(end, "<void>");
  } else {
    sprintf(buf, "<fmt not implemented>");
  }
  return buf;
}

bool val_is_ok(val_t result) { return result.type == VAL_OK; }

bool val_is_panic(val_t result) { return result.type == VAL_PANIC; }

val_t val_make_ok(datum_t *v) {
  val_t result = {.type = VAL_OK, .ok_value = v};
  return result;
}

val_t val_make_panic(char *message) {
  val_t result = {.type = VAL_PANIC, .panic_message = message};
  return result;
}

bool ctx_is_ok(ctx_t result) { return result.type == CTX_OK; }

bool ctx_is_panic(ctx_t result) { return result.type == CTX_PANIC; }

ctx_t ctx_make_ok(namespace_t *v) {
  ctx_t result = {.type = CTX_OK, .ok_value = v};
  return result;
}

ctx_t ctx_make_panic(char *message) {
  ctx_t result = {.type = CTX_PANIC, .panic_message = message};
  return result;
}

namespace_t *namespace_make(datum_t *vars, datum_t *stack) {
  namespace_t *res = malloc(sizeof(namespace_t));
  res->vars = vars;
  res->stack = stack;
  return res;
}

namespace_t *namespace_make_empty() {
  return namespace_make(datum_make_nil(), datum_make_nil());
}

namespace_t *namespace_set(namespace_t *ns, datum_t *symbol, datum_t *value) {
  datum_t *kv = datum_make_list_3(symbol, datum_make_symbol(":value"), value);
  return namespace_make(datum_make_list(kv, ns->vars), ns->stack);
}

namespace_t *namespace_set_fn(namespace_t *ns, datum_t *symbol,
                              datum_t *value) {
  state_t *s = state_make();
  char *err = state_init_fn_body(s, value);
  if (err != NULL) {
    fprintf(stderr, "bad function def\n");
    exit(EXIT_FAILURE);
  }
  datum_t *fn = datum_make_operator(s, NULL);
  datum_t *kv = datum_make_list_3(symbol, datum_make_symbol(":fn"), fn);
  return namespace_make(datum_make_list(kv, ns->vars), ns->stack);
}

datum_t *namespace_cell_get_value(datum_t *cell, namespace_t *ns) {
  datum_t *raw_value = cell->list_tail->list_head;
  datum_t *keyval;
  if (!strcmp(cell->list_head->symbol_value, ":value")) {
    return raw_value;
  } else if (!strcmp(cell->list_head->symbol_value, ":fn")) {
    if (!datum_is_operator(raw_value)) {
      fprintf(stderr, "namespace implementation error");
      exit(EXIT_FAILURE);
    }
    return datum_make_operator(raw_value->operator_state, ns);
  } else {
    fprintf(stderr, "namespace implementation error");
    exit(EXIT_FAILURE);
  }
}

val_t namespace_get(namespace_t *ns, datum_t *symbol) {
  for (datum_t *cur = ns->vars; !datum_is_nil(cur); cur = cur->list_tail) {
    datum_t *entry = cur->list_head;
    if (!strcmp(entry->list_head->symbol_value, symbol->symbol_value)) {
      datum_t *cell = entry->list_tail;
      return val_make_ok(namespace_cell_get_value(cell, ns));
    }
  }
  char *msg = malloc(1024);
  sprintf(msg, "unbound symbol: %s", symbol->symbol_value);
  return val_make_panic(msg);
}

namespace_t *namespace_put(namespace_t *ns, datum_t *value) {
  return namespace_make(ns->vars, datum_make_list(value, ns->stack));
}

val_t namespace_peek(namespace_t *ns) {
  if (datum_is_nil(ns->stack)) {
    return val_make_panic("peek failed");
  }
  return val_make_ok(ns->stack->list_head);
}

namespace_t *namespace_pop(namespace_t *ns) {
  if (datum_is_nil(ns->stack)) {
    fprintf(stderr, "cannot pop from an empty stack\n");
    exit(EXIT_FAILURE);
  }
  return namespace_make(ns->vars, ns->stack->list_tail);
}

val_t list_map(val_t (*fn)(datum_t *, namespace_t *), datum_t *items,
               namespace_t *ctxt) {
  if (!datum_is_list(items)) {
    return val_make_panic("expected a list");
  }
  datum_t *evaled_items = datum_make_nil();
  datum_t **tail = &evaled_items;
  for (datum_t *arg = items; !datum_is_nil(arg); arg = arg->list_tail) {
    val_t evaled_arg = fn(arg->list_head, ctxt);
    if (val_is_panic(evaled_arg)) {
      return evaled_arg;
    }
    *tail = datum_make_list_1(evaled_arg.ok_value);
    tail = &((*tail)->list_tail);
  }
  return val_make_ok(evaled_items);
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
  if (!strcmp(definition->symbol_value, "val")) {
    *type = malloc(sizeof(ffi_type));
    (*type)->type = FFI_TYPE_STRUCT;
    (*type)->size = 0; // Lost 5 hours debugging non-deterministic failures on
                       // Mac before adding this line.
    (*type)->alignment = 0;
    ffi_type **elements = malloc(4 * sizeof(ffi_type *));
    elements[0] = &ffi_type_sint;
    elements[1] = &ffi_type_pointer;
    elements[2] = &ffi_type_pointer;
    elements[3] = NULL;
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

val_t pointer_ffi_call(datum_t *f, ffi_cif *cif, void **cargs) {
  void (*fn_ptr)(void) = __extension__(void (*)(void))(f->pointer_value);
  char *rettype = f->pointer_descriptor->list_tail->list_head->symbol_value;

  if (!strcmp(rettype, "pointer")) {
    void *res = malloc(sizeof(void *));
    ffi_call(cif, fn_ptr, res, cargs);
    return val_make_ok(datum_make_pointer_to_pointer(res));
  }
  if (!strcmp(rettype, "sizet")) {
    void *res = malloc(sizeof(size_t));
    ffi_call(cif, fn_ptr, res, cargs);
    return val_make_ok(datum_make_int(*(int64_t *)res));
  }
  if (!strcmp(rettype, "int")) {
    void *res = malloc(sizeof(int));
    ffi_call(cif, fn_ptr, res, cargs);
    return val_make_ok(datum_make_int(*(int64_t *)res));
  }
  if (!strcmp(rettype, "val")) {
    val_t res;
    ffi_call(cif, fn_ptr, &res, cargs);
    if (val_is_panic(res)) {
      return val_make_panic(res.panic_message);
    }
    return val_make_ok(res.ok_value);
  }
  return val_make_panic("unknown return type for extern func");
}

val_t pointer_call(datum_t *f, datum_t *args, namespace_t *ctxt) {
  ffi_cif cif;
  char *err = NULL;
  err = pointer_ffi_init_cif(f, &cif);
  if (err != NULL) {
    return val_make_panic(err);
  }
  void *cargs[32];
  err = pointer_ffi_serialize_args(f, args, cargs);
  if (err != NULL) {
    return val_make_panic(err);
  }
  return pointer_ffi_call(f, &cif, cargs);
}

ctx_t datum_eval(datum_t *e, namespace_t *ctxt) {
  state_t *s = state_make();
  char *err = state_init(s, e);
  if (err != NULL) {
    return ctx_make_panic(err);
  }
  return state_eval(s, ctxt);
}

static val_t datum_expand(datum_t *e, namespace_t *ctxt) {
  if (!datum_is_list(e) || datum_is_nil(e)) {
    return val_make_ok(e);
  }
  if (!datum_is_symbol(e->list_head) ||
      strcmp(e->list_head->symbol_value, "bang")) {
    return list_map(datum_expand, e, ctxt);
  }
  if (datum_is_nil(e->list_tail) || !datum_is_nil(e->list_tail->list_tail)) {
    return val_make_panic("! should be used with a single arg");
  }
  val_t exp = datum_expand(e->list_tail->list_head, ctxt);
  if (val_is_panic(exp)) {
    return exp;
  }
  ctx_t ev = datum_eval(exp.ok_value, ctxt);
  if (ctx_is_panic(ev)) {
    return val_make_panic(ev.panic_message);
  }
  val_t res = namespace_peek(ev.ok_value);
  if (val_is_panic(res)) {
    return res;
  }
  return val_make_ok(res.ok_value);
}

val_t builtin_concat_bytestrings(datum_t *x, datum_t *y) {
  if (!datum_is_bytestring(x) || !datum_is_bytestring(y)) {
    return val_make_panic("expected integers");
  }
  char *buf =
      malloc(strlen(x->bytestring_value) + strlen(y->bytestring_value) + 1);
  buf[0] = '\0';
  strcat(buf, x->bytestring_value);
  strcat(buf, y->bytestring_value);
  return val_make_ok(datum_make_bytestring(buf));
}

val_t builtin_add(datum_t *x, datum_t *y) {
  if (!datum_is_integer(x) || !datum_is_integer(y)) {
    return val_make_panic("expected integers");
  }
  return val_make_ok(datum_make_int(x->integer_value + y->integer_value));
}

val_t builtin_cons(datum_t *head, datum_t *tail) {
  if (!datum_is_list(tail)) {
    return val_make_panic("cons requires a list as a second argument");
  }
  return val_make_ok(datum_make_list(head, tail));
}

val_t builtin_head(datum_t *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return val_make_panic("car expects a nonempty list");
  }
  return val_make_ok(list->list_head);
}

val_t builtin_tail(datum_t *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return val_make_panic("cdr expects a nonempty list");
  }
  return val_make_ok(list->list_tail);
}

datum_t *namespace_list(namespace_t *ns) {
  datum_t *result = datum_make_nil();
  datum_t **nil = &result;
  for (datum_t *cur = ns->vars; !datum_is_nil(cur); cur = cur->list_tail) {
    datum_t *entry = cur->list_head;
    datum_t *key = entry->list_head;
    datum_t *cell = entry->list_tail;
    datum_t *val = namespace_cell_get_value(cell, ns);
    datum_t *keyval = datum_make_list_2(key, val);
    *nil = datum_make_list_1(keyval);
    nil = &((*nil)->list_tail);
  }
  return result;
}

static ctx_t stream_eval(FILE *stream, namespace_t *ctxt) {
  read_result_t rr;
  for (; read_result_is_ok(rr = datum_read(stream));) {
    // printf("running %s\n", datum_repr(rr.ok_value));
    val_t exp = datum_expand(rr.ok_value, ctxt);
    if (val_is_panic(exp)) {
      return ctx_make_panic(exp.panic_message);
    }
    // printf("expanded to %s\n", datum_repr(exp.ok_value));
    state_t *s = state_make();
    state_init(s, exp.ok_value);
    ctx_t val = state_eval(s, ctxt);
    if (ctx_is_panic(val)) {
      return val;
    }
    ctxt = val.ok_value;
  }
  if (read_result_is_panic(rr)) {
    return ctx_make_panic(rr.panic_message);
  }
  if (read_result_is_right_paren(rr)) {
    return ctx_make_panic("unmatched right paren");
  }
  return ctx_make_ok(ctxt);
}

val_t builtin_shared_library(datum_t *library_name) {
  if (!datum_is_bytestring(library_name)) {
    return val_make_panic("load-shared-library expects a bytestring");
  }
  void **handle = malloc(sizeof(void *));
  *handle = dlopen(library_name->bytestring_value, RTLD_LAZY);
  char *err = dlerror();
  if (!*handle) {
    return val_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                         datum_make_bytestring(err)));
  }
  return val_make_ok(datum_make_list_2(datum_make_symbol(":ok"),
                                       datum_make_pointer_to_pointer(handle)));
}

val_t builtin_extern_pointer(datum_t *shared_library, datum_t *name,
                             datum_t *descriptor) {
  if (!datum_is_pointer(shared_library) ||
      !datum_is_symbol(shared_library->pointer_descriptor) ||
      strcmp(shared_library->pointer_descriptor->symbol_value, "pointer")) {
    return val_make_panic("wrong externcdata usage");
  }
  void *handle = *(void **)shared_library->pointer_value;
  if (!datum_is_bytestring(name)) {
    return val_make_panic("externcdata expected a string");
  }
  void *call_ptr = dlsym(handle, name->bytestring_value);
  char *err = dlerror();
  if (err != NULL) {
    return val_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                         datum_make_bytestring(err)));
  }
  return val_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"), datum_make_pointer(call_ptr, descriptor)));
}

val_t builtin_repr(datum_t *v) {
  return val_make_ok(datum_make_bytestring(datum_repr(v)));
}

val_t builtin_eq(datum_t *x, datum_t *y) {
  datum_t *t = datum_make_list_1(datum_make_nil());
  datum_t *f = datum_make_nil();
  if (datum_is_symbol(x) && datum_is_symbol(y)) {
    if (!strcmp(x->symbol_value, y->symbol_value)) {
      return val_make_ok(t);
    }
    return val_make_ok(f);
  }
  if (datum_is_integer(x) && datum_is_integer(y)) {
    if (x->integer_value == y->integer_value) {
      return val_make_ok(t);
    }
    return val_make_ok(f);
  }

  return val_make_panic("eq can't compare those things");
}

val_t builtin_annotate(datum_t *arg_value) {
  char *type;
  if (datum_is_list(arg_value)) {
    type = ":list";
  } else if (datum_is_symbol(arg_value)) {
    type = ":symbol";
  } else if (datum_is_bytestring(arg_value)) {
    type = ":bytestring";
  } else if (datum_is_integer(arg_value)) {
    type = ":integer";
  } else if (datum_is_operator(arg_value)) {
    type = ":operator";
  } else if (datum_is_pointer(arg_value)) {
    type = ":pointer";
  } else {
    return val_make_panic("incomplete implementation of type");
  }
  return val_make_ok(datum_make_list_2(datum_make_symbol(type), arg_value));
}

val_t builtin_is_constant(datum_t *arg_value) {
  if (datum_is_integer(arg_value) || datum_is_bytestring(arg_value) ||
      (datum_is_symbol(arg_value) && arg_value->symbol_value[0] == ':')) {
    return val_make_ok(datum_make_list_1(datum_make_nil()));
  }
  return val_make_ok(datum_make_nil());
}

val_t builtin_panic(datum_t *arg_value) {
  if (!datum_is_bytestring(arg_value)) {
    return val_make_panic("panic expects a bytestring");
  }
  return val_make_panic(arg_value->bytestring_value);
}

void namespace_def_extern_fn(namespace_t **ctxt, char *name, val_t (*fn)(),
                             int cnt) {
  datum_t *sig = datum_make_nil();
  for (int i = 0; i < cnt; ++i) {
    sig = datum_make_list(datum_make_symbol("datum"), sig);
  }
  datum_t *wrapped_fn =
      datum_make_pointer(__extension__(void *) fn,
                         datum_make_list_2(sig, datum_make_symbol("val")));
  *ctxt = namespace_set(*ctxt, datum_make_symbol(name), wrapped_fn);
}

ctx_t namespace_make_prelude() {
  namespace_t *ns = namespace_make_empty();

  namespace_def_extern_fn(&ns, "panic", builtin_panic, 1);
  namespace_def_extern_fn(&ns, "shared-library", builtin_shared_library, 1);
  namespace_def_extern_fn(&ns, "extern-pointer", builtin_extern_pointer, 3);
  namespace_def_extern_fn(&ns, "cons", builtin_cons, 2);
  namespace_def_extern_fn(&ns, "head", builtin_head, 1);
  namespace_def_extern_fn(&ns, "tail", builtin_tail, 1);
  namespace_def_extern_fn(&ns, "eq", builtin_eq, 2);
  namespace_def_extern_fn(&ns, "annotate", builtin_annotate, 1);
  namespace_def_extern_fn(&ns, "is-constant", builtin_is_constant, 1);
  namespace_def_extern_fn(&ns, "repr", builtin_repr, 1);
  namespace_def_extern_fn(&ns, "concat-bytestrings", builtin_concat_bytestrings,
                          2);
  namespace_def_extern_fn(&ns, "+", builtin_add, 2);

  FILE *prelude =
      fmemopen(zlisp_impl_prelude_lisp, zlisp_impl_prelude_lisp_len, "r");
  if (prelude == NULL) {
    return ctx_make_panic("error while reading the prelude source");
  }

  return stream_eval(prelude, ns);
}

ctx_t namespace_make_eval_file(char *filename) {
  FILE *module = fopen(filename, "r");
  if (module == NULL) {
    return ctx_make_panic("error while opening the required file");
  }
  ctx_t prelude = namespace_make_prelude();
  if (ctx_is_panic(prelude)) {
    return prelude;
  }
  namespace_t *ns = prelude.ok_value;
  char filename_copy[1024];
  strcpy(filename_copy, filename);
  datum_t *new_this_directory = datum_make_bytestring(dirname(filename_copy));
  ns = namespace_set(ns, datum_make_symbol("this-directory"),
                     new_this_directory);
  ctx_t ns_ = stream_eval(module, ns);
  return ns_;
}
