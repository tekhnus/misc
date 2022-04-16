// zlisp interpreter.
#include <zlisp-impl/zlisp-impl.h>

#include <ctype.h>
#include <libgen.h>

#include <stddef.h>
#if INTERFACE
#include <stdint.h>
#include <stdbool.h>
#endif
#include <stdlib.h>
#include <string.h>

enum fdatumype {
  FDATUM_OK,
  FDATUM_PANIC,
};

bool datum_is_the_symbol(datum *d, char *val) {
  return datum_is_symbol(d) && !strcmp(d->symbol_value, val);
}

routine_0 routine_0_make(prog *s, state *ctxt) {
  routine_0 res = {.prog_ = s, .state_ = ctxt};
  return res;
}

routine_0 routine_0_make_null() {
  routine_0 res = {};
  return res;
}

routine_1 routine_1_make_null() {
  routine_1 res = {};
  return res;
}

routine_2 routine_2_make_null() {
  routine_2 res = {};
  return res;
}

bool routine_0_is_null(routine_0 r) { return r.prog_ == NULL && r.state_ == NULL; }
bool routine_1_is_null(routine_1 r) { return routine_0_is_null(r.cur) && r.par == NULL; }
bool routine_2_is_null(routine_2 r) { return routine_1_is_null(r.cur) && r.par == NULL; }

int list_length(datum *seq) {
  if (!datum_is_list(seq)) {
    return -1;
  }
  int res;
  for (res = 0; !datum_is_nil(seq); seq = seq->list_tail, ++res) {
  }
  return res;
}

bool datum_is_nil(datum *e) { return e->type == DATUM_NIL; }

bool datum_is_list(datum *e) {
  return e->type == DATUM_NIL || e->type == DATUM_LIST;
}

bool datum_is_symbol(datum *e) { return e->type == DATUM_SYMBOL; }

bool datum_is_integer(datum *e) { return e->type == DATUM_INTEGER; }

bool datum_is_bytestring(datum *e) { return e->type == DATUM_BYTESTRING; }

bool datum_is_routine_0(datum *e) { return e->type == DATUM_ROUTINE_0; }

bool datum_is_routine_1(datum *e) { return e->type == DATUM_ROUTINE_1; }

bool datum_is_void(datum *e) { return e->type == DATUM_VOID; }

datum *datum_make_nil() {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_NIL;
  return e;
}

datum *datum_make_list(datum *head, datum *tail) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_LIST;
  e->list_head = head;
  e->list_tail = tail;
  return e;
}

datum *datum_make_list_1(datum *head) {
  return datum_make_list(head, datum_make_nil());
}

datum *datum_make_list_2(datum *head, datum *second) {
  return datum_make_list(head, datum_make_list_1(second));
}

datum *datum_make_list_3(datum *head, datum *second, datum *third) {
  return datum_make_list(head, datum_make_list_2(second, third));
}

datum *datum_make_symbol(char *name) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_SYMBOL;
  size_t length = strlen(name);
  e->symbol_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->symbol_value[i] = name[i];
  }
  return e;
}

datum *datum_make_bytestring(char *text) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_BYTESTRING;
  size_t length = strlen(text);
  e->bytestring_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->bytestring_value[i] = text[i];
  }
  return e;
}

datum *datum_make_int(int64_t value) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_INTEGER;
  e->integer_value = value;
  return e;
}

datum *datum_make_routine_0(routine_0 r) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_ROUTINE_0;
  e->routine_0_value = r;
  return e;
}

datum *datum_make_routine_1(routine_1 r) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_ROUTINE_1;
  e->routine_1_value = r;
  return e;
}

datum *datum_make_void() {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_VOID;
  return e;
}

bool read_result_is_ok(read_result x) { return x.type == READ_RESULT_OK; }

bool read_result_is_panic(read_result x) { return x.type == READ_RESULT_PANIC; }

bool read_result_is_eof(read_result x) { return x.type == READ_RESULT_EOF; }

bool read_result_is_right_paren(read_result x) {
  return x.type == READ_RESULT_RIGHT_PAREN;
}

read_result read_result_make_ok(datum *e) {
  read_result result = {.type = READ_RESULT_OK, .ok_value = e};
  return result;
}

read_result read_result_make_panic(char *message) {
  read_result result = {.type = READ_RESULT_PANIC, .panic_message = message};
  return result;
}

read_result read_result_make_eof(void) {
  read_result result = {.type = READ_RESULT_EOF};
  return result;
}

read_result read_result_make_right_paren(void) {
  read_result result = {.type = READ_RESULT_RIGHT_PAREN};
  return result;
}

bool is_whitespace(char c) { return isspace(c) || c == ','; }

bool is_allowed_inside_symbol(char c) {
  return isalnum(c) || c == '.' || c == '-' || c == '_' || c == ':' || c == '+';
}

bool consume_control_sequence(char c, datum **form) {
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
  if (c == '^') {
    *form = datum_make_symbol("hat");
    return true;
  }
  return false;
}

enum token_type {
  TOKEN_DATUM,
  TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_PAREN,
  TOKEN_CONTROL_SEQUENCE,
  TOKEN_ERROR,
  TOKEN_EOF,
};

struct token {
  enum token_type type;
  union {
    datum *datum_value;
    datum *control_sequence_symbol;
    char *error_message;
  };
};

struct token token_read(FILE *strm) {
  char c;
  for (; !feof(strm) && is_whitespace(c = getc(strm));) {
  }
  if (feof(strm)) {
    return (struct token){.type = TOKEN_EOF};
  }
  if (c == ')') {
    return (struct token){.type = TOKEN_RIGHT_PAREN};
  }
  if (c == '(') {
    return (struct token){.type = TOKEN_LEFT_PAREN};
  }
  if (isdigit(c) || c == '-') {
    int64_t sign = 1;
    char h;
    if (c == '-') {
      sign = -1;
      c = getc(strm);
      if (!isdigit(c)) {
        return (struct token){.type = TOKEN_ERROR, .error_message = "expected a number after unary minus"};
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
    return (struct token){.type = TOKEN_DATUM, .datum_value = datum_make_int(sign * val)};
  }
  if (is_allowed_inside_symbol(c)) {
    char *nm = malloc(128);
    nm[0] = c;
    int i;
    char x;
    for (i = 1; !feof(strm) && is_allowed_inside_symbol(x = getc(strm));
         nm[i++] = x) {
    }
    if (!feof(strm)) {
      ungetc(x, strm);
    }
    nm[i] = '\0';
    datum *sym = datum_make_symbol(nm);
    return (struct token){.type = TOKEN_DATUM, .datum_value = sym};
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
        return (struct token){.type = TOKEN_ERROR, .error_message = "unknown escape code"};
      }
      literal[i] = x;
    }
    literal[i] = '\0';
    return (struct token){.type = TOKEN_DATUM, .datum_value = datum_make_bytestring(literal)};
  }
  datum *form;
  if (consume_control_sequence(c, &form)) {
    return (struct token){.type = TOKEN_CONTROL_SEQUENCE, .control_sequence_symbol = form};
  }
  char *err = malloc(1024);
  sprintf(err, "unexpected symbol: 0x%x", c);
  return (struct token){.type = TOKEN_ERROR, .error_message = err};
}

read_result datum_read(FILE *strm) {
  struct token tok = token_read(strm);
  if (tok.type == TOKEN_ERROR) {
    return read_result_make_panic(tok.error_message);
  }
  if (tok.type == TOKEN_EOF) {
    return read_result_make_eof();
  }
  if (tok.type == TOKEN_DATUM) {
    return read_result_make_ok(tok.datum_value);
  }
  if (tok.type == TOKEN_RIGHT_PAREN) {
    return read_result_make_right_paren();
  }
  if (tok.type == TOKEN_LEFT_PAREN) {
    read_result elem;
    datum *list = datum_make_nil();
    datum **end_marker = &list;
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
  if (tok.type == TOKEN_CONTROL_SEQUENCE) {
    read_result v = datum_read(strm);
    if (read_result_is_panic(v)) {
      return v;
    }
    if (!read_result_is_ok(v)) {
      return read_result_make_panic(
          "expected an expression after a control character");
    }
    datum *res = datum_make_list_1(tok.control_sequence_symbol);
    res->list_tail = datum_make_list_1(v.ok_value);
    return read_result_make_ok(res);
  }
  return read_result_make_panic("unhandled token type");
}

fdatum datum_read_one(FILE *stre) {
  read_result rr = datum_read(stre);
  if (read_result_is_panic(rr)) {
    return fdatum_make_panic(rr.panic_message);
  }
  if (read_result_is_right_paren(rr)) {
    return fdatum_make_panic("unmatched right paren");
  }
  if (read_result_is_eof(rr)) {
    return fdatum_make_ok(datum_make_nil());
  }
  return fdatum_make_ok(datum_make_list_1(rr.ok_value));
}

fdatum datum_read_all(FILE *stre) {
  read_result rr;
  datum *res = datum_make_nil();
  datum **resend = &res;
  for (; read_result_is_ok(rr = datum_read(stre));) {
    *resend = datum_make_list(rr.ok_value, datum_make_nil());
    resend = &((*resend)->list_tail);
  }
  if (read_result_is_panic(rr)) {
    return fdatum_make_panic(rr.panic_message);
  }
  if (read_result_is_right_paren(rr)) {
    return fdatum_make_panic("unmatched right paren");
  }
  return fdatum_make_ok(res);
}

char *datum_repr(datum *e) {
  char *buf = malloc(1024 * sizeof(char));
  char *end = buf;
  if (datum_is_integer(e)) {
    sprintf(buf, "%" PRId64, e->integer_value);
  } else if (datum_is_list(e)) {
    end += sprintf(end, "(");
    for (datum *item = e; !datum_is_nil(item); item = item->list_tail) {
      end += sprintf(end, "%s ", datum_repr(item->list_head));
    }
    end += sprintf(end, ")");
  } else if (datum_is_symbol(e)) {
    end += sprintf(end, "%s", e->symbol_value);
  } else if (datum_is_bytestring(e)) {
    end += sprintf(end, "\"%s\"", e->bytestring_value);
  } else if (datum_is_routine_0(e) || datum_is_routine_1(e)) {
    end += sprintf(end, "<form>");
  } else if (datum_is_void(e)) {
    end += sprintf(end, "<void>");
  } else {
    sprintf(buf, "<fmt not implemented>");
  }
  return buf;
}

bool fdatum_is_ok(fdatum result) { return result.type == FDATUM_OK; }

bool fdatum_is_panic(fdatum result) { return result.type == FDATUM_PANIC; }

fdatum fdatum_make_ok(datum *v) {
  fdatum result = {.type = FDATUM_OK, .ok_value = v};
  return result;
}

fdatum fdatum_make_panic(char *message) {
  fdatum result = {.type = FDATUM_PANIC, .panic_message = message};
  return result;
}

fdatum fdatum_get_value(fdatum result) {
  return result;  // A temporary hack for getting the result from lisp :)
}

char *fdatum_get_panic_message(fdatum result) {
  // printf("!!!%s\n", result.panic_message);
  return result.panic_message;
}

LOCAL state *state_make(datum *vars, datum *stack) {
  state *res = malloc(sizeof(state));
  res->vars = vars;
  res->stack = stack;
  return res;
}

state *state_make_fresh() {
  return state_make(datum_make_nil(), datum_make_nil());
}

void state_set_var(state **ns, datum *symbol, datum *value) {
  datum *kv = datum_make_list_2(symbol, value);
  *ns = state_make(datum_make_list(kv, (*ns)->vars), (*ns)->stack);
}

fdatum state_get_var(state *ns, datum *symbol) {
  for (datum *cur = ns->vars; !datum_is_nil(cur); cur = cur->list_tail) {
    datum *entry = cur->list_head;
    if (!strcmp(entry->list_head->symbol_value, symbol->symbol_value)) {
      datum *cell = entry->list_tail;
      return fdatum_make_ok(cell->list_head);
    }
  }
  char *msg = malloc(1024);
  sprintf(msg, "unbound symbol: %s", symbol->symbol_value);
  return fdatum_make_panic(msg);
}

fdatum list_map(fdatum (*fn)(datum *, state *), datum *items, state *ctxt) {
  if (!datum_is_list(items)) {
    return fdatum_make_panic("expected a list");
  }
  datum *evaled_items = datum_make_nil();
  datum **tail = &evaled_items;
  for (datum *arg = items; !datum_is_nil(arg); arg = arg->list_tail) {
    fdatum evaled_arg = fn(arg->list_head, ctxt);
    if (fdatum_is_panic(evaled_arg)) {
      return evaled_arg;
    }
    *tail = datum_make_list_1(evaled_arg.ok_value);
    tail = &((*tail)->list_tail);
  }
  return fdatum_make_ok(evaled_items);
}

datum *state_list_vars(state *ns) {
  datum *result = datum_make_nil();
  datum **nil = &result;
  for (datum *cur = ns->vars; !datum_is_nil(cur); cur = cur->list_tail) {
    datum *keyval = cur->list_head;
    *nil = datum_make_list_1(keyval);
    nil = &((*nil)->list_tail);
  }
  return result;
}

bool datum_eq(datum *x, datum *y) {
  if (datum_is_symbol(x) && datum_is_symbol(y)) {
    if (!strcmp(x->symbol_value, y->symbol_value)) {
      return true;
    }
    return false;
  }
  if (datum_is_integer(x) && datum_is_integer(y)) {
    if (x->integer_value == y->integer_value) {
      return true;
    }
    return false;
  }
  if (datum_is_bytestring(x) && datum_is_bytestring(y)) {
    if (!strcmp(x->bytestring_value, y->bytestring_value)) {
      return true;
    }
    return false;
  }
  if (datum_is_list(x) && datum_is_list(y)) {
    if (datum_is_nil(x) && datum_is_nil(y)) {
      return true;
    }
    if (datum_is_nil(x) || datum_is_nil(y)) {
      return false;
    }
    return datum_eq(x->list_head, y->list_head) &&
           datum_eq(x->list_tail, y->list_tail);
  }
  return false;
}

bool datum_is_constant(datum *d) {
  return (datum_is_integer(d) || datum_is_bytestring(d) ||
          (datum_is_symbol(d) && d->symbol_value[0] == ':'));
}

void state_stack_put(state **ns, datum *value) {
  *ns = state_make((*ns)->vars, datum_make_list(value, (*ns)->stack));
}

datum *state_stack_pop(state **s) {
  if (datum_is_nil((*s)->stack)) {
    fprintf(stderr, "popping from an empty stack is an oh no no\n");
    exit(EXIT_FAILURE);
  }
  datum *res = (*s)->stack->list_head;
  *s = state_make((*s)->vars, (*s)->stack->list_tail);
  return res;
}

void state_stack_new(state **s) {
  state_stack_put(s, datum_make_symbol("__function_call"));
}

datum *state_stack_collect(state **s) {
  datum *form = datum_make_nil();
  for (;;) {
    datum *arg = state_stack_pop(s);
    if (datum_is_the_symbol(arg, "__function_call")) {
      break;
    }
    form = datum_make_list(arg, form);
  }
  return form;
}
