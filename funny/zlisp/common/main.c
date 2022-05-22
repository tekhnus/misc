// zlisp interpreter.
#include <extern.h>
#if INTERFACE
#include <stdbool.h>
#include <stdint.h>
#endif
#include <ctype.h>
#include <libgen.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>

enum fdatumype {
  FDATUM_OK,
  FDATUM_PANIC,
};

EXPORT bool datum_is_the_symbol(datum *d, char *val) {
  return datum_is_symbol(d) && !strcmp(d->symbol_value, val);
}

EXPORT int list_length(datum *seq) {
  if (!datum_is_list(seq)) {
    return -1;
  }
  int res;
  for (res = 0; !datum_is_nil(seq); seq = seq->list_tail, ++res) {
  }
  return res;
}

EXPORT bool datum_is_nil(datum *e) { return e->type == DATUM_NIL; }

EXPORT bool datum_is_list(datum *e) {
  return e->type == DATUM_NIL || e->type == DATUM_LIST;
}

EXPORT bool datum_is_symbol(datum *e) { return e->type == DATUM_SYMBOL; }

EXPORT bool datum_is_integer(datum *e) { return e->type == DATUM_INTEGER; }

EXPORT bool datum_is_bytestring(datum *e) {
  return e->type == DATUM_BYTESTRING;
}

EXPORT datum *datum_make_nil() {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_NIL;
  return e;
}

EXPORT datum *datum_make_list(datum *head, datum *tail) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_LIST;
  e->list_head = head;
  e->list_tail = tail;
  return e;
}

EXPORT datum *datum_make_list_1(datum *head) {
  return datum_make_list(head, datum_make_nil());
}

EXPORT datum *datum_make_list_2(datum *head, datum *second) {
  return datum_make_list(head, datum_make_list_1(second));
}

LOCAL datum *datum_make_list_3(datum *head, datum *second, datum *third) {
  return datum_make_list(head, datum_make_list_2(second, third));
}

LOCAL datum *datum_make_list_4(datum *head, datum *second, datum *third,
                         datum *fourth) {
  return datum_make_list(
      head,
      datum_make_list(
                      second, datum_make_list_2(third, fourth)));
}

LOCAL datum *datum_make_list_5(datum *head, datum *second, datum *third,
                         datum *fourth, datum *fifth) {
  return datum_make_list(
      head,
      datum_make_list(
          second, datum_make_list(third, datum_make_list_2(fourth, fifth))));
}

EXPORT datum *datum_make_symbol(char *name) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_SYMBOL;
  size_t length = strlen(name);
  e->symbol_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->symbol_value[i] = name[i];
  }
  return e;
}

EXPORT datum *datum_make_bytestring(char *text) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_BYTESTRING;
  size_t length = strlen(text);
  e->bytestring_value = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->bytestring_value[i] = text[i];
  }
  return e;
}

EXPORT datum *datum_make_int(int64_t value) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_INTEGER;
  e->integer_value = value;
  return e;
}

EXPORT bool read_result_is_ok(read_result x) {
  return x.type == READ_RESULT_OK;
}

EXPORT bool read_result_is_panic(read_result x) {
  return x.type == READ_RESULT_PANIC;
}

LOCAL bool read_result_is_eof(read_result x) { return x.type == READ_RESULT_EOF; }

EXPORT bool read_result_is_right_paren(read_result x) {
  return x.type == READ_RESULT_RIGHT_PAREN;
}

LOCAL read_result read_result_make_ok(datum *e) {
  read_result result = {.type = READ_RESULT_OK, .ok_value = e};
  return result;
}

LOCAL read_result read_result_make_panic(char *message) {
  read_result result = {.type = READ_RESULT_PANIC, .panic_message = message};
  return result;
}

LOCAL read_result read_result_make_eof(void) {
  read_result result = {.type = READ_RESULT_EOF};
  return result;
}

LOCAL read_result read_result_make_right_paren(void) {
  read_result result = {.type = READ_RESULT_RIGHT_PAREN};
  return result;
}

LOCAL bool is_whitespace(char c) { return isspace(c) || c == ','; }

LOCAL bool is_allowed_inside_symbol(char c) {
  return isalnum(c) || c == '.' || c == '-' || c == '_' || c == ':' || c == '+';
}

LOCAL bool consume_control_sequence(char c, datum **form) {
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

LOCAL struct token token_read(FILE *strm) {
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
        return (struct token){.type = TOKEN_ERROR,
                              .error_message =
                                  "expected a number after unary minus"};
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
    return (struct token){.type = TOKEN_DATUM,
                          .datum_value = datum_make_int(sign * val)};
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
        return (struct token){.type = TOKEN_ERROR,
                              .error_message = "unknown escape code"};
      }
      literal[i] = x;
    }
    literal[i] = '\0';
    return (struct token){.type = TOKEN_DATUM,
                          .datum_value = datum_make_bytestring(literal)};
  }
  datum *form;
  if (consume_control_sequence(c, &form)) {
    return (struct token){.type = TOKEN_CONTROL_SEQUENCE,
                          .control_sequence_symbol = form};
  }
  char *err = malloc(1024);
  sprintf(err, "unexpected symbol: 0x%x", c);
  return (struct token){.type = TOKEN_ERROR, .error_message = err};
}

EXPORT read_result datum_read(FILE *strm) {
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

fdatum datum_read_one(FILE *stre) { // used in lisp
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

EXPORT char *datum_repr(datum *e) {
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
  } else {
    sprintf(buf, "<fmt not implemented>");
  }
  return buf;
}

EXPORT bool fdatum_is_panic(fdatum result) {
  return result.type == FDATUM_PANIC;
}

EXPORT fdatum fdatum_make_ok(datum *v) {
  fdatum result = {.type = FDATUM_OK, .ok_value = v};
  return result;
}

EXPORT fdatum fdatum_make_panic(char *message) {
  fdatum result = {.type = FDATUM_PANIC, .panic_message = message};
  return result;
}

char *fdatum_get_panic_message(fdatum result) { // used in lisp
  return result.panic_message;
}

state *state_make(datum *vars, datum *stack) {
  state *res = malloc(sizeof(state));
  res->vars = vars;
  res->stack = stack;
  return res;
}

EXPORT state *state_make_fresh() {
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

EXPORT bool datum_eq(datum *x, datum *y) {
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

EXPORT bool datum_is_constant(datum *d) {
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

EXPORT prog_slice prog_slice_make(size_t capacity) {
  prog_slice res;
  res.begin = malloc(capacity * sizeof(prog));
  res.length = 0;
  res.capacity = capacity;
  return res;
}

EXPORT prog *prog_slice_append_new(prog_slice *s) {
  if (s->length == s->capacity) {
    fprintf(stderr, "prog slice capacity overflow %zu\n", s->capacity);
    exit(EXIT_FAILURE);
  }
  prog *p = s->begin + s->length++;
  p->type = PROG_END;
  return p;
}

prog *prog_slice_at(prog_slice s, size_t index) {
  if (index >= s.length) {
    fprintf(stderr, "prog slice index overflow\n");
    exit(EXIT_FAILURE);
  }
  return s.begin + index;
}

EXPORT size_t prog_slice_length(prog_slice s) { return s.length; }

EXPORT datum *prog_slice_to_datum(prog_slice sl) {
  datum *res = datum_make_nil();
  datum **tail = &res;
  for (size_t i = 0; i < prog_slice_length(sl); ++i) {
    prog *p = prog_slice_at(sl, i);
    *tail = datum_make_list_1(prog_to_datum(sl, p));
    tail = &((*tail)->list_tail);
  }
  return res;
}

LOCAL datum *prog_to_datum(prog_slice sl, prog *p) {
  switch (p->type) {
  case PROG_END: {
    return datum_make_list_1(datum_make_symbol(":end"));
  } break;
  case PROG_IF: {
    return datum_make_list_3(datum_make_symbol(":if"),
                             prog_to_offset(sl, p->if_true),
                             prog_to_offset(sl, p->if_false));
  } break;
  case PROG_NOP: {
    return datum_make_list_2(datum_make_symbol(":nop"),
                             prog_to_offset(sl, p->nop_next));
  } break;
  case PROG_PUT_CONST: {
    return datum_make_list_3(datum_make_symbol(":put-const"),
                             (p->put_const_value),
                             prog_to_offset(sl, p->put_const_next));
  } break;
  case PROG_PUT_VAR: {
    return datum_make_list_3(datum_make_symbol(":put-var"), (p->put_var_value),
                             prog_to_offset(sl, p->put_var_next));
  } break;
  case PROG_ARGS: {
    return datum_make_list_2(datum_make_symbol(":args"),
                             prog_to_offset(sl, p->args_next));
  } break;
  case PROG_CALL: {
    return datum_make_list_3(datum_make_symbol(":call"),
                             datum_make_int(p->call_hat),
                             prog_to_offset(sl, p->call_next));
  } break;
  case PROG_HOST: {
    return datum_make_list_3(datum_make_symbol(":host"), p->host_instruction,
                             prog_to_offset(sl, p->host_next));
  } break;
  case PROG_COLLECT: {
    return datum_make_list_2(datum_make_symbol(":collect"),
                             prog_to_offset(sl, p->collect_next));
  } break;
  case PROG_UNCOLLECT: {
    return datum_make_list_2(datum_make_symbol(":uncollect"),
                             prog_to_offset(sl, p->uncollect_next));
  } break;
  case PROG_POP: {
    return datum_make_list_3(datum_make_symbol(":pop"), (p->pop_var),
                             prog_to_offset(sl, p->pop_next));
  } break;
  case PROG_SET_CLOSURES: {
    return datum_make_list_5(datum_make_symbol(":set-closures"),
                             prog_to_offset(sl, p->set_closures_prog),
                             p->set_closures_name,
                             datum_make_int(p->set_closures_hat),
                             prog_to_offset(sl, p->set_closures_next));
  } break;
  case PROG_PUT_PROG: {
    return datum_make_list_4(datum_make_symbol(":put-prog"),
                             prog_to_offset(sl, p->put_prog_value),
                             datum_make_int(p->put_prog_capture),
                             prog_to_offset(sl, p->put_prog_next));
  } break;
  case PROG_RETURN: {
    return datum_make_list_2(datum_make_symbol(":return"),
                             datum_make_int(p->return_hat));
  } break;
  case PROG_YIELD: {
    return datum_make_list_3(datum_make_symbol(":yield"),
                             datum_make_int(p->yield_hat),
                             prog_to_offset(sl, p->yield_next));
  } break;
  case PROG_IMPORT: {
    return datum_make_list_2(datum_make_symbol(":import"),
                             prog_to_offset(sl, p->import_next));
  } break;
  }
  fprintf(stderr, "prog_to_datum incomplete\n");
  exit(EXIT_FAILURE);
}

datum *prog_to_offset(prog_slice sl, prog *p) {
  if (p < prog_slice_at(sl, 0) ||
      p > prog_slice_at(sl, prog_slice_length(sl) - 1)) {
    fprintf(stderr,
            "prog_to_offset received a prog from another slice. slice: (%p, "
            "%p), prog: %p\n",
            prog_slice_at(sl, 0), prog_slice_at(sl, prog_slice_length(sl) - 1),
            p);
    exit(EXIT_FAILURE);
  }
  return datum_make_int(p - prog_slice_at(sl, 0));
}