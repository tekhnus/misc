// zlisp interpreter.
#include <extern.h>
#if INTERFACE
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#endif
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

EXPORT datum *datum_make_list_3(datum *head, datum *second, datum *third) {
  return datum_make_list(head, datum_make_list_2(second, third));
}

EXPORT datum *datum_make_list_4(datum *head, datum *second, datum *third,
                         datum *fourth) {
  return datum_make_list(
      head,
      datum_make_list(
                      second, datum_make_list_2(third, fourth)));
}

EXPORT datum *datum_make_list_5(datum *head, datum *second, datum *third,
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

state *state_make(datum *vars) {
  state *res = malloc(sizeof(state));
  res->vars = vars;
  return res;
}

EXPORT state *state_make_fresh() {
  return state_make(datum_make_nil());
}

void state_set_var(state **ns, datum *symbol, datum *value) {
  datum *kv = datum_make_list_2(symbol, value);
  *ns = state_make(datum_make_list(kv, (*ns)->vars));
}

datum *var_at(datum *vars, int offset) {
  if (datum_is_nil(vars)) {
    fprintf(stderr, "var not found\n");
    exit(EXIT_FAILURE);
  }
  /* We have to check for anons anywhere (not only on the top of the list),
     because lambdas inherit the anons during the closure. */
  if (datum_is_the_symbol(vars->list_head->list_head, ":anon")) {
    return var_at(vars->list_tail, offset);
  }
  if (offset == 0) {
    return vars->list_head;
  }
  return var_at(vars->list_tail, offset - 1);
}

fdatum state_get_var(state *ns, datum *symbol, int offset) {
  datum *entry = var_at(ns->vars, offset);
  if (strcmp(entry->list_head->symbol_value, symbol->symbol_value)) {
    fprintf(stderr, "state_get_var: offset didn't match\n");
    exit(EXIT_FAILURE);
  }
  datum *cell = entry->list_tail;
  return fdatum_make_ok(cell->list_head);
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
  state_set_var(ns, datum_make_symbol(":anon"), value);
}

void state_stack_put_all(state **ns, datum *list) {
  if (!datum_is_list(list)) {
    fprintf(stderr, "put_all expected a list\n");
    exit(EXIT_FAILURE);
  }
  for (datum *rest = list; !datum_is_nil(rest); rest = rest->list_tail) {
    state_stack_put(ns, rest->list_head);
  }
}

datum *state_stack_pop(state **s) {
  if (datum_is_nil((*s)->vars)) {
    fprintf(stderr, "popping from an empty stack is an oh no no\n");
    exit(EXIT_FAILURE);
  }
  datum *cell = (*s)->vars->list_head;
  if (!datum_is_the_symbol(cell->list_head, ":anon")) {
    fprintf(stderr, "tried to pop, but it's not an anonymous!\n");
    exit(EXIT_FAILURE);
  }
  *s = state_make((*s)->vars->list_tail);
  return cell->list_tail->list_head;
}

datum *state_stack_collect(state **s, size_t count) {
  datum *form = datum_make_nil();
  for (size_t i = 0; i < count; ++i) {
    datum *arg = state_stack_pop(s);
    form = datum_make_list(arg, form);
  }
  return form;
}

EXPORT prog_slice prog_slice_make(size_t capacity) {
  prog_slice res;
  res.begin = malloc(capacity * sizeof(datum));
  res.length = 0;
  res.capacity = capacity;
  return res;
}

EXPORT size_t prog_slice_append_new(prog_slice *s) {
  if (s->length == s->capacity) {
    fprintf(stderr, "prog slice capacity overflow %zu\n", s->capacity);
    exit(EXIT_FAILURE);
  }
  size_t res = s->length++;
  datum *p = s->begin + res;
  *p = *datum_make_list_1(datum_make_symbol(":end"));
  return res;
}

datum *prog_slice_datum_at(prog_slice s, size_t index) {
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
    *tail = datum_make_list_1(prog_slice_datum_at(sl, i));
    tail = &((*tail)->list_tail);
  }
  return res;
}

EXPORT datum *list_at(datum *list, unsigned index) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    fprintf(stderr, "list_at panic\n");
    exit(EXIT_FAILURE);
  }
  if (index == 0) {
    return list->list_head;
  }
  return list_at(list->list_tail, index - 1);
}
