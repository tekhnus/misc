// zlisp interpreter.
#include <zlisp-impl/main.h>

#include <ctype.h>
#include <dlfcn.h>
#include <libgen.h>

#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

enum fdatumype {
  FDATUM_OK,
  FDATUM_PANIC,
};

enum fstate_type {
  FSTATE_OK,
  FSTATE_PANIC,
};

bool datum_is_the_symbol(datum *d, char *val) {
  return datum_is_symbol(d) && !strcmp(d->symbol_value, val);
}

routine routine_make(prog *s, state *ctxt) {
  routine res = {.prog_ = s, .state_ = ctxt};
  return res;
}

routine routine_make_null() {
  routine res = {};
  return res;
}

bool routine_is_null(routine r) { return r.prog_ == NULL && r.state_ == NULL; }

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

bool datum_is_routine(datum *e) { return e->type == DATUM_ROUTINE; }

bool datum_is_pointer(datum *e) { return e->type == DATUM_POINTER; }

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

datum *datum_make_routine(prog *s, state *lexical_bindings) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_ROUTINE;
  e->routine_value.prog_ = s;
  e->routine_value.state_ = lexical_bindings;
  return e;
}

datum *datum_make_pointer(void *data, datum *signature) {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_POINTER;
  e->pointer_descriptor = signature;
  e->pointer_value = data;
  return e;
}

datum *datum_make_pointer_to_pointer(void **ptr) {
  return datum_make_pointer(ptr, datum_make_symbol("pointer"));
}

datum *datum_make_void() {
  datum *e = malloc(sizeof(datum));
  e->type = DATUM_VOID;
  return e;
}

bool read_result_is_ok(read_result x) { return x.type == READ_RESULT_OK; }

bool read_result_is_panic(read_result x) {
  return x.type == READ_RESULT_PANIC;
}

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

read_result datum_read(FILE *strm) {
  char c;
  for (; !feof(strm) && is_whitespace(c = getc(strm));) {
  }
  if (feof(strm)) {
    return read_result_make_eof();
  }
  if (c == ')') {
    return read_result_make_right_paren();
  }
  if (c == '(') {
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
         nm[i++] = x) {
    }
    if (!feof(strm)) {
      ungetc(x, strm);
    }
    nm[i] = '\0';
    datum *sym = datum_make_symbol(nm);
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
  datum *form;
  if (consume_control_sequence(c, &form)) {
    read_result v = datum_read(strm);
    if (read_result_is_panic(v)) {
      return v;
    }
    if (!read_result_is_ok(v)) {
      return read_result_make_panic(
          "expected an expression after a control character");
    }
    datum *res = datum_make_list_1(form);
    res->list_tail = datum_make_list_1(v.ok_value);
    return read_result_make_ok(res);
  }
  char *err = malloc(1024);
  sprintf(err, "unexpected symbol: 0x%x", c);
  return read_result_make_panic(err);
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
  } else if (datum_is_routine(e)) {
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

bool fstate_is_ok(fstate result) { return result.type == FSTATE_OK; }

bool fstate_is_panic(fstate result) { return result.type == FSTATE_PANIC; }

fstate fstate_make_ok(state *v) {
  fstate result = {.type = FSTATE_OK, .ok_value = v};
  return result;
}

fstate fstate_make_panic(char *message) {
  fstate result = {.type = FSTATE_PANIC, .panic_message = message};
  return result;
}

state *state_make(datum *vars, datum *stack, routine parent,
                    routine hat_parent) {
  state *res = malloc(sizeof(state));
  res->vars = vars;
  res->stack = stack;
  res->parent = parent;
  res->hat_parent = hat_parent;
  return res;
}

state *state_make_fresh() {
  routine zero = routine_make_null();
  return state_make(datum_make_nil(), datum_make_nil(), zero, zero);
}

state *state_set_var(state *ns, datum *symbol, datum *value) {
  datum *kv = datum_make_list_3(symbol, datum_make_symbol(":value"), value);
  return state_make(datum_make_list(kv, ns->vars), ns->stack, ns->parent,
                    ns->hat_parent);
}

state *state_set_fn(state *ns, datum *symbol, datum *value) {
  datum *kv = datum_make_list_3(symbol, datum_make_symbol(":fn"), value);
  return state_make(datum_make_list(kv, ns->vars), ns->stack, ns->parent,
                    ns->hat_parent);
}

datum *namespace_cell_get_value(datum *cell, state *ns) {
  datum *raw_value = cell->list_tail->list_head;
  if (!strcmp(cell->list_head->symbol_value, ":value")) {
    return raw_value;
  } else if (!strcmp(cell->list_head->symbol_value, ":fn")) {
    if (!datum_is_routine(raw_value)) {
      fprintf(stderr, "namespace implementation error");
      exit(EXIT_FAILURE);
    }
    state *routine_ns = state_make(ns->vars, datum_make_nil(),
                                     routine_make_null(), routine_make_null());
    return datum_make_routine(raw_value->routine_value.prog_, routine_ns);
  } else {
    fprintf(stderr, "namespace implementation error");
    exit(EXIT_FAILURE);
  }
}

fdatum state_get_var(state *ns, datum *symbol) {
  for (datum *cur = ns->vars; !datum_is_nil(cur); cur = cur->list_tail) {
    datum *entry = cur->list_head;
    if (!strcmp(entry->list_head->symbol_value, symbol->symbol_value)) {
      datum *cell = entry->list_tail;
      return fdatum_make_ok(namespace_cell_get_value(cell, ns));
    }
  }
  char *msg = malloc(1024);
  sprintf(msg, "unbound symbol: %s", symbol->symbol_value);
  return fdatum_make_panic(msg);
}

fdatum list_map(fdatum (*fn)(datum *, state *), datum *items,
                  state *ctxt) {
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

bool ffi_type_init(ffi_type **type, datum *definition) {
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

char *pointer_ffi_init_cif(datum *f, ffi_cif *cif) {
  datum *sig = f->pointer_descriptor;
  if (!datum_is_list(sig) || datum_is_nil(sig) ||
      datum_is_nil(sig->list_tail) ||
      !datum_is_nil(sig->list_tail->list_tail)) {
    return "the signature should be a two-item list";
  }
  ffi_type **arg_types = malloc(sizeof(ffi_type *) * 32);
  int arg_count = 0;
  datum *arg_def;
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

char *pointer_ffi_serialize_args(datum *f, datum *args, void **cargs) {
  int arg_cnt = 0;
  datum *arg = args;
  for (datum *argt = f->pointer_descriptor->list_head; !datum_is_nil(argt);
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
      datum *sig;
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

fdatum pointer_ffi_call(datum *f, ffi_cif *cif, void **cargs) {
  void (*fn_ptr)(void) = __extension__(void (*)(void))(f->pointer_value);
  char *rettype = f->pointer_descriptor->list_tail->list_head->symbol_value;

  if (!strcmp(rettype, "pointer")) {
    void *res = malloc(sizeof(void *));
    ffi_call(cif, fn_ptr, res, cargs);
    return fdatum_make_ok(datum_make_pointer_to_pointer(res));
  }
  if (!strcmp(rettype, "sizet")) {
    void *res = malloc(sizeof(size_t));
    ffi_call(cif, fn_ptr, res, cargs);
    return fdatum_make_ok(datum_make_int(*(int64_t *)res));
  }
  if (!strcmp(rettype, "int")) {
    void *res = malloc(sizeof(int));
    ffi_call(cif, fn_ptr, res, cargs);
    return fdatum_make_ok(datum_make_int(*(int64_t *)res));
  }
  if (!strcmp(rettype, "val")) {
    fdatum res;
    ffi_call(cif, fn_ptr, &res, cargs);
    if (fdatum_is_panic(res)) {
      return fdatum_make_panic(res.panic_message);
    }
    return fdatum_make_ok(res.ok_value);
  }
  return fdatum_make_panic("unknown return type for extern func");
}

fdatum pointer_call(datum *f, datum *args) {
  ffi_cif cif;
  char *err = NULL;
  err = pointer_ffi_init_cif(f, &cif);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  void *cargs[32];
  err = pointer_ffi_serialize_args(f, args, cargs);
  if (err != NULL) {
    return fdatum_make_panic(err);
  }
  return pointer_ffi_call(f, &cif, cargs);
}

fdatum builtin_concat_bytestrings(datum *x, datum *y) {
  if (!datum_is_bytestring(x) || !datum_is_bytestring(y)) {
    return fdatum_make_panic("expected integers");
  }
  char *buf =
      malloc(strlen(x->bytestring_value) + strlen(y->bytestring_value) + 1);
  buf[0] = '\0';
  strcat(buf, x->bytestring_value);
  strcat(buf, y->bytestring_value);
  return fdatum_make_ok(datum_make_bytestring(buf));
}

fdatum builtin_add(datum *x, datum *y) {
  if (!datum_is_integer(x) || !datum_is_integer(y)) {
    return fdatum_make_panic("expected integers");
  }
  return fdatum_make_ok(datum_make_int(x->integer_value + y->integer_value));
}

fdatum builtin_cons(datum *head, datum *tail) {
  if (!datum_is_list(tail)) {
    return fdatum_make_panic("cons requires a list as a second argument");
  }
  return fdatum_make_ok(datum_make_list(head, tail));
}

fdatum builtin_head(datum *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("car expects a nonempty list");
  }
  return fdatum_make_ok(list->list_head);
}

fdatum builtin_tail(datum *list) {
  if (!datum_is_list(list) || datum_is_nil(list)) {
    return fdatum_make_panic("cdr expects a nonempty list");
  }
  return fdatum_make_ok(list->list_tail);
}

datum *state_list_vars(state *ns) {
  datum *result = datum_make_nil();
  datum **nil = &result;
  for (datum *cur = ns->vars; !datum_is_nil(cur); cur = cur->list_tail) {
    datum *entry = cur->list_head;
    datum *key = entry->list_head;
    datum *cell = entry->list_tail;
    datum *val = namespace_cell_get_value(cell, ns);
    datum *keyval = datum_make_list_2(key, val);
    *nil = datum_make_list_1(keyval);
    nil = &((*nil)->list_tail);
  }
  return result;
}

fdatum builtin_shared_library(datum *library_name) {
  if (!datum_is_bytestring(library_name)) {
    return fdatum_make_panic("load-shared-library expects a bytestring");
  }
  void **handle = malloc(sizeof(void *));
  *handle = dlopen(library_name->bytestring_value, RTLD_LAZY);
  char *err = dlerror();
  if (!*handle) {
    return fdatum_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                            datum_make_bytestring(err)));
  }
  return fdatum_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"), datum_make_pointer_to_pointer(handle)));
}

fdatum builtin_extern_pointer(datum *shared_library, datum *name,
                                datum *descriptor) {
  if (!datum_is_pointer(shared_library) ||
      !datum_is_symbol(shared_library->pointer_descriptor) ||
      strcmp(shared_library->pointer_descriptor->symbol_value, "pointer")) {
    return fdatum_make_panic("wrong externcdata usage");
  }
  void *handle = *(void **)shared_library->pointer_value;
  if (!datum_is_bytestring(name)) {
    return fdatum_make_panic("externcdata expected a string");
  }
  void *call_ptr = dlsym(handle, name->bytestring_value);
  char *err = dlerror();
  if (err != NULL) {
    return fdatum_make_ok(datum_make_list_2(datum_make_symbol(":err"),
                                            datum_make_bytestring(err)));
  }
  return fdatum_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"), datum_make_pointer(call_ptr, descriptor)));
}

fdatum builtin_repr(datum *v) {
  return fdatum_make_ok(datum_make_bytestring(datum_repr(v)));
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

fdatum builtin_eq(datum *x, datum *y) {
  datum *t = datum_make_list_1(datum_make_nil());
  datum *f = datum_make_nil();
  if (datum_eq(x, y)) {
    return fdatum_make_ok(t);
  }
  return fdatum_make_ok(f);
}

fdatum builtin_annotate(datum *arg_value) {
  char *type;
  if (datum_is_list(arg_value)) {
    type = ":list";
  } else if (datum_is_symbol(arg_value)) {
    type = ":symbol";
  } else if (datum_is_bytestring(arg_value)) {
    type = ":bytestring";
  } else if (datum_is_integer(arg_value)) {
    type = ":integer";
  } else if (datum_is_routine(arg_value)) {
    type = ":operator";
  } else if (datum_is_pointer(arg_value)) {
    type = ":pointer";
  } else {
    return fdatum_make_panic("incomplete implementation of type");
  }
  return fdatum_make_ok(datum_make_list_2(datum_make_symbol(type), arg_value));
}

fdatum builtin_is_constant(datum *arg_value) {
  if (datum_is_constant(arg_value)) {
    return fdatum_make_ok(datum_make_list_1(datum_make_nil()));
  }
  return fdatum_make_ok(datum_make_nil());
}

fdatum builtin_panic(datum *arg_value) {
  if (!datum_is_bytestring(arg_value)) {
    return fdatum_make_panic("panic expects a bytestring");
  }
  return fdatum_make_panic(arg_value->bytestring_value);
}

void namespace_def_extern_fn(state **ctxt, char *name, fdatum (*fn)(),
                             int cnt) {
  datum *sig = datum_make_nil();
  for (int i = 0; i < cnt; ++i) {
    sig = datum_make_list(datum_make_symbol("datum"), sig);
  }
  datum *wrapped_fn =
      datum_make_pointer(__extension__(void *) fn,
                         datum_make_list_2(sig, datum_make_symbol("val")));
  *ctxt = state_set_var(*ctxt, datum_make_symbol(name), wrapped_fn);
}

state *state_make_builtins() {
  state *ns = state_make_fresh();

  namespace_def_extern_fn(&ns, "panic--", builtin_panic, 1);
  namespace_def_extern_fn(&ns, "shared-library--", builtin_shared_library, 1);
  namespace_def_extern_fn(&ns, "extern-pointer--", builtin_extern_pointer, 3);
  namespace_def_extern_fn(&ns, "cons--", builtin_cons, 2);
  namespace_def_extern_fn(&ns, "head--", builtin_head, 1);
  namespace_def_extern_fn(&ns, "tail--", builtin_tail, 1);
  namespace_def_extern_fn(&ns, "eq--", builtin_eq, 2);
  namespace_def_extern_fn(&ns, "annotate--", builtin_annotate, 1);
  namespace_def_extern_fn(&ns, "is-constant--", builtin_is_constant, 1);
  namespace_def_extern_fn(&ns, "repr--", builtin_repr, 1);
  namespace_def_extern_fn(&ns, "concat-bytestrings--", builtin_concat_bytestrings,
                          2);
  namespace_def_extern_fn(&ns, "+--", builtin_add, 2);
  return ns;
}

bool datum_is_constant(datum *d) {
  return (datum_is_integer(d) || datum_is_bytestring(d) ||
          (datum_is_symbol(d) && d->symbol_value[0] == ':'));
}


void state_stack_put(state **ns, datum *value) {
  *ns = state_make((*ns)->vars, datum_make_list(value, (*ns)->stack), (*ns)->parent,
                    (*ns)->hat_parent);
}

datum *state_stack_pop(state **s) {
  datum *res = (*s)->stack->list_head;
  *s = state_make((*s)->vars, (*s)->stack->list_tail, (*s)->parent, (*s)->hat_parent);
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

routine state_get_parent(state *ns, bool hat) {
  if (hat) {
    return ns->hat_parent;
  }
  return ns->parent;
}

state *state_change_parent(state *ns, routine new_parent, bool hat) {
  if (hat) {
    return state_make(ns->vars, ns->stack, ns->parent, new_parent);
  }
  return state_make(ns->vars, ns->stack, new_parent, ns->hat_parent);
}

