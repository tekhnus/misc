#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <dlfcn.h>

#define LIST 1
#define SYMBOL 2
#define INT 3
#define NATIVE_FORM 4
#define FORM 5
#define BYTESTRING 6
#define EXTERNCFN 7
#define EXTERNCPTR 8

typedef struct expr expr_t;
typedef struct eval_result eval_result_t;
typedef struct context context_t;

char *readline(char *prompt) {
  char *buf = malloc(1024 * sizeof(char));
  printf("%s", prompt);
  char *res = fgets(buf, 1024, stdin);
  if (res == NULL) {
    buf[0] = '\0';
  }
  return buf;
}


typedef struct stream stream_t;
struct stream {
  char *buf;
  size_t end;
  size_t position;
};

bool stream_eof(stream_t *s) {
  return s->position == s->end;
}

char stream_getc(stream_t *s) {
  return s->buf[s->position++];
}

void stream_ungetc(stream_t *s) {
  --s->position;
}

typedef struct namespace namespace_t;

struct expr {
  int8_t type;
  expr_t *car;
  expr_t *cdr;
  char *text;
  int64_t value;
  eval_result_t (*call)(expr_t *, context_t *);
  expr_t *body;
  bool pre_eval;
  bool post_eval;
  namespace_t *lexical_bindings;
  void *extern_c_ptr;
  expr_t *extern_c_signature;
};


typedef struct eval_result eval_result_t;
struct eval_result {
  int8_t type;
  expr_t *expr;
  char *message;
};

#define EXPR 1
#define EOS 2
#define ERR 3
#define CLOSING 4

typedef struct read_result read_result_t;

struct read_result {
  int8_t type;
  expr_t *expr;
  char *message;
};

bool is_whitespace(char symbol) {
  return symbol == ' ' || symbol == '\t' || symbol == '\n' || symbol == '\r' || symbol == ',';
}

bool is_symbol(char symbol) {
  return 'a' <= symbol && symbol <= 'z';
}

read_result_t read_result_make_eof(void) {
  read_result_t result = {EOS, NULL, NULL};
  return result;
}

read_result_t read_result_make_closing(void) {
  read_result_t result = {CLOSING, NULL, NULL};
  return result;
}

bool read_result_is_expr(read_result_t x) {
  return x.type == EXPR;
}

bool read_result_is_eof(read_result_t x) {
  return x.type == EOS;
}

bool read_result_is_err(read_result_t x) {
  return x.type == ERR;
}

bool read_result_is_closing(read_result_t x) {
  return x.type == CLOSING;
}

read_result_t read_result_make_err(char *message) {
  read_result_t result = {ERR, NULL, message};
  return result;
}

read_result_t read_result_make_expr(expr_t *e) {
  read_result_t result = {EXPR, e, NULL};
  return result;
}

expr_t *expr_make_list(expr_t *car) {
  expr_t *e = malloc(sizeof(expr_t));
  e->type = LIST;
  e->car = car;
  e->cdr = NULL;
  return e;
}

expr_t *expr_make_nil() {
  return NULL;
}

expr_t *expr_make_symbol(char *name) {
  expr_t *e = malloc(sizeof(expr_t));
  e->type = SYMBOL;
  size_t length = strlen(name);
  e->text = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->text[i] = name[i];
  }
  return e;
}


expr_t *expr_make_bytestring(char *text) {
  expr_t *e = malloc(sizeof(expr_t));
  e->type = BYTESTRING;
  size_t length = strlen(text);
  e->text = malloc((length + 1) * sizeof(char));
  for (size_t i = 0; i <= length; ++i) {
    e->text[i] = text[i];
  }
  return e;
}

expr_t *expr_make_int(int64_t value) {
  expr_t *e = malloc(sizeof(expr_t));
  e->type = INT;
  e->value = value;
  return e;
}

expr_t *expr_make_native_form(eval_result_t (*call)(expr_t *, context_t *)) {
  expr_t *e = malloc(sizeof(expr_t));
  e->type = NATIVE_FORM;
  e->call = call;
  return e;
}

expr_t *expr_make_form(expr_t *body, namespace_t *lexical_bindings, bool pre_eval, bool post_eval) {
  expr_t *e = malloc(sizeof(expr_t));
  e->type = FORM;
  e->body = body;
  e->lexical_bindings = lexical_bindings;
  e->pre_eval = pre_eval;
  e->post_eval = post_eval;
  return e;
}

expr_t *expr_make_externcfn(void *call_ptr, expr_t *signature) {
  expr_t *e = malloc(sizeof(expr_t));
  e->type = EXTERNCFN;
  e->extern_c_signature = signature;
  e->extern_c_ptr = call_ptr;
  return e;
}

expr_t *expr_make_externcptr(void *ptr) {
  expr_t *e = malloc(sizeof(expr_t));
  e->type = EXTERNCPTR;
  e->extern_c_ptr = ptr;
  return e;
}

bool expr_is_nil(expr_t *e) {
  return e == NULL;
}

bool expr_is_list(expr_t *e) {
  return e == NULL || e->type == LIST;
}

bool expr_is_symbol(expr_t *e) {
  return e != NULL && e->type == SYMBOL;
}

bool expr_is_int(expr_t *e) {
  return e != NULL && e->type == INT;
}

bool expr_is_bytestring(expr_t *e) {
  return e != NULL && e->type == BYTESTRING;
}

bool expr_is_form(expr_t *e) 
{
  return e != NULL && e->type == FORM;
}


bool expr_is_native_form(expr_t *e) 
{
  return e != NULL && e->type == NATIVE_FORM;
}


bool expr_is_externcfn(expr_t *e) 
{
  return e != NULL && e->type == EXTERNCFN;
}

bool expr_is_externcptr(expr_t *e) 
{
  return e != NULL && e->type == EXTERNCPTR;
}

typedef struct expr flat_namespace_t;

flat_namespace_t *flat_namespace_make() {
  return NULL;
}

flat_namespace_t *flat_namespace_set(flat_namespace_t *ns, expr_t *symbol, expr_t *value) {
  expr_t *kv = expr_make_list(symbol);
  kv->cdr = expr_make_list(value);
  expr_t *new_ns = expr_make_list(kv);
  new_ns->cdr = ns;
  return new_ns;
}

struct namespace {
  namespace_t *parent;
  flat_namespace_t *own_bindings;
};

namespace_t *namespace_make_child(namespace_t *parent_namespace) {
  namespace_t *res = malloc(sizeof(namespace_t));
  res->parent = parent_namespace;
  res->own_bindings = flat_namespace_make();
  return res;
}

namespace_t *namespace_make_new() {
  return namespace_make_child(NULL);
}

bool consume_control_sequence(char c, expr_t **form){
  if (c == '\'') {
    *form = expr_make_symbol("quote");
    return true;
  }
  if (c == '`') {
    *form = expr_make_symbol("backquote");
    return true;
  }
  if (c == '~') {
    *form = expr_make_symbol("tilde");
    return true;
  }
  return false;
}

void stream_read(stream_t *s) {
  s->buf = readline("> ");
  s->end = strlen(s->buf);
  s->position = 0;
}

void stream_more(stream_t *s) {
  s->buf = readline(". ");
  s->end = strlen(s->buf);
  s->position = 0;
}

read_result_t zread(stream_t *strm) {
  char c;
  for (; !stream_eof(strm) && is_whitespace(c = stream_getc(strm)););
  if (stream_eof(strm)) {
    return read_result_make_eof();
  }
  if (c == ')') {
    return read_result_make_closing();
  }
  if (c == '(') {
    read_result_t elem;
    expr_t *list = expr_make_nil();
    expr_t **end_marker = &list;
    for (;;) {
      while (read_result_is_expr(elem = zread(strm))) {
	*end_marker = expr_make_list(elem.expr);
	end_marker = &((*end_marker)->cdr);
      }
      if (read_result_is_closing(elem)) {
	return read_result_make_expr( list);
      }
      if (read_result_is_eof(elem)) {
	stream_more(strm);
	if (stream_eof(strm)) {
	  return read_result_make_err( "expected ')', got EOS");
	}
      } else {
	break;
      }
    }
    return elem;
  }
  if (is_symbol(c)) {
    char *nm = malloc(128);
    nm[0] = c;
    int i;
    char x;
    for (i = 1; !stream_eof(strm) && is_symbol(x = stream_getc(strm)); nm[i++] = x);
    if (!stream_eof(strm)) {
      stream_ungetc(strm);
    }
    nm[i] = '\0';
    expr_t *sym = expr_make_symbol(nm);
    return read_result_make_expr( sym);
  }
  if (c == '"') {
    char literal[256];
    char x;
    size_t i;
    for (i = 0; (x = stream_getc(strm)) != '"'; ++i) {
      literal[i] = x;
    }
    literal[i] = '\0';
    return read_result_make_expr(expr_make_bytestring(literal));
  }
  if (('0' <= c && c <= '9') || c == '-') {
    int64_t sign = 1;
    char h;
    if (c == '-') {
      sign = -1;
      c = stream_getc(strm);
      if (!('0' <= c && c <= '9')) {
	return read_result_make_err( "expected a number after unary minus");
      }
    }
    int val = c - '0';
    for (; !stream_eof(strm) && '0' <= (h = stream_getc(strm)) && h <= '9';) {
      val *= 10;
      val += h - '0';
    }
    if (!stream_eof(strm)) {
      stream_ungetc(strm);
    }
    return read_result_make_expr( expr_make_int(sign * val));
  }
  expr_t *form;
  if (consume_control_sequence(c, &form)) {
    read_result_t v = zread(strm);
    if (read_result_is_err(v)) {
      return v;
    }
    if (!read_result_is_expr(v)) {
      return read_result_make_err( "expected an expression after a control character");
    }
    expr_t *res = expr_make_list(form);
    res->cdr = expr_make_list(v.expr);
    return read_result_make_expr(res);
  }
  char err[1024];
  sprintf(err, "unexpected symbol: 0x%x", c);
  return read_result_make_err( err);
}

eval_result_t eval_result_make_err(char *message) {
  eval_result_t result = {ERR, NULL, message};
  return result;
}
eval_result_t eval_result_make_expr(expr_t *e) {
  eval_result_t result = {EXPR, e, NULL};
  return result;
}

bool eval_result_is_expr(eval_result_t result) {
  return result.type == EXPR;
}

bool eval_result_is_err(eval_result_t result) {
  return result.type == ERR;
}

typedef struct eval_result eval_result_t;

eval_result_t flat_namespace_get(flat_namespace_t *ns, expr_t *symbol) {
  for (expr_t *cur = ns; !expr_is_nil(cur); cur = cur->cdr) {
    expr_t *kv = cur->car;
    if (!strcmp(kv->car->text, symbol->text)) {
      return eval_result_make_expr(kv->cdr->car);
    }
  }
  char msg[1024];
  sprintf(msg, "unbound symbol: %s", symbol->text);
  return eval_result_make_err(msg);
}

struct context {
  namespace_t *bindings;
};
typedef struct context context_t;

context_t context_make(namespace_t *namespace) {
  context_t ctxt = {namespace};
  return ctxt;
}
  
eval_result_t context_get(context_t *ctxt, expr_t *symbol) {
  eval_result_t v;
  namespace_t *bindings;
  for (bindings = ctxt->bindings; bindings != NULL; bindings = bindings->parent) {
    v = flat_namespace_get(bindings->own_bindings, symbol);
    if(!eval_result_is_err(v)) {
      return v;
    }
  }
  return v;
}

void context_set(context_t *ctxt, expr_t *symbol, expr_t *value) {
  flat_namespace_t **locals = &ctxt->bindings->own_bindings;
  *locals = flat_namespace_set(*locals, symbol, value);
}

eval_result_t eval(expr_t *e, context_t *ctxt);

expr_t *args_symbol(void) {
  return expr_make_symbol("args");
}

eval_result_t apply_form(expr_t *f, expr_t *args, context_t *ctxt) {
  expr_t *passed_args = NULL;
  if (!f->pre_eval) {
    passed_args = args;
  }
  else {
     expr_t **tail = &passed_args;
     for (expr_t *arg = args; !expr_is_nil(arg); arg = arg->cdr) {
       eval_result_t evaled_arg = eval(arg->car, ctxt);
       if (eval_result_is_err(evaled_arg)) {
	 return evaled_arg;
       }
       *tail = expr_make_list(evaled_arg.expr);
       tail = &((*tail)->cdr);
     }
  }
  context_t form_ctxt = context_make
    (namespace_make_child(f->lexical_bindings));
  context_set(&form_ctxt, args_symbol(), passed_args);
  eval_result_t expansion = eval(f->body, &form_ctxt);
  if (eval_result_is_err(expansion)) {
    return expansion;
  }
  if (!f->post_eval) {
    return expansion;
  }
  expansion = eval(expansion.expr, ctxt);
  return expansion;
}

eval_result_t apply_externcfn(expr_t *f, expr_t *args, context_t *ctxt) {
  expr_t *passed_args = NULL;
  expr_t **tail = &passed_args;
  for (expr_t *arg = args; !expr_is_nil(arg); arg = arg->cdr) {
    eval_result_t evaled_arg = eval(arg->car, ctxt);
    if (eval_result_is_err(evaled_arg)) {
      return evaled_arg;
    }
    *tail = expr_make_list(evaled_arg.expr);
    tail = &((*tail)->cdr);
  }
  FILE* (*call)(char *, char *) = f->extern_c_ptr;
  void* res = call(passed_args->car->text, passed_args->cdr->car->text);
  return eval_result_make_expr(expr_make_externcptr(res));
}

char *fmt(expr_t *e);
eval_result_t apply(expr_t *f, expr_t *args, context_t *ctxt) {
  if (!expr_is_list(args)) {
    return eval_result_make_err("args should be list");
  }
  if (expr_is_native_form(f)) {
    return (f->call)(args, ctxt);
  }
  if (expr_is_form(f)) {
    return apply_form(f, args, ctxt);
  }
  if (expr_is_externcfn(f)) {
    return apply_externcfn(f, args, ctxt);
  }
  return eval_result_make_err("car should be callable");
}

eval_result_t add(expr_t *args, context_t *ctxt) {
  int64_t res = 0;
  for (expr_t *arg = args; !expr_is_nil(arg); arg = arg->cdr) {
    eval_result_t x = eval(arg->car, ctxt);
    if (eval_result_is_err(x)) {
      return x;
    }
    if (!expr_is_int(x.expr)) {
      return eval_result_make_err("expected integers");
    }
    res += x.expr->value;
  }
  return eval_result_make_expr(expr_make_int(res));
}

eval_result_t eval_car(expr_t *e, context_t *ctxt) {
  if (expr_is_nil(e) || !expr_is_nil(e->cdr)) {
    return eval_result_make_err("eval expects exactly one argument");
  }
  eval_result_t v = eval(e->car, ctxt);
  if (eval_result_is_err(v)) {
    return v;
  }
  v = eval(v.expr, ctxt);
  return v;
}

eval_result_t cons(expr_t *args, context_t *ctxt) {
  if (expr_is_nil(args) || expr_is_nil(args->cdr) || !expr_is_nil(args->cdr->cdr)) {
    return eval_result_make_err("cons expects exactly two arguments");
  }
  if (!expr_is_list(args->cdr->car)) {
    return eval_result_make_err("cons requires a list as a second argument");
  }
  eval_result_t er = eval(args->car, ctxt);
  if (eval_result_is_err(er)) {
    return er;
  }
  expr_t *result = expr_make_list(er.expr);
  er = eval(args->cdr->car, ctxt);
  if (eval_result_is_err(er)) {
    return er;
  }
  result->cdr = er.expr;
  return eval_result_make_expr(result);
}

eval_result_t car(expr_t *args, context_t *ctxt) {
  if (expr_is_nil(args) || !expr_is_nil(args->cdr)) {
    return eval_result_make_err("car expects exactly one argument");
  }
  eval_result_t er = eval(args->car, ctxt);
  if (eval_result_is_err(er)) {
    return er;
  }
  if (!expr_is_list(er.expr) || expr_is_nil(er.expr)) {
    return eval_result_make_err("car expects a nonempty list");
  }
  return eval_result_make_expr(er.expr->car);
}


eval_result_t cdr(expr_t *args, context_t *ctxt) {
  if (expr_is_nil(args) || !expr_is_nil(args->cdr)) {
    return eval_result_make_err("cdr expects exactly one argument");
  }
  eval_result_t er = eval(args->car, ctxt);
  if (eval_result_is_err(er)) {
    return er;
  }
  if (!expr_is_list(er.expr) || expr_is_nil(er.expr)) {
    return eval_result_make_err("cdr expects a nonempty list");
  }
  return eval_result_make_expr(er.expr->cdr);
}

eval_result_t macro(expr_t *args, context_t *ctxt) {
  if (expr_is_nil(args) || !expr_is_nil(args->cdr)) {
    return eval_result_make_err("macro expects a single argument");
  }
  return eval_result_make_expr(expr_make_form(args->car, ctxt->bindings, false, true));
}

eval_result_t fn(expr_t *args, context_t *ctxt) {
  if (expr_is_nil(args) || !expr_is_nil(args->cdr)) {
    return eval_result_make_err("fn expects a single argument");
  }
  return eval_result_make_expr(expr_make_form(args->car, ctxt->bindings, true, false));
}

eval_result_t form(expr_t *args, context_t *ctxt) {
  if (expr_is_nil(args) || !expr_is_nil(args->cdr)) {
    return eval_result_make_err("form expects a single argument");
  }
  return eval_result_make_expr(expr_make_form(args->car, ctxt->bindings, false, false));
}

eval_result_t def(expr_t *args, context_t *ctxt) {
  if (expr_is_nil(args) || expr_is_nil(args->cdr) || !expr_is_nil(args->cdr->cdr)) {
    return eval_result_make_err("def expects exactly two arguments");
  }
  if (!expr_is_symbol(args->car)) {
    return eval_result_make_err("def requires a symbol as a first argument");
  }
  eval_result_t er = eval(args->cdr->car, ctxt);
  if (!eval_result_is_err(er)) {
    context_set(ctxt, args->car, er.expr);
  }
  return er;
}

eval_result_t backquote(expr_t *args, context_t *ctxt) {
  if (!expr_is_list(args->car) || expr_is_nil(args->car)) {
    return eval_result_make_expr(args->car);
  }
  if (expr_is_symbol(args->car->car) && !strcmp(args->car->car->text, "tilde")) 
    {
      return eval(args->car->cdr->car, ctxt);
    }
  expr_t *processed = expr_make_nil();
  expr_t **tail = &processed;
  for (expr_t *elem = args->car; !expr_is_nil(elem); elem = elem->cdr) 
    {
      eval_result_t inner = backquote(expr_make_list(elem->car), ctxt);
      if (eval_result_is_err(inner)) 
	{
	  return inner;
	}
      *tail = expr_make_list(inner.expr);
      tail = &((*tail)->cdr);
    }
  
  return eval_result_make_expr(processed);
}


eval_result_t if_(expr_t *args, context_t *ctxt) {
  if (expr_is_nil(args) || expr_is_nil(args->cdr) || expr_is_nil(args->cdr->cdr) || !expr_is_nil(args->cdr->cdr->cdr)) {
    return eval_result_make_err("if expects exactly three arguments");
  }
  eval_result_t condition = eval(args->car, ctxt);
  if (eval_result_is_err(condition)) {
    return condition;
  }
  if (!expr_is_nil(condition.expr)) {
    return eval(args->cdr->car, ctxt);
  }
  return eval(args->cdr->cdr->car, ctxt);
}

eval_result_t externcfn(expr_t *args, context_t *ctxt) {
  if (expr_is_nil(args) || expr_is_nil(args->cdr) || expr_is_nil(args->cdr->cdr) || !expr_is_nil(args->cdr->cdr->cdr)) {
    return eval_result_make_err("externcfn expects exactly three arguments");
  }
  if (!expr_is_bytestring(args->car) || !expr_is_bytestring(args->cdr->car) || !expr_is_list(args->cdr->cdr->car)) {
    return eval_result_make_err("wrong externcfn signature");
  }
  void *handle = dlopen(args->car->text, RTLD_LAZY);
  char *err = dlerror();
  if (!handle) {
    return eval_result_make_err(err);
  }
  void *call_ptr = dlsym(handle, args->cdr->car->text);
  err = dlerror();
  if (err != NULL) {
    return eval_result_make_err(err);
  }
  return eval_result_make_expr(expr_make_externcfn(call_ptr, args->cdr->cdr->car));
}

eval_result_t eval(expr_t *e, context_t *ctxt) {
  if (expr_is_nil(e)) {
      return eval_result_make_err("cannot eval an empty list");
  }
  if (expr_is_list(e)) {
    eval_result_t f = eval(e->car, ctxt);
    if (!eval_result_is_expr(f)) {
      return f;
    }
    eval_result_t app = apply(f.expr, e->cdr, ctxt);
    return app;
  }
  if (expr_is_symbol(e)) {
    return context_get(ctxt, e);
  }
  if (expr_is_int(e) || expr_is_bytestring(e)) {
    return eval_result_make_expr(e);
  }
  return eval_result_make_err("non-evalable expression");
}
  
char *fmt(expr_t *e) {
  char *buf = malloc(1024 * sizeof(char));
  char *end = buf;
  if (expr_is_int(e)) {
    sprintf(buf, "%ld", e->value);
  }
  else if(expr_is_list(e)) {
    end += sprintf(end, "(");
    for (expr_t *item = e; !expr_is_nil(item); item = item->cdr) {
      end += sprintf(end, "%s ", fmt(item->car));
    }
    end += sprintf(end, ")");
  }
  else if(expr_is_symbol(e)) {
    end += sprintf(end, "%s", e->text);
  }
  else if(expr_is_bytestring(e)) {
    end += sprintf(end, "\"%s\"", e->text);
  }
  else if(expr_is_form(e)) 
    {
      end += sprintf(end, "<form>");
    }
  else if(expr_is_native_form(e)) {
    end += sprintf(end, "<native form>");
  }
  else if(expr_is_externcfn(e)) {
    end += sprintf(end, "<externcfn>");
  }
  else if(expr_is_externcptr(e)) {
    end += sprintf(end, "<externcptr %x>", e->extern_c_ptr);
  }
  else {
    sprintf(buf, "[fmt not implemented]");
  }
  return buf;
}

void context_set_native_form(context_t *ctxt, char *name, eval_result_t (*form)(expr_t *, context_t *)) {
  context_set(ctxt, expr_make_symbol(name), expr_make_native_form(form));
}

int main(void) {
  context_t context = context_make(namespace_make_new());

  context_set_native_form(&context, "add", add);
  context_set_native_form(&context, "eval", eval_car);
  context_set_native_form(&context, "cons", cons);
  context_set_native_form(&context, "car", car);
  context_set_native_form(&context, "cdr", cdr);
  context_set_native_form(&context, "builtinmacro", macro);
  context_set_native_form(&context, "builtinfn", fn);
  context_set_native_form(&context, "builtinform", form);
  context_set_native_form(&context, "def", def);
  context_set_native_form(&context, "if", if_);
  context_set_native_form(&context, "backquote", backquote);
  context_set_native_form(&context, "externcfn", externcfn);
		    
  for (; !feof(stdin);) {
    read_result_t rr;
    stream_t strm;
    stream_read(&strm);
    for (; read_result_is_expr(rr = zread(&strm));) {
      eval_result_t val = eval(rr.expr, &context);
      if (eval_result_is_err(val)) {
	printf("%s\n", val.message);
      }
      else {
	char *s = fmt(val.expr);
	printf("%s\n", s);
      }
    }
    if(read_result_is_closing(rr)) {
      printf("unmatched closing bracket\n");
    }
    else if (read_result_is_err(rr)) {
      printf("%s\n", rr.message);
    }
  }
  return 0;
}
