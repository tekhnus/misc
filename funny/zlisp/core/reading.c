// zlisp interpreter.
#include <reading.h>
#include <string.h>
#if INTERFACE
#include <stdbool.h>
#endif
#include <assert.h>
#include <ctype.h>
#include <stdlib.h>

#if EXPORT_INTERFACE
enum token_type {
  TOKEN_DATUM,
  TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_PAREN,
  TOKEN_RIGHT_SQUARE,
  TOKEN_LEFT_SQUARE,
  TOKEN_RIGHT_CURLY,
  TOKEN_LEFT_CURLY,
  TOKEN_CONTROL_SEQUENCE,
  TOKEN_ERROR,
  TOKEN_EOF,
};
#endif

EXPORT vec datum_read_all(FILE *stre, context *ctxt) {
  datum rr;
  vec res = vec_make(0);
  while (true) {
    rr = datum_read(stre, ctxt, TOKEN_EOF);
    if (ctxt->aborted) {
      return (vec){};
    }
    if (datum_is_nil(&rr)) {
      break;
    }
    vec_extend(&res, &rr);
  }
  return res;
}

EXPORT datum datum_read_one(datum *args, context *ctxt) { // used in lisp
  assert(datum_is_list(args));
  assert(list_length(args) == 1);
  assert(datum_is_integer(list_at(args, 0)));
  FILE *stre = *(FILE **)list_at(args, 0)->integer_value;
  datum rr = datum_read(stre, ctxt, TOKEN_EOF);
  if (ctxt->aborted) {
    return (datum){};
  }
  if (datum_is_nil(&rr)) {
    return datum_make_list_of(datum_make_list_of(datum_make_symbol(":eof")));
  }
  datum *d = malloc(sizeof(datum));
  *d = rr;
  return datum_make_list_of(datum_make_list_of(datum_make_symbol(":ok"), datum_make_int((size_t)d)));
}

LOCAL bool is_whitespace(char c) { return isspace(c) || c == ','; }

LOCAL bool is_allowed_inside_symbol(char c) {
  return isalnum(c) || c == '.' || c == '-' || c == '_' || c == ':' ||
         c == '+' || c == '/' || c == '=';
}

LOCAL bool consume_control_sequence(char c, datum *form) {
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
    *form = datum_make_symbol("flat");
    return true;
  }
  if (c == '@') {
    *form = datum_make_symbol("at");
    return true;
  }
  return false;
}

struct token {
  enum token_type type;
  union {
    datum datum_value;
    datum control_sequence_symbol;
    char *error_message;
  };
};

LOCAL struct token token_read(FILE *strm, context *ctxt) {
  char c = 0; // = 0 is for the compiler.
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
  if (c == ']') {
    return (struct token){.type = TOKEN_RIGHT_SQUARE};
  }
  if (c == '[') {
    return (struct token){.type = TOKEN_LEFT_SQUARE};
  }
  if (c == '}') {
    return (struct token){.type = TOKEN_RIGHT_CURLY};
  }
  if (c == '{') {
    return (struct token){.type = TOKEN_LEFT_CURLY};
  }
  if (isdigit(c) || c == '-') {
    int64_t sign = 1;
    char h = 0; // = 0 is to satisfy the compiler.
    if (c == '-') {
      sign = -1;
      c = getc(strm);
      if (!isdigit(c)) {
        abortf(ctxt, "expected a number after unary minus");
        return (struct token){};
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
    ungetc(c, strm);
    char nm[16][1024];
    nm[0][0] = '\0';
    int c = 0;
    int i;
    char x = 0; // = 0 is to satisfy the compiler
    for (i = 0; !feof(strm) && is_allowed_inside_symbol(x = getc(strm));) {
      if (x == '/') {
        if (strlen(nm[c]) == 0) {
          strcpy(nm[c], "empty-symbol");
        }
        ++c;
        i = 0;
      } else {
        nm[c][i++] = x;
      }
      nm[c][i] = '\0';
    }
    if (!feof(strm)) {
      ungetc(x, strm);
    }
    datum sym;
    if (c == 0) {
      sym = datum_make_symbol(nm[0]);
    } else {
      vec elems = vec_make(0);
      vec_append(&elems, datum_make_symbol("polysym"));
      for (int cc = 0; cc <= c; ++cc) {
        datum comp = datum_make_symbol(nm[cc]);
        vec_append(&elems, comp);
      }
      sym = datum_make_list(elems);
    }
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
        abortf(ctxt, "unknown escape code");
        return (struct token){};
      }
      literal[i] = x;
    }
    literal[i] = '\0';
    return (struct token){.type = TOKEN_DATUM,
                          .datum_value = datum_make_bytestring(literal)};
  }
  datum form;
  if (consume_control_sequence(c, &form)) {
    return (struct token){.type = TOKEN_CONTROL_SEQUENCE,
                          .control_sequence_symbol = form};
  }
  abortf(ctxt, "unexpected symbol: 0x%x\n", c);
  return (struct token){};
}

LOCAL datum datum_read(FILE *strm, context *ctxt, enum token_type terminator) {
  struct token tok = token_read(strm, ctxt);
  if (ctxt->aborted) {
    return (datum){};
  }
  if (tok.type == terminator) {
    return datum_make_list_of();
  }
  if (tok.type == TOKEN_DATUM) {
    datum val = tok.datum_value;
    return datum_make_list_of(val);
  }
  if (tok.type == TOKEN_LEFT_PAREN || tok.type == TOKEN_LEFT_SQUARE ||
      tok.type == TOKEN_LEFT_CURLY) {
    datum elem;
    vec list = vec_make(0);
    enum token_type list_terminator;
    if (tok.type == TOKEN_LEFT_PAREN) {
      list_terminator = TOKEN_RIGHT_PAREN;
    } else if (tok.type == TOKEN_LEFT_SQUARE) {
      list_terminator = TOKEN_RIGHT_SQUARE;
    } else if (tok.type == TOKEN_LEFT_CURLY) {
      list_terminator = TOKEN_RIGHT_CURLY;
    } else {
      assert(false);
    }
    while (true) {
      elem = datum_read(strm, ctxt, list_terminator);
      if (ctxt->aborted) {
        return (datum){};
      }
      if (datum_is_nil(&elem)) {
        break;
      }
      vec_extend(&list, &elem);
    }
    if (tok.type == TOKEN_LEFT_PAREN) {
      return datum_make_list_of(
          datum_make_list_of(datum_make_symbol("call"), datum_make_list(list)));
    }
    if (tok.type == TOKEN_LEFT_SQUARE) {
      return datum_make_list_of(datum_make_list(list));
    }
    if (tok.type == TOKEN_LEFT_CURLY) {
      return datum_make_list_of(datum_make_list(list));
    }
    assert(false);
  }
  if (tok.type == TOKEN_CONTROL_SEQUENCE) {
    datum v = datum_read(strm, ctxt, terminator);
    if (ctxt->aborted) {
      return (datum){};
    }
    if (datum_is_nil(&v)) {
      abortf(ctxt, 
          "expected an expression after a control character");
      return (datum){};
    }
    return datum_make_list_of(tok.control_sequence_symbol, v);
  }
  abortf(ctxt, "unhandled token type");
  return (datum){};
}
