// zlisp interpreter.
#include <extern.h>
#if INTERFACE
#include <stdbool.h>
#endif
#include <ctype.h>
#include <stdlib.h>

EXPORT bool read_result_is_ok(read_result x) {
  return x.type == READ_RESULT_OK;
}

EXPORT bool read_result_is_panic(read_result x) {
  return x.type == READ_RESULT_PANIC;
}

LOCAL bool read_result_is_eof(read_result x) {
  return x.type == READ_RESULT_EOF;
}

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
  return isalnum(c) || c == '.' || c == '-' || c == '_' || c == ':' ||
         c == '+' || c == '/';
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
  if (c == '@') {
    *form = datum_make_symbol("at");
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
    char **nm = malloc(128);
    nm[0] = malloc(128);
    nm[0][0] = c;
    int c = 0;
    int i;
    char x;
    for (i = 1; !feof(strm) && is_allowed_inside_symbol(x = getc(strm));) {
      if (x == '/') {
        nm[c++][i] = '\0';
        nm[c] = malloc(128);
        i = 0;
      } else {
        nm[c][i++] = x;
      }
    }
    if (!feof(strm)) {
      ungetc(x, strm);
    }
    nm[c][i] = '\0';
    datum *sym;
    if (c == 0) {
      sym = datum_make_symbol(nm[0]);
    } else {
      sym = datum_make_list_of(1, datum_make_symbol("polysym"));
      for (int cc = 0; cc <= c; ++cc) {
        sym = list_append(sym, datum_make_symbol(nm[cc]));
      }
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
    for (;;) {
      while (read_result_is_ok(elem = datum_read(strm))) {
        list = list_append(list, elem.ok_value);
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
    datum *res = datum_make_list_of(2, tok.control_sequence_symbol, v.ok_value);
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
    return fdatum_make_panic("eof");
  }
  return fdatum_make_ok(datum_make_list_of(1, rr.ok_value));
}
