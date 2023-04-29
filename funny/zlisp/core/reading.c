// zlisp interpreter.
#include <extern.h>
#include <string.h>
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

LOCAL bool read_result_is_right_paren(read_result x) {
  return x.type == READ_RESULT_RIGHT_PAREN;
}

LOCAL bool read_result_is_right_bracket(read_result x) {
  return x.type == READ_RESULT_RIGHT_BRACKET;
}

LOCAL read_result read_result_make_ok(datum e) {
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

LOCAL read_result read_result_make_right_bracket(void) {
  read_result result = {.type = READ_RESULT_RIGHT_BRACKET};
  return result;
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
  TOKEN_RIGHT_BRACKET,
  TOKEN_LEFT_BRACKET,
  TOKEN_CONTROL_SEQUENCE,
  TOKEN_ERROR,
  TOKEN_EOF,
};

struct token {
  enum token_type type;
  union {
    datum datum_value;
    datum control_sequence_symbol;
    char *error_message;
  };
};

LOCAL struct token token_read(FILE *strm) {
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
  if (c == '}') {
    return (struct token){.type = TOKEN_RIGHT_BRACKET};
  }
  if (c == '{') {
    return (struct token){.type = TOKEN_LEFT_BRACKET};
  }
  if (isdigit(c) || c == '-') {
    int64_t sign = 1;
    char h = 0; // = 0 is to satisfy the compiler.
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
      sym = datum_make_list_of(datum_make_symbol("polysym"));
      for (int cc = 0; cc <= c; ++cc) {
        datum comp = datum_make_symbol(nm[cc]);
        list_append(&sym, comp);
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
  datum form;
  if (consume_control_sequence(c, &form)) {
    return (struct token){.type = TOKEN_CONTROL_SEQUENCE,
                          .control_sequence_symbol = form};
  }
  fprintf(stderr, "unexpected symbol: 0x%x\n", c);
  return (struct token){.type = TOKEN_ERROR,
                        .error_message = "unexpected symbol"};
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
    datum val = tok.datum_value;
    return read_result_make_ok(val);
  }
  if (tok.type == TOKEN_RIGHT_PAREN) {
    return read_result_make_right_paren();
  }
  if (tok.type == TOKEN_RIGHT_BRACKET) {
    return read_result_make_right_bracket();
  }
  if (tok.type == TOKEN_LEFT_PAREN || tok.type == TOKEN_LEFT_BRACKET) {
    read_result elem;
    datum list = datum_make_nil();
    if (tok.type == TOKEN_LEFT_BRACKET) {
      list_append(&list, datum_make_symbol("progn"));
    }
    for (;;) {
      while (read_result_is_ok(elem = datum_read(strm))) {
        list_append(&list, elem.ok_value);
      }
      if (tok.type == TOKEN_LEFT_PAREN && read_result_is_right_paren(elem)) {
        return read_result_make_ok(list);
      }
      if (tok.type == TOKEN_LEFT_BRACKET && read_result_is_right_bracket(elem)) {
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
    datum res = datum_make_list_of(tok.control_sequence_symbol, v.ok_value);
    return read_result_make_ok(res);
  }
  return read_result_make_panic("unhandled token type");
}

EXPORT read_result datum_read_all(FILE *stre) {
  read_result rr;
  datum res = datum_make_nil();
  for (; read_result_is_ok(rr = datum_read(stre));) {
    list_append(&res, rr.ok_value);
  }
  if (read_result_is_panic(rr)) {
    return read_result_make_panic(rr.panic_message);
  }
  if (read_result_is_right_paren(rr) || read_result_is_right_bracket(rr)) {
    return read_result_make_panic("unmatched right paren");
  }
  return read_result_make_ok(res);
}

EXPORT fdatum datum_read_one(FILE *stre) { // used in lisp
  read_result rr = datum_read(stre);
  if (read_result_is_panic(rr)) {
    return fdatum_make_panic(rr.panic_message);
  }
  if (read_result_is_right_paren(rr) || read_result_is_right_bracket(rr)) {
    return fdatum_make_panic("unmatched right paren");
  }
  if (read_result_is_eof(rr)) {
    return fdatum_make_panic("eof");
  }
  return fdatum_make_ok(datum_make_list_of(rr.ok_value));
}
