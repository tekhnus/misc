#ifndef DZETA_LEX_H
#define DZETA_LEX_H

/*
 * Lexer for dzeta-lisp.
 */

#include <stdlib.h>

/*
 * "Text Stream" class.
 * This structure actually is a string and an iterator.
 */
struct Stream;


struct Stream* stream_create(const char* text);


void stream_delete(struct Stream* stream);


/*
 * Return char at current position.
 * If stream ended, returns zero char.
 */
char stream_peek(const struct Stream* stream);


/*
 * If stream begins with text, rewind it to the end of the text
 * and return true. Otherwise, return false.
 */
int stream_match(struct Stream* stream, const char* text);


/*
 * Return true iff stream ended.
 */
int stream_eof(const struct Stream* stream);


enum TokenType {
    L_PARENS, R_PARENS,

    IF,

    INTEGER,
    ID
};


/*
 * A token is atomic unit of program source. It can represent
 * a bracket, an identifier, a number etc.
 */
struct Token;


struct Token* token_create(enum TokenType type);


struct Token* token_create_integer(int value);


struct Token* token_create_id(const char* string_value, size_t string_value_size);


void token_delete(struct Token* token);


/*
 * Print information about token to stdout. For debug purposes.
 */
void token_print(struct Token* token);


/*
 * Return true iff c is valid identifier char,
 * such as letter, digit, question or exclamation mark, etc.
 */
int is_id_char(char c);


/*
 * Seek for the next token in stream.
 * Skips whitespace characters.
 * If stream ended, returns NULL.
 */
struct Token* stream_next_token(struct Stream* stream);


#endif
