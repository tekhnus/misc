/*
 * Implementation of dzeta_lex.h.
 */

#include "dzeta_lex.h"

#include <ctype.h>
#include <string.h>
#include <stdio.h>


struct Stream {
    const char* text;
    size_t position;
};


struct Stream* stream_create(const char* text) {
    struct Stream* stream = malloc(sizeof(struct Stream));
    // freed at stream_delete
    stream->text = text;
    stream->position = 0;
    return stream;
}


void stream_delete(struct Stream* stream) {
    free(stream);
}


/*
 * Return char at current position.
 * If stream ended, returns zero char.
 */
char stream_peek(const struct Stream* stream) {
    return stream->text[stream->position];
}


/*
 * If stream begins with text, rewind it to the end of the text
 * and return true. Otherwise, return false. 
 */
int stream_match(struct Stream* stream, const char* text) {
    size_t size = strlen(text);
    int result = strncmp(stream->text + stream->position, text, size);
    if(result == 0) {
        stream->position += size;
    }
    return result == 0;
}


/*
 * Return true iff stream ended.
 */
int stream_eof(const struct Stream* stream) {
    return stream_peek(stream) == '\0';
}


struct Token {
    enum TokenType type;

    int int_value;

    const char* string_value;
    size_t string_value_size;
};


struct Token* token_create(enum TokenType type) {
    struct Token* token = malloc(sizeof(struct Token));
    // freed at token_delete
    token->type = type;
    return token;
}


struct Token* token_create_integer(int int_value) {
    struct Token* token = token_create(INTEGER);
    token->int_value = int_value;
    return token;
}


struct Token* token_create_id(const char* string_value, size_t string_value_size) {
    struct Token* token = token_create(ID);
    token->string_value = string_value;
    token->string_value_size = string_value_size;
    return token;
}


void token_delete(struct Token* token) {
    free(token);
}


void token_print(struct Token* token) {
    if(token->type == L_PARENS) {
        printf("token (\n");
    }
    else if(token->type == R_PARENS) {
        printf("token )\n");
    }
    else if(token->type == IF) {
        printf("token if\n");
    }
    else if(token->type == INTEGER) {
        printf("token int: %d\n", token->int_value);
    }
    else if(token->type == ID) {
        printf("token id: %.*s\n", (int)token->string_value_size, token->string_value);
    }
    else {
        printf("token unknown\n");
    }
}


int is_id_char(char c) {
    return isalnum(c);
}


/*
 * Seek for the next token in stream.
 * Skips whitespace characters.
 * If stream ended, returns NULL.
 */
struct Token* stream_next_token(struct Stream* stream) {
    struct Token* token = NULL;

    if(stream_eof(stream)) {
        return NULL;
    }

    char peek = stream_peek(stream);
    while(isspace(peek)) {
        ++stream->position;
        peek = stream_peek(stream);
    }

    if(stream_eof(stream)) {
        return NULL;
    }

    if(peek == '(') {
        token = token_create(L_PARENS);
        ++stream->position;
    }
    else if(peek == ')') {
        token = token_create(R_PARENS);
        ++stream->position;
    }
    else if(isdigit(peek)) {
        size_t start = stream->position;
        while(!stream_eof(stream) && isdigit(stream_peek(stream))) {
            ++stream->position;
        }
        size_t count = stream->position - start;
        char* copied = calloc(count + 1, sizeof(char));
        // freed several lines below
        strncpy(copied, stream->text + start, count);
        int parsed = atoi(copied);
        free(copied);
        token = token_create_integer(parsed);
    }
    else if(stream_match(stream, "if")) {
        token = token_create(IF);
    }
    else { // it's identifier
        size_t start = stream->position;
        while(!stream_eof(stream) && is_id_char(stream_peek(stream))) {
            ++stream->position;
        }
        size_t count = stream->position - start;
        token = token_create_id(stream->text + start, count);
    }

    return token;
}
