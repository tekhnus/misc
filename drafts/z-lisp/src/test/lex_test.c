#include "dzeta_lex.h"
#include "dzeta_io.h"

void test(const char* source) {
    struct Stream* stream = stream_create(source);
    struct Token* token = NULL;
    while((token = stream_next_token(stream))) {
        token_print(token);
        token_delete(token);
    }
    stream_delete(stream);
}

void stress_test(const char* filename) {
    char* source = read_file(filename);
    struct Stream* stream = stream_create(source);
    struct Token* token = NULL;
    while((token = stream_next_token(stream))) {
        token_delete(token);
    }
    stream_delete(stream);
    free(source);
}

int main() {
    test("(hello (if good 42 43))");
    stress_test("test.dz");
    return 0;
}
