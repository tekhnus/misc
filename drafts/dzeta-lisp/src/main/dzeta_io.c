/*
 * Implementation of dzeta_io.h.
 */

#include "dzeta_io.h"

#include <stdio.h>
#include <stdlib.h>


char* read_file(const char* filename) {
    FILE* file = fopen(filename, "r");
    if(!file) {
        fprintf(stderr, "Error while opening file: %s\n", filename);
        exit(1);
    }

    // get file size
    fseek(file, 0, SEEK_END);
    size_t size = ftell(file);
    rewind(file);

    // allocate memory and read
    char* buffer = calloc(size + 1, sizeof(char));
    // freed by user
    size_t actual_size = fread(buffer, sizeof(char), size, file);
    buffer[actual_size] = '\0';

    fclose(file);
    return buffer;
}
