#ifndef DZETA_IO_H
#define DZETA_IO_H

/*
 * Useful I/O procedures for dzeta-lisp. 
 */


/*
 * Read the entire file, return its content.
 * In case of I/O error, exit with return status 1.
 * Returned string is allocated on the heap and must be
 * freed by the user manually.
 */
char* read_file(const char* filename);


#endif
