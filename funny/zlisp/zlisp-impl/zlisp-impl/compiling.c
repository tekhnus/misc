#include <zlisp-impl/compiling.h>
#include <stdlib.h>

prog *prog_make() {
  prog *res = malloc(sizeof(prog));
  res->type = PROG_END;
  return res;
}
