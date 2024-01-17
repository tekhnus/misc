/* This is a demonstration of stackful asymmetric coroutines.
   Context switching is implemented with the help of inline assembly. */
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "co.h"

void swap(int *a, int *b) {
  int tmp = *a;
  *a = *b;
  *b = tmp;
}

void fib(void) {
  int a = 0, b = 1;
  while (a < 100) {
    co_yield_int(a);
    a = a + b;
    swap(&a, &b);
  }
}

#define last_element(arr) arr + sizeof(arr) / sizeof(arr[0])

int main(void) {
  char stack[1 * 1024 * 1024];
  co_t s = co_new(last_element(stack));

  bool fin = false;
  co_init(&s, fib, &fin);
  for (int x; x = co_resume_receive_int(&s), !fin;) {
    printf("%d\n", x);
  }
  return 0;
}
