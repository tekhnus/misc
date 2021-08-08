#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

char _Alignas(16) suspend_stack[1024 * 1024 * 32];
void *suspend_rsp = &suspend_stack[1024 * 1024 * 32 - 16];
void *suspend_rbp;

void swap(int *a, int *b) {
  int tmp = *a;
  *a = *b;
  *b = tmp;
}

void fib(void) {
  asm("call yield");
  int a = 0, b = 1;
  for (; a < 100;) {
    printf("%d\n", a);
    a = a + b;
    swap(&a, &b);
    asm("call yield");
  }
}

void *old_rsp;
void *old_rbp;

bool init(void) {
  asm("mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=r"(old_rsp), "=r"(old_rbp)
      :
      :);
  asm("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "r"(suspend_rsp), "r"(suspend_rbp)
      :);

  asm("call _fib");

  asm("mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=r"(suspend_rsp), "=r"(suspend_rbp)
      :
      :);
  asm("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "r"(old_rsp), "r"(old_rbp)
      :);

  return false;
}

bool resume(void) {
  asm("mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=r"(old_rsp), "=r"(old_rbp)
      :
      :);
  asm("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "r"(suspend_rsp), "r"(suspend_rbp)
      :);

  asm("ret \n"
      "yield:");

  asm("mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=r"(suspend_rsp), "=r"(suspend_rbp)
      :
      :);
  asm("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "r"(old_rsp), "r"(old_rbp)
      :);
  
  return true;
}

int main(void) {
  init();
  while (resume())
    ;
  return 0;
}
