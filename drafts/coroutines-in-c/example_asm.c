#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct secondary_stack {
  void *suspend_rsp;
  void *suspend_rbp;
};

void swap(int *a, int *b) {
  int tmp = *a;
  *a = *b;
  *b = tmp;
}

bool yield();

void fib(void) {
  yield();
  int a = 0, b = 1;
  for (; a < 100;) {
    printf("%d\n", a);
    a = a + b;
    swap(&a, &b);
    yield();
  }
}

struct secondary_stack toplevel;
struct secondary_stack *st[1024] = {&toplevel};
int current = 0;


void (*start_f)();

bool start(struct secondary_stack *s, void (*f)()) {
  start_f = f;

  asm("mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=r"(st[current]->suspend_rsp), "=r"(st[current]->suspend_rbp)
      :
      :);
  ++current;
  st[current] = s;
  asm("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "r"(st[current]->suspend_rsp), "r"(st[current]->suspend_rbp)
      :);

  asm("call *%0" : : "r"(start_f) :);

  asm("mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=r"(st[current]->suspend_rsp), "=r"(st[current]->suspend_rbp)
      :
      :);
  --current;
  asm("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "r"(st[current]->suspend_rsp), "r"(st[current]->suspend_rbp)
      :);

  return false;
}

bool resume(struct secondary_stack *s) {
  asm volatile("mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=m"(st[current]->suspend_rsp), "=m"(st[current]->suspend_rbp)
      :
      :);
  ++current;
  st[current] = s;
  asm volatile("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "m"(st[current]->suspend_rsp), "m"(st[current]->suspend_rbp)
      :);

  return false;
};

bool yield() {
  asm volatile("mov %%rsp, %0 \n"
               "mov %%rbp, %1 \n"
               : "=m"(st[current]->suspend_rsp), "=m"(st[current]->suspend_rbp)
               :
               :);
  --current;  
  asm volatile("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "m"(st[current]->suspend_rsp), "m"(st[current]->suspend_rbp)
      :);

  return true;
}

struct secondary_stack secondary_stack_make(size_t n) {
  n = n - n % 16 + 1;  
  struct secondary_stack res;
  char *suspend_stack = malloc(n);
  res.suspend_rsp = suspend_stack + n - 1;
  return res;
}

#define KiB 1024
#define MiB 1048576

int main(void) {
  struct secondary_stack thestack = secondary_stack_make(1 * MiB);  

  start(&thestack, fib);
  while (resume(&thestack))
    ;
  return 0;
}
