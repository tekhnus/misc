#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct secondary_stack {
  void *suspend_rsp;
  void *suspend_rbp;
  bool finished;
};

void swap(int *a, int *b) {
  int tmp = *a;
  *a = *b;
  *b = tmp;
}

void yield(int val);

void fib(void) {
  yield(0);
  int a = 0, b = 1;
  for (; a < 100;) {
    // printf("%d\n", a);
    a = a + b;
    swap(&a, &b);
    yield(a);
  }
}

struct secondary_stack toplevel;
struct secondary_stack *st[1024] = {&toplevel};
int current = 0;

void (*start_f)();

int start(struct secondary_stack *s, void (*f)()) {
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
  st[current]->finished = true;
  --current;
  asm("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "r"(st[current]->suspend_rsp), "r"(st[current]->suspend_rbp)
      :);

  return 0;
}

int val_int;

int resume(struct secondary_stack *s) {
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

  asm("return_int:");
  return val_int;
};

void yield(int val) {
  val_int = val;
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
  asm("jmp return_int");
}

struct secondary_stack secondary_stack_make(size_t n) {
  n = n - n % 16 + 1;
  struct secondary_stack res;
  char *suspend_stack = malloc(n);
  res.suspend_rsp = suspend_stack + n - 1;
  res.finished = false;
  return res;
}

bool finished(struct secondary_stack *s) { return s->finished; }

#define KiB 1024
#define MiB 1048576

int main(void) {
  struct secondary_stack s = secondary_stack_make(1 * MiB);

  for (int x = start(&s, fib); !finished(&s); x = resume(&s)) {
    printf("%d\n", x);
  }
  return 0;
}
