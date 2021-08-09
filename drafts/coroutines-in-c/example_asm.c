#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct secondary_stack {
  void *suspend_rsp;
  void *suspend_rbp;
  bool started;
  bool finished;
};

void swap(int *a, int *b) {
  int tmp = *a;
  *a = *b;
  *b = tmp;
}

void yield(int val);

void fib(void) {
  int a = 0, b = 1;
  while (a < 100) {
    yield(a);
    a = a + b;
    swap(&a, &b);
  }
}

struct secondary_stack toplevel;
struct secondary_stack *st[1024] = {&toplevel};
int current = 0;

void (*co_f)();

int val_int;

void co_main(void);

int start(struct secondary_stack *s, void (*f)()) {
  co_f = f;

  asm volatile("mov %%rsp, %0 \n"
               "mov %%rbp, %1 \n"
               : "=r"(st[current]->suspend_rsp), "=r"(st[current]->suspend_rbp)
               :
               :);
  ++current;
  st[current] = s;
  asm volatile("mov %0, %%rsp \n"
               "mov %1, %%rbp \n"
               :
               : "r"(st[current]->suspend_rsp), "r"(st[current]->suspend_rbp)
               :);
  asm volatile("jmp _co_main");

  asm volatile("yield_int_to_start:");
  s->started = true;
  return val_int;
}

void co_main(void) {
  co_f();

  st[current]->finished = true;
  yield(0);
}

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
  asm volatile("jmp send_void");

  asm volatile("yield_int_to_resume:");
  return val_int;
};

bool co_started;

void yield(int val) {
  co_started = st[current]->started;
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
  asm volatile("cmp $0, %0 \n"
               "jz yield_int_to_start \n"
               "jmp yield_int_to_resume"
               :
               : "r"(co_started));
  asm volatile("send_void:");
}

struct secondary_stack secondary_stack_make(size_t n) {
  n = n - n % 16 + 1;
  struct secondary_stack res;
  char *suspend_stack = malloc(n);
  res.suspend_rsp = suspend_stack + n - 1;
  res.suspend_rbp = res.suspend_rsp;
  res.started = false;
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
