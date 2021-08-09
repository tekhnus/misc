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

#define co_stack_push(s)                                                       \
  asm volatile("mov %%rsp, %0 \n"                                              \
               "mov %%rbp, %1 \n"                                              \
               : "=r"(st[current]->suspend_rsp),                               \
                 "=r"(st[current]->suspend_rbp)                                \
               :                                                               \
               :);                                                             \
  ++current;                                                                   \
  st[current] = s;                                                             \
  asm volatile("mov %0, %%rsp \n"                                              \
               "mov %1, %%rbp \n"                                              \
               :                                                               \
               : "r"(st[current]->suspend_rsp), "r"(st[current]->suspend_rbp)  \
               :)

#define co_stack_pop()                                                         \
  asm volatile("mov %%rsp, %0 \n"                                              \
               "mov %%rbp, %1 \n"                                              \
               : "=m"(st[current]->suspend_rsp),                               \
                 "=m"(st[current]->suspend_rbp)                                \
               :                                                               \
               :);                                                             \
  --current;                                                                   \
  asm volatile("mov %0, %%rsp \n"                                              \
               "mov %1, %%rbp \n"                                              \
               :                                                               \
               : "m"(st[current]->suspend_rsp), "m"(st[current]->suspend_rbp)  \
               :)

void co_resume_impl(struct secondary_stack *s, bool resume) {
  if (!resume) {
    co_stack_push(s);
    asm volatile("jmp _co_main");
  } else {
    co_stack_push(s);
    asm volatile("jmp continue_yield");
  }
  asm volatile("continue_resume:");
  return;
}

int start(struct secondary_stack *s, void (*f)()) {
  co_f = f;
  co_resume_impl(s, false);
  return val_int;
}

void co_main(void) {
  co_f();
  st[current]->finished = true;
  yield(0);
}

int resume(struct secondary_stack *s) {
  co_resume_impl(s, true);
  return val_int;
};

void yield_impl() {
  co_stack_pop();
  asm volatile("jmp continue_resume");
  asm volatile("continue_yield:");
}

void yield(int val) {
  val_int = val;
  yield_impl();
}

struct secondary_stack secondary_stack_make(size_t n) {
  n = n - n % 16 + 1;
  struct secondary_stack res;
  char *suspend_stack = malloc(n);
  res.suspend_rsp = suspend_stack + n - 1;
  res.suspend_rbp = res.suspend_rsp;
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
