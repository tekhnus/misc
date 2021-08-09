#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

void (*co_f)();
int val_int;

struct secondary_stack {
  void *suspend_rsp;
  void *suspend_rbp;
  bool finished;
};

struct secondary_stack toplevel;
struct secondary_stack *st[1024] = {&toplevel};
int current = 0;

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

void yield() {
  co_stack_pop();
  asm volatile("jmp continue_resume");
  asm volatile("continue_yield:");
}

void yield_int(int val) {
  val_int = val;
  yield();
}

void co_main(void) {
  co_f();
  st[current]->finished = true;
  yield();
}

void start_or_resume(struct secondary_stack *s, bool resume) {
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
  start_or_resume(s, false);
  return val_int;
}

void resume(struct secondary_stack *s) { start_or_resume(s, true); }

int resume_receive_int(struct secondary_stack *s) {
  resume(s);
  return val_int;
};

struct secondary_stack secondary_stack_make(char *rsp) {
  struct secondary_stack res;
  res.suspend_rsp = rsp;
  res.suspend_rbp = rsp;
  res.finished = false;
  return res;
}

bool finished(struct secondary_stack *s) { return s->finished; }

void swap(int *a, int *b) {
  int tmp = *a;
  *a = *b;
  *b = tmp;
}

void fib(void) {
  int a = 0, b = 1;
  while (a < 100) {
    yield_int(a);
    a = a + b;
    swap(&a, &b);
  }
}

#define last_element(arr) arr + sizeof(arr) / sizeof(arr[0])

int main(void) {
  char stack[1 * 1024 * 1024];
  struct secondary_stack s = secondary_stack_make(last_element(stack));

  for (int x = start(&s, fib); !finished(&s); x = resume_receive_int(&s)) {
    printf("%d\n", x);
  }
  return 0;
}
