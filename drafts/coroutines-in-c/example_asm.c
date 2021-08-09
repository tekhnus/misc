#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

void (*co_f)();

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

#define START 1
#define RESUME 2
#define YIELD 3

void switch_context(struct secondary_stack *s, int what) {
  switch (what) {
  case START:
    co_stack_push(s);
    co_f();
    st[current]->finished = true;
    for (;;) {
      switch_context(NULL, YIELD);
    }
  case RESUME:
    co_stack_push(s);
    return;
  case YIELD:
    co_stack_pop();
    return;
  }
  return;
}

void yield() { switch_context(NULL, YIELD); }

void start(struct secondary_stack *s, void (*f)()) {
  co_f = f;
  switch_context(s, START);
}

void resume(struct secondary_stack *s) { switch_context(s, RESUME); }

struct secondary_stack secondary_stack_make(char *rsp) {
  struct secondary_stack res;
  res.suspend_rsp = rsp;
  res.suspend_rbp = rsp;
  res.finished = false;
  return res;
}

bool finished(struct secondary_stack *s) { return s->finished; }

int val_int;

void yield_int(int val) {
  val_int = val;
  yield();
}

int resume_receive_int(struct secondary_stack *s) {
  resume(s);
  return val_int;
};

int start_receive_int(struct secondary_stack *s, void (*f)()) {
  start(s, f);
  return val_int;
}

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

  for (int x = start_receive_int(&s, fib); !finished(&s);
       x = resume_receive_int(&s)) {
    printf("%d\n", x);
  }
  return 0;
}
