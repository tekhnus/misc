#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct secondary_stack {
  void *suspend_rsp;
  void *suspend_rbp;
};

struct secondary_stack toplevel;
struct secondary_stack *st[1024] = {&toplevel};
int current = 0;

#define save_n_switch()                                                        \
  asm volatile("mov %%rsp, %0 \n"                                              \
               "mov %%rbp, %1 \n"                                              \
               "mov %2, %%rsp \n"                                              \
               "mov %3, %%rbp \n"                                              \
               : "=m"(saved_rsp), "=m"(saved_rbp)                              \
               : "m"(new_rsp), "m"(new_rbp)                                    \
               : "memory")

#define co_stack_push(s)                                                       \
  new_rsp = s->suspend_rsp;                                                    \
  new_rbp = s->suspend_rbp;                                                    \
  save_n_switch();                                                             \
  st[current]->suspend_rsp = saved_rsp;                                        \
  st[current]->suspend_rbp = saved_rbp;                                        \
  ++current;                                                                   \
  st[current] = s;

#define co_stack_pop()                                                         \
  --current;                                                                   \
  new_rsp = st[current]->suspend_rsp;                                          \
  new_rbp = st[current]->suspend_rbp;                                          \
  save_n_switch();                                                             \
  ++current;                                                                   \
  st[current]->suspend_rsp = saved_rsp;                                        \
  st[current]->suspend_rbp = saved_rbp;                                        \
  --current;

#define START 1
#define RESUME 2
#define YIELD 3

void switch_context(int what, struct secondary_stack *s, void (*m)(void)) {
  // off-stack storage
  static struct secondary_stack *s_copy;
  static void (*m_copy)(void);
  static void *saved_rsp;
  static void *saved_rbp;
  static void *new_rsp;
  static void *new_rbp;

  s_copy = s;
  m_copy = m;
  switch (what) {
  case START:
    co_stack_push(s_copy);
    m_copy();
    return;
  case RESUME:
    co_stack_push(s_copy);
    return;
  case YIELD:
    co_stack_pop();
    return;
  }
  return;
}

void yield() { switch_context(YIELD, NULL, NULL); }

void start_loop(struct secondary_stack *s, void (*m)()) {
  switch_context(START, s, m);
}

void resume(struct secondary_stack *s) { switch_context(RESUME, s, NULL); }

struct secondary_stack secondary_stack_make(char *rsp) {
  struct secondary_stack res;
  res.suspend_rsp = rsp;
  res.suspend_rbp = rsp;
  return res;
}

void (*co_f)();
bool *co_fin;

void co_main() {
  bool *co_fin_copy = co_fin;
  co_f();
  *co_fin_copy = true;
  for (;;) {
    yield();
  }
}

void start_function(struct secondary_stack *s, bool *fin, void (*f)()) {
  co_f = f;
  co_fin = fin;
  start_loop(s, co_main);
}

int val_int;

void yield_int(int val) {
  val_int = val;
  yield();
}

int resume_receive_int(struct secondary_stack *s) {
  resume(s);
  return val_int;
};

int start_receive_int(struct secondary_stack *s, bool *fin, void (*f)()) {
  start_function(s, fin, f);
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

  bool fin = false;
  for (int x = start_receive_int(&s, &fin, fib); !fin;
       x = resume_receive_int(&s)) {
    printf("%d\n", x);
  }
  return 0;
}
