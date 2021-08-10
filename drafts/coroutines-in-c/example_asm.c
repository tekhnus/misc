#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

struct secondary_stack {
  void *suspend_rsp;
  void *suspend_rbp;
  bool finished;
};

struct secondary_stack toplevel;
struct secondary_stack *st[1024] = {&toplevel};
int current = 0;

void *saved_rsp;
void *saved_rbp;
void *new_rsp;
void *new_rbp;

#define save_n_switch()                                                        \
  asm volatile("mov %%rsp, %0 \n"                                              \
               "mov %%rbp, %1 \n"                                              \
               "mov %2, %%rsp \n"                                              \
               "mov %3, %%rbp \n"                                              \
               : "=m"(saved_rsp), "=m"(saved_rbp)                              \
               : "m"(new_rsp), "m"(new_rbp))

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
  res.finished = false;
  return res;
}

bool finished(struct secondary_stack *s) { return s->finished; }

void (*co_f)();

void co_main() {
  co_f();
  st[current]->finished = true;
  for (;;) {
    yield();
  }
}

void start_function(struct secondary_stack *s, void (*f)()) {
  co_f = f;
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

int start_receive_int(struct secondary_stack *s, void (*f)()) {
  start_function(s, f);
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
