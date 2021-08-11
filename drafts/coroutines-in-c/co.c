#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* SYMMETRIC COROUTINE IMPLEMENTATION */

typedef void *co_t;

co_t co_new(void *stack_top) { return stack_top; }

void co_switch(co_t *a, co_t const *b, void (*f)(void)) {
  // off-stack storage
  static co_t *a_;
  static co_t const *b_;
  static void (*f_)(void);

  a_ = a;
  b_ = b;
  f_ = f;

  asm volatile("push %%rax \n"
               "push %%rcx \n"
               "push %%rdx \n"
               "push %%r8 \n"
               "push %%r9 \n"
               "push %%r10 \n"
               "push %%r11 \n"

               "push %%rsi \n"
               "push %%rdi \n"

               "push %%rbx \n"
               "push %%rbp \n"
               "push %%r12 \n"
               "push %%r13 \n"
               "push %%r14 \n"
               "push %%r15 \n"

               "mov %%rsp, %0 \n"
               "mov %1, %%rsp \n"

               "cmpq $0, %2 \n"
               "jz switch_context_resume \n"
               "jmp *%2 \n"
               "switch_context_resume: \n"

               "pop %%r15 \n"
               "pop %%r14 \n"
               "pop %%r13 \n"
               "pop %%r12 \n"
               "pop %%rbp \n"
               "pop %%rbx \n"

               "pop %%rdi \n"
               "pop %%rsi \n"

               "pop %%r11 \n"
               "pop %%r10 \n"
               "pop %%r9 \n"
               "pop %%r8 \n"
               "pop %%rdx \n"
               "pop %%rcx \n"
               "pop %%rax \n"

               : "=m"(*a_)
               : "m"(*b_), "m"(f_)
               : "memory");
}

/* ASYMMETRIC COROUTINE FACILITIES */

static co_t co_main;
static co_t *co_stack[1024] = {&co_main};
static int co_current = 0;

void co_yield() {
  --co_current;
  co_switch(co_stack[co_current + 1], co_stack[co_current], NULL);
}

void co_start(co_t *c, void (*f)()) {
  ++co_current;
  co_stack[co_current] = c;
  co_switch(co_stack[co_current - 1], co_stack[co_current], f);
}

void co_resume(co_t *c) {
  ++co_current;
  co_stack[co_current] = c;
  co_switch(co_stack[co_current - 1], co_stack[co_current], NULL);
}

/* HELPERS FOR PASSING VALUES */

static int val_int;

void co_yield_int(int val) {
  val_int = val;
  co_yield();
}

int co_resume_receive_int(co_t *s) {
  co_resume(s);
  return val_int;
};

/* HELPERS FOR LAUNCHING FUNCTIONS CONVENIENTLY */

// the "arguments" for `co_wrapper` function are provided via global variables.
static void (*co_wrapper_f)();
static bool *co_wrapper_fin;

static void co_wrapper() {
  void (*co_f_)() = co_wrapper_f;
  bool *co_fin_ = co_wrapper_fin;

  co_yield();
  co_f_();
  *co_fin_ = true;
  for (;;) {
    co_yield();
  }
}

void co_init(co_t *s, void (*f)(), bool *fin) {
  co_wrapper_f = f;
  co_wrapper_fin = fin;
  co_start(s, co_wrapper);
}
