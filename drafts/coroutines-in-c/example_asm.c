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

struct secondary_stack toplevel;
struct secondary_stack *st[1024] = {&toplevel};
int current = 0;


void *old_rsp;
void *old_rbp;

void (*start_f)();


void *resume_rsp;
void *resume_rbp;

bool start(struct secondary_stack *s, void (*f)()) {
  resume_rsp = s->suspend_rsp;
  resume_rbp = s->suspend_rbp;
  start_f = f;

  asm("mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=r"(old_rsp), "=r"(old_rbp)
      :
      :);
  asm("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "r"(resume_rsp), "r"(resume_rbp)
      :);

  asm("call *%0" : : "r"(start_f) :);

  asm("mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=r"(resume_rsp), "=r"(resume_rbp)
      :
      :);
  asm("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "r"(old_rsp), "r"(old_rbp)
      :);

  return false;
}

bool resume(struct secondary_stack *s) {
  //resume_rsp = s->suspend_rsp;
  //resume_rbp = s->suspend_rbp;
  asm volatile("mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=m"(old_rsp), "=m"(old_rbp)
      :
      :);
  asm volatile("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "m"(resume_rsp), "m"(resume_rbp)
      :);

  asm volatile(
      "ret \n"
      "yield:");

  asm volatile("mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=m"(resume_rsp), "=m"(resume_rbp)
      :
      :);
  asm volatile("mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      :
      : "m"(old_rsp), "m"(old_rbp)
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
