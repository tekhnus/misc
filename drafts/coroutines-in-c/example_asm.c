#include <stdio.h>
#include <stdlib.h>


char _Alignas(16) suspend_stack[1024 * 1024 * 32];
void *suspend_rsp = &suspend_stack[1024 * 1024 * 32 - 16];
void *suspend_rbp;

void swap(int *a, int *b) {
  int tmp = *a;
  *a = *b;
  *b = tmp;
}

void fib(void) {
  asm("call yield \n");
  int a = 0, b = 1;
  for (;;) {
    printf("%d\n", a);
    a = a + b;
    swap(&a, &b);
    asm("call yield \n");
  }
}

void *old_rsp;
void *old_rbp;

void init(void) {
  asm(
      "mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=r"(old_rsp), "=r"(old_rbp)
      : /* nope */
      : /* nope */
      );
  asm(
      "mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      : /* nope */
      : "r"(suspend_rsp), "r"(suspend_rbp)
      : /* nope */
      );
  asm("call _fib");
    asm(
	// Save rsp and rbp
	"mov %%rsp, %0 \n"
	"mov %%rbp, %1 \n"
    
	: "=r"(suspend_rsp), "=r"(suspend_rbp)
	:
	:
	);
    asm(
	"mov %0, %%rsp \n"
	"mov %1, %%rbp \n"
	: /* nope */
	: "r"(old_rsp), "r"(old_rbp)
	: /* nope */
	);
}

void resume(void) {
  asm(
      "mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=r"(old_rsp), "=r"(old_rbp)
      : /* nope */
      : /* nope */
      );
  asm(
      "mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      : /* nope */
      : "r"(suspend_rsp), "r"(suspend_rbp)
      : /* nope */
      );
  asm(
      "ret \n");
  asm("yield:");
  asm(
      // Save rsp and rbp
      "mov %%rsp, %0 \n"
      "mov %%rbp, %1 \n"
      : "=r"(suspend_rsp), "=r"(suspend_rbp)
      :
      :
      );
  asm(
      "mov %0, %%rsp \n"
      "mov %1, %%rbp \n"
      : /* nope */
      : "r"(old_rsp), "r"(old_rbp)
      : /* nope */
      );
  //printf("still alive\n");
}

void bigfun() {
  char bigarr[300000];
  printf("%c\n", bigarr[300000 - 1]);
}

int main(void) {
  init();
  for (int i = 0; i < 10; ++i) {
    resume();
  }
  //printf("hello, world!\n");
  //bigfun();
  return 0;
}
