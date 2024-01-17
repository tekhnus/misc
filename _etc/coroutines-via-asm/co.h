#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

/* SYMMETRIC COROUTINE IMPLEMENTATION */

typedef void *co_t;

co_t co_new(void *stack_top);

void co_switch(co_t *a, co_t const *b, void (*f)(void));

/* ASYMMETRIC COROUTINE FACILITIES */

void co_yield();

void co_start(co_t *c, void (*f)());

void co_resume(co_t *c);

/* HELPERS FOR PASSING VALUES */

void co_yield_int(int val);

int co_resume_receive_int(co_t *s);

/* HELPERS FOR LAUNCHING FUNCTIONS CONVENIENTLY */

void co_init(co_t *s, void (*f)(), bool *fin);
