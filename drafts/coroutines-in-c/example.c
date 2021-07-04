/* This is an implementation of a quite limited version of coroutines in C.
   See https://www.chiark.greenend.org.uk/~sgtatham/coroutines.html */
   
#include <stdlib.h>
#include <stdio.h>

typedef int execution_point_t;
#define new_execution_point -1

#define begin_coroutine_body(point) \
  execution_point_t *__point = point;	 \
  switch(*__point) {		 \
  case -1:

#define yield(val)  \
  *__point = __LINE__; \
  return val;	    \
  case __LINE__:

#define end_coroutine_body()			      \
  break;					      \
  default:					      \
  fprintf(stderr, "unreachable statement reached\n"); \
  exit(EXIT_FAILURE); }				      \

/* And we're done with the implementation. Next comes the example. */

struct decode_rle_state {
  char cur;
  char cnt;
};

/* The execution point and the state here are managed by the caller
   just for the purpose of demonstration.
   They could be stored as static variables. See the coroutine below. */
char decode_rle(execution_point_t *point, struct decode_rle_state *state) {
  /* A simplest run-length string decoder.
     Transforms "y#3x#1#z" into "yxxx#z"
     (read as "'y', 3 times 'x', 1 time '#', 'z') */
  begin_coroutine_body(point);

  for (;;) {
    state->cur = getchar();
    if (state->cur == EOF) {
      break;
    }
    if (state->cur == '#') {
      state->cnt = getchar() - '0';
      state->cur = getchar();
      for (; state->cnt > 0; --state->cnt) {
	yield(state->cur);
      }
      continue;
    }
    yield(state->cur);
  }
  for (;;) {
    yield(EOF);
  }

  end_coroutine_body();
}

char format_triples() {
  static execution_point_t point = new_execution_point;
  static int current_word_length = 0;

  static execution_point_t decode_rle_point = new_execution_point;
  static struct decode_rle_state decode_rle_state;

  begin_coroutine_body(&point);

  static char input;
  while ((input = decode_rle(&decode_rle_point, &decode_rle_state)) != EOF) {
    if (current_word_length == 3) {
      yield(' ');
      current_word_length = 0;
    }
    ++current_word_length;
    yield(input);
  }

  for (;;) {
    yield(EOF);
  }

  end_coroutine_body();
}

int main(void) {
  char c;
  while ((c = format_triples()) != EOF) {
    printf("%c", c);
  }

  return 0;
}
