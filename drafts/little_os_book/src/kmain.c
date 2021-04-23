#include "io.h"

static char *const fb = ((char *const) 0x000B8000);

const unsigned short INDEX_PORT = 0x3D4;
const unsigned short DATA_PORT = 0x3D5;

const char CURSOR_POS_HIGH = 14;
const char CURSOR_POS_LOW = 15;

void putchar(
	     unsigned char row, unsigned char col,
	     char c,
	     unsigned char fg, unsigned char bg) {
  unsigned int loc = 2 * (80 * row + col);
  fb[loc] = c;
  fb[loc + 1] = (bg << 4) | fg;
}

unsigned short find_cursor() {
  unsigned short result = 0;
  outb(INDEX_PORT, CURSOR_POS_HIGH);
  result |= (inb(DATA_PORT) << 8);
  outb(INDEX_PORT, CURSOR_POS_LOW);
  result |= inb(DATA_PORT);
  return result;
}

void move_cursor(unsigned char row, unsigned char col) {
  unsigned short loc = 80 * row + col;

  outb(INDEX_PORT, CURSOR_POS_HIGH);
  outb(DATA_PORT, loc >> 8);
  outb(INDEX_PORT, CURSOR_POS_LOW);
  outb(DATA_PORT, loc);
}

void writechar(char c, unsigned char fg, unsigned char bg) {
  unsigned short cur = find_cursor();
  unsigned char row = cur / 80;
  unsigned char col = cur % 80;
  putchar(row, col, c, fg, bg);
  move_cursor(row, col + 1);
}

void write(char *buf, unsigned int len) {
  unsigned int cnt;
  for (cnt = 0; cnt < len; ++cnt) {
    writechar(buf[cnt], 15, 2);
  }
}

void kmain() {
  write("  Hello, world!  ", 17);
  for (;;);
}
