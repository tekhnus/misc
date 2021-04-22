static char *const fb = ((char *const) 0x000B8000);

void putchar(
	     unsigned char row, unsigned char col,
	     char c,
	     unsigned char fg, unsigned char bg) {
  unsigned int loc = 2 * (80 * row + col);
  fb[loc] = c;
  fb[loc + 1] = (bg << 4) | fg;
}

void write(char *buf, unsigned int len) {
  unsigned int cnt;
  for (cnt = 0; cnt < len; ++cnt) {
    putchar(0, cnt, buf[cnt], 15, 2);
  }
}

void kmain() {
  write("  Hello, world!  ", 17);
  for (;;);
}
