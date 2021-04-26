#include <stdint.h>
#include <stdbool.h>
#include "io.h"

static char *const fb = ((char *const) 0x000B8000);

const unsigned short INDEX_PORT = 0x3D4;
const unsigned short DATA_PORT = 0x3D5;

const char CURSOR_POS_HIGH = 14;
const char CURSOR_POS_LOW = 15;

struct gdt_entry {
  uint16_t limit_low;
  uint16_t base_low;
  uint8_t base_mid;
  uint8_t access;
  uint8_t limit_high_and_flags;
  uint8_t base_high;
} __attribute__((packed));

struct gdt_entry gdt[3];

struct {
  uint16_t size;
  uint32_t offset;
} __attribute__((packed)) gdt_descriptor;

void gdt_set_access(struct gdt_entry *instance, bool is_executable) {
  instance->access = 0x92 | (is_executable << 3);
}

void gdt_set_flags(struct gdt_entry *instance) {
  instance->limit_high_and_flags = (instance->limit_high_and_flags & 0x0F) | 0xC0;
}

void gdt_set_limit(struct gdt_entry *instance, uint32_t value) {
  instance->limit_low = value & 0xFFFF;
  instance->limit_high_and_flags = (instance->limit_high_and_flags & 0xF0) | ((value & 0xF0000) >> 16);
}

void init_gdt_and_descriptor() {
  gdt_set_limit(&gdt[1], 0xFFFFF);
  gdt_set_flags(&gdt[1]);
  gdt_set_access(&gdt[1], true);

  gdt_set_limit(&gdt[2], 0xFFFFF);
  gdt_set_flags(&gdt[2]);
  gdt_set_access(&gdt[2], false);

  gdt_descriptor.size = sizeof(gdt) - 1;
  gdt_descriptor.offset = (uintptr_t) &gdt;
}

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
