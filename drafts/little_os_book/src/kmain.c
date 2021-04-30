#include <stdint.h>
#include <stdbool.h>
#include <stdarg.h>
#include <stddef.h>
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

  gdt_descriptor.size = sizeof(gdt) - 1; // The "minus one" part is a GDT-specific nuance.
  gdt_descriptor.offset = (uintptr_t) &gdt;
}

struct idt_entry {
  uint16_t offset_low;
  uint16_t segment_selector;
  uint8_t reserved;
  uint8_t flags;
  uint16_t offset_high;
} __attribute__((packed));

struct idt_entry idt[256];

struct {
  uint16_t size;
  uint32_t offset;
} __attribute((packed)) idt_descriptor;

void idt_init(struct idt_entry *instance) {
  instance->flags = 0x8E; /* privelege level is encoded here */
  instance->segment_selector = 1 * sizeof(struct gdt_entry); /* the GDT offset in bytes
							       of the code segment which should be used for running this handler */
}

void idt_set_offset(struct idt_entry *instance, uint32_t value) {
  instance->offset_low = value & 0xFFFF;
  instance->offset_high = (value >> 16);
}

/* ISR's are defined in loader.s */
void isr0();

void init_idt_and_descriptor() {
  idt_init(&idt[0]);
  idt_set_offset(&idt[0], (uint32_t) &isr0);

  idt_descriptor.size = sizeof(idt);
  idt_descriptor.offset = (uint32_t) &idt;
}

/* The following struct captures the results of 'pushad'.
   The order is from stack top to stack bottom (therefore from last pushed to first pushed).
 */
struct registers {
  uint32_t edi;
  uint32_t esi;
  uint32_t ebp;
  uint32_t esp;
  uint32_t ebx;
  uint32_t edx;
  uint32_t ecx;
  uint32_t eax;
} __attribute__((packed));

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

const unsigned char WHITE = 15;
const unsigned char RED = 4;
const unsigned char GREEN = 2;
const unsigned char BLACK = 0;

unsigned char global_fg = WHITE;
unsigned char global_bg = BLACK;

void writechar(char c) {
  unsigned short cur = find_cursor();
  unsigned char row = cur / 80;
  unsigned char col = cur % 80;
  if (c == '\n') {
    move_cursor(row + 1, 0);
    return;
  }
  putchar(row, col, c, global_fg, global_bg);
  move_cursor(row, col + 1);
}

void writestr(char *buf) {
  size_t cnt;
  for (cnt = 0; buf[cnt] != '\0'; ++cnt) {
    writechar(buf[cnt]);
  }
}

void writeunsigned(unsigned int value) {
  if (value == 0) {
    writechar('0');
    return;
  }
  while (value > 0) {
    writechar('0' + value % 10);
    value /= 10;
  }
}

void kprintf(char *fmt, ...) {
  size_t j;
  va_list ap;
  char *s;
  unsigned int u;
  va_start(ap, fmt);
  for (j = 0; fmt[j] != '\0'; ++j) {
    if (fmt[j] != '%') {
      writechar(fmt[j]);
      continue;
    }
    ++j;
    if (fmt[j] == 's') {
      s = va_arg(ap, char*);
      writestr(s);
      continue;
    }
    if (fmt[j] == 'u') {
      u = va_arg(ap, unsigned int);
      writeunsigned(u);
      continue;
    }
  }
  
}

void interrupt_handler(struct registers regs, uint32_t int_code, uint32_t error_code) {
  /* We could add the following arguments: eip, cs, eflags (and maybe esp and ss),
     but currenly I see no use of them */
  global_bg = RED;
  kprintf("Exception occured!\n");
  kprintf("Interrupt code: %u\n", (unsigned int) int_code);
  kprintf("Error code: %u\n", (unsigned int) error_code);
  kprintf("EAX value: %u\n", (unsigned int) regs.eax);
  global_bg = BLACK;
  for (;;);
}

void kmain() {
  global_bg = GREEN;
  kprintf("Hello, %s!\n", "world");
  global_bg = BLACK;
  kprintf("%u\n", 1u / 0u);
  for (;;);
}
