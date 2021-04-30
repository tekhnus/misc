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

void idt_setup(struct idt_entry *instance, uint32_t offset) {
  idt_init(instance);
  idt_set_offset(instance, offset);
}

/* ISR's are defined in loader.s */
void isr0();
void isr1();
void isr2();
void isr3();
void isr4();
void isr5();
void isr6();
void isr7();
void isr8();
void isr9();
void isr10();
void isr11();
void isr12();
void isr13();
void isr14();
// 15 reserved
void isr16();
void isr17();
void isr18();
void isr19();
void isr20();
// 21-29 reserved
void isr30();
// 31 reserved

void init_idt_and_descriptor() {
  idt_setup(&idt[0], (uint32_t) &isr0);
  idt_setup(&idt[1], (uint32_t) &isr1);
  idt_setup(&idt[2], (uint32_t) &isr2);
  idt_setup(&idt[3], (uint32_t) &isr3);
  idt_setup(&idt[4], (uint32_t) &isr4);
  idt_setup(&idt[5], (uint32_t) &isr5);
  idt_setup(&idt[6], (uint32_t) &isr6);
  idt_setup(&idt[7], (uint32_t) &isr7);
  idt_setup(&idt[8], (uint32_t) &isr8);
  idt_setup(&idt[9], (uint32_t) &isr9);
  idt_setup(&idt[10], (uint32_t) &isr10);
  idt_setup(&idt[11], (uint32_t) &isr11);
  idt_setup(&idt[12], (uint32_t) &isr12);
  idt_setup(&idt[13], (uint32_t) &isr13);
  idt_setup(&idt[14], (uint32_t) &isr14);
  // 15 reserved
  idt_setup(&idt[16], (uint32_t) &isr16);
  idt_setup(&idt[17], (uint32_t) &isr17);
  idt_setup(&idt[18], (uint32_t) &isr18);
  idt_setup(&idt[19], (uint32_t) &isr19);
  idt_setup(&idt[20], (uint32_t) &isr20);
  // 21-29 reserved
  idt_setup(&idt[30], (uint32_t) &isr30);
  // 31 reserved

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
  unsigned int decimalmask;
  for (decimalmask = 1; decimalmask * 10 < value; decimalmask *= 10);
  for (; decimalmask > 0; decimalmask /= 10) {
    writechar('0' + (value / decimalmask));
    value %= decimalmask;
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
  // kprintf("%u\n", 1u / 0u); // FIXME added to test division by zero interrupt
  for (;;);
}
