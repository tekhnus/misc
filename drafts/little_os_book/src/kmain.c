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

/* ISR's for CPU exceptions. */
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

/* ISR's for IRQ's. */
void isr32();
void isr33();
void isr34();
void isr35();
void isr36();
void isr37();
void isr38();
void isr39();
void isr40();
void isr41();
void isr42();
void isr43();
void isr44();
void isr45();
void isr46();
void isr47();

void init_idt_and_descriptor() {
  /* ISR's for CPU exceptions. */
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

  /* ISR's for IRQ's. */
  idt_setup(&idt[32], (uint32_t) &isr32);
  idt_setup(&idt[33], (uint32_t) &isr33);
  idt_setup(&idt[34], (uint32_t) &isr34);
  idt_setup(&idt[35], (uint32_t) &isr35);
  idt_setup(&idt[36], (uint32_t) &isr36);
  idt_setup(&idt[37], (uint32_t) &isr37);
  idt_setup(&idt[38], (uint32_t) &isr38);
  idt_setup(&idt[39], (uint32_t) &isr39);
  idt_setup(&idt[40], (uint32_t) &isr40);
  idt_setup(&idt[41], (uint32_t) &isr41);
  idt_setup(&idt[42], (uint32_t) &isr42);
  idt_setup(&idt[43], (uint32_t) &isr43);
  idt_setup(&idt[44], (uint32_t) &isr44);
  idt_setup(&idt[45], (uint32_t) &isr45);
  idt_setup(&idt[46], (uint32_t) &isr46);
  idt_setup(&idt[47], (uint32_t) &isr47);

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

static const unsigned short KEYBOARD_DATA_PORT = 0x60;

static const char NON_PRINTABLE = '\0';
/* Scan code set 1 is used. */
static const char us_qwerty_printable[256] = {
  NON_PRINTABLE,
  NON_PRINTABLE,
  '1',
  '2',
  '3',
  '4',
  '5',
  '6',
  '7',
  '8',
  '9',
  '0',
  '-',
  '=',
  NON_PRINTABLE,
  NON_PRINTABLE, /* Tab */
  'q',
  'w',
  'e',
  'r',
  't',
  'y',
  'u',
  'i',
  'o',
  'p',
  '[',
  ']',
  NON_PRINTABLE,
  NON_PRINTABLE,
  'a',
  's',
  'd',
  'f',
  'g',
  'h',
  'j',
  'k',
  'l',
  ';',
  '\'',
  '`',
  NON_PRINTABLE,
  '\\',
  'z',
  'x',
  'c',
  'v',
  'b',
  'n',
  'm',
  ',',
  '.',
  '/',
  NON_PRINTABLE,
  '*',				/* KEYPAD */
  NON_PRINTABLE,
  ' ',
};
static const unsigned char US_QWERTY_ENTER = 0x1C;

void irq_keyboard_handler() {
  unsigned char scan_code = inb(KEYBOARD_DATA_PORT);
  bool released_flg = scan_code & 0x80;
  unsigned char key_code = scan_code & 0x7F;
  if (!released_flg) {
    char c = us_qwerty_printable[key_code];
    if (c != NON_PRINTABLE) {
      writechar(c);
    } else if (key_code == US_QWERTY_ENTER) {
      writechar('\n');
    }
  }
}

/* There are two Programmable Interrupt Controllers (PICS): master and slave.
   Each has two I/O ports. */
const unsigned short MASTER_PIC_PORT_A = 0x20;
const unsigned short MASTER_PIC_PORT_B = 0x21;
const unsigned short SLAVE_PIC_PORT_A = 0xA0;
const unsigned short SLAVE_PIC_PORT_B = 0xA1;

void irq_handler(uint32_t irq_code) {
  // kprintf("IRQ code: %u\n", (unsigned int) irq_code); // FIXME this is just to debug the IRQ.

  if (irq_code == 1) {
    irq_keyboard_handler();
  }
  /* We should always acknowledge the interrupt.
     It is done by sending an OCW2 to the port A of the PIC.
     0x20 means sending a non-specific End Of Interrupt (EOI) command.
  */
  if (irq_code > 7) {
    /* An acknowledgement is sent to slave PIC if the IRQ was fired by it. */
    outb(SLAVE_PIC_PORT_A, 0x20);
  }
  /* An acknowledgement is always sent to master PIC. */
  outb(MASTER_PIC_PORT_A, 0x20);
}

void interrupt_handler(struct registers regs, uint32_t int_code, uint32_t error_code) {
  /* We could add the following arguments: eip, cs, eflags (and maybe esp and ss),
     but currenly I see no use of them */

  if (int_code < 32) {
    global_bg = RED;
    kprintf("CPU exception occured!\n");
    kprintf("Interrupt code: %u\n", (unsigned int) int_code);
    kprintf("Error code: %u\n", (unsigned int) error_code);
    kprintf("EAX value: %u\n", (unsigned int) regs.eax);
    global_bg = BLACK;
    for (;;);
  }
  if (int_code < 48) {
    irq_handler(int_code - 32);
    return;
  }
  global_bg = RED;
  kprintf("Unexpected interrupt fired!\n");
  kprintf("Interrupt code: %u\n", (unsigned int) int_code);
  kprintf("Error code: %u\n", (unsigned int) error_code);
  global_bg = BLACK;
  for (;;);
}


void initialize_pics() {
  /* PIC initialization is done by sending four commands
     called Initialization Command Words (ICW) to its ports,
     after which a single Operation Control Word (OCW) is sent.
     ICW1 is sent to port A and ICW2, ICW3, ICW4 and OCW1 are sent to port B. */

  /* ICW1 is some technical flags. */
  outb(MASTER_PIC_PORT_A, 0x11);
  outb(SLAVE_PIC_PORT_A, 0x11);

  /* Some sources recommend to wait between sending ICW's. */

  /* ICW2 is offset (in entries, not in bytes!) in IDT. */
  outb(MASTER_PIC_PORT_B, 0x20); /* As entries 0x00-0x1F are mapped to CPU exceptions, we start from 0x20 */
  outb(SLAVE_PIC_PORT_B, 0x28); /* Entries 0x20-0x27 are mapped to master PIC above, so we map 0x28-0x2F to slave PIC. */

  /* ICW3. */
  outb(MASTER_PIC_PORT_B, 0x04); /* This is the mask of the pins which are connected to slave PIC's. We want to have a single slave connected to pin number 2. */
  outb(SLAVE_PIC_PORT_B, 0x02); /* This is the number of the master's pin to which this slave is connected. */

  /* ICW4 is some more technical flags. */
  /* Some sources send different ICW4 words. It's somehow related to buffering */
  outb(MASTER_PIC_PORT_B, 0x01);
  outb(SLAVE_PIC_PORT_B, 0x01);

  /* OCW1 commands to enable/disable some IRQ's. The zero mask means enable everything */
  outb(MASTER_PIC_PORT_B, 0x00);
  outb(SLAVE_PIC_PORT_B, 0x00);
}

void kmain() {
  global_bg = GREEN;
  kprintf("Hello, %s!\n", "world");
  global_bg = BLACK;
  // kprintf("%u\n", 1u / 0u); // FIXME added to test division by zero interrupt
  for (;;);
}
