	;; Export the "loader" symbol
	;; It will be configured as an entry point in the linker script
	global loader
	;; Export the ISR symbols; their addresses are needed by the C code
	;; which sets up the IDT.
	;; Export the outb and inb functions; they are used in I/O C code.
	global outb
	global inb
	;; Those are the names of C functions and variables we'll use.
	extern init_gdt_and_descriptor
	extern gdt_descriptor
	extern init_idt_and_descriptor
	extern idt_descriptor
	extern interrupt_handler
	extern initialize_pics
	extern kmain

	;; Define several constants for later use
        MAGIC_NUMBER equ 0x1BADB002
        FLAGS        equ 0x0
        CHECKSUM     equ -MAGIC_NUMBER
	KERNEL_STACK_SIZE equ 4096

	;; The code section starts
        section .text
	;; First comes the GRUB header.
	;; It must be longword aligned.
        align 4

        dd MAGIC_NUMBER
	dd FLAGS
        dd CHECKSUM
	;; Next comes the loader code.
loader:
	;; We make the esp point to the first
	;; address after (= below) the segment reserved for the stack.
	mov esp, kernel_stack + KERNEL_STACK_SIZE
	;; TODO: shouldn't we also set ebp here?
	;; Now we can call the C code!

	;; Since the loader is booted by GRUB using the Multiboot standard,
	;; the CPU is already in the protected mode, and the segments
	;; with full range (0x00000000-0xFFFFFFFF) are used for code and data.
	;; However, we initialize our own GDT
	;; just in order to be sure that we control it fully.
	call init_gdt_and_descriptor   ; We init the GDT table and its descriptor.
	lgdt [gdt_descriptor]	       ; And load it.
	mov ax, 0x10
	mov ds, ax ; 0x10 Means the second (e.g. data) entry
	mov es, ax
	mov fs, ax
	mov gs, ax
	mov ss, ax
	jmp 0x08:justnextline ; 0x08 means the first (e.g. code) entry in the GDT
justnextline:

	;; Now we init and load the IDT.
	call init_idt_and_descriptor
	lidt [idt_descriptor]

	; FIXME added just to test general protection fault
	; mov ax, 0x33
	; mov ss, ax

	;; The interrupts from PIC's are disabled by GRUB.
	;; Before we enable them, we are going to reconfigure them,
	;; because their offsets in IDT overlap with CPU exception IRQ's.
	call initialize_pics

	;; Now we are going to allow CPU to respond to external interrupts.
	sti

	call kmain
	.loop:
        jmp .loop
outb:			  ; void outb(unsigned short port, unsigned char data)
	mov al, [esp + 8] ; data
	mov dx, [esp + 4] ; port
	out dx, al
	ret
inb:	      ; unsigned char inb(unsigned short port)
	mov dx, [esp + 4]
	xor eax, eax ; Just in case. TODO: find out if this is really needed
	in al, dx
	ret

	%macro isr_wo_error_code 1
	global isr%1
isr%1:
	push 0			; push an error code stub
	push %1 ; push the interrupt code
	jmp isr_common
	%endmacro

	%macro isr_w_error_code 1
	global isr%1
isr%1:
	; the error code is already pushed by the CPU
	push %1 ; push the interrupt code
	jmp isr_common
	%endmacro

	isr_wo_error_code 0
	isr_wo_error_code 1
	isr_wo_error_code 2
	isr_wo_error_code 3
	isr_wo_error_code 4
	isr_wo_error_code 5
	isr_wo_error_code 6
	isr_wo_error_code 7
	isr_w_error_code 8
	isr_wo_error_code 9
	isr_w_error_code 10
	isr_w_error_code 11
	isr_w_error_code 12
	isr_w_error_code 13
	isr_w_error_code 14
	; 15 reserved
	isr_wo_error_code 16
	isr_w_error_code 17
	isr_wo_error_code 18
	isr_wo_error_code 19
	isr_wo_error_code 20
	; 21-29 reserved
	isr_w_error_code 30
	; 31 reserved
	
isr_common:	
	pushad			; push the register values
	call interrupt_handler	; call the C handler
	popad			; restore the register values
	add esp, 8		; pop the interrupt code (pushed earlier by us) and error code pushed by CPU (or its stub pushed earlier by us)
	;; Now the registers are restored back to the state they were when the interrupt fired.
	;; The stack is restored too (modulo the error code, which was popped from the top
	;; of the stack, if it was there in the first place).
	;; We are in a position to call iret.
	iret
	
	section .bss
	align 4
kernel_stack:
	resb KERNEL_STACK_SIZE
