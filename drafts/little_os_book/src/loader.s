	;; Export the "loader" symbol
	;; It will be configured as an entry point in the linker script
	global loader
	global outb
	global inb
	;; Those are the names of C functions and variables we'll use.
	extern init_gdt_and_descriptor
	extern gdt_descriptor
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

	section .bss
	align 4
kernel_stack:
	resb KERNEL_STACK_SIZE
