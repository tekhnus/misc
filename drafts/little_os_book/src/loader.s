	;; Export the "loader" symbol
	;; It will be configured as an entry point in the linker script
	global loader
	;; Those are the names of C functions we'll use.
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
	;; Now we can call the C code!
	call kmain
	.loop:
        jmp .loop

	section .bss
	align 4
kernel_stack:
	resb KERNEL_STACK_SIZE
