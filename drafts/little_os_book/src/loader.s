	;; Export the "loader" symbol
	;; It will be configured as an entry point in the linker script
	global loader

	;; Define several constants for later use
        MAGIC_NUMBER equ 0x1BADB002
        FLAGS        equ 0x0
        CHECKSUM     equ -MAGIC_NUMBER

	;; The code section starts
        section .text:
	;; First comes the GRUB header.
	;; It must be longword aligned.
        align 4

        dd MAGIC_NUMBER
	dd FLAGS
        dd CHECKSUM
	;; Next comes the loader code.
loader:
	mov eax, 0xCAFEBABE
	.loop:
        jmp .loop
