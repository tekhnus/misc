/* Make the "loader" symbol an entry point. */
ENTRY(loader)

SECTIONS {
	 /* The location counter is shifted to 1MB,
	 because addresses below 1MB are reserved. */
         . = 0x00100000;

         .text ALIGN (0x1000) :
         {
                *(.text)
         }

         .rodata ALIGN (0x1000) :
         {
                *(.rodata*)
         }

         .data ALIGN (0x1000) :
         {
                *(.data)
         }

         .bss ALIGN (0x1000):
         {
                *(COMMON)
                *(.bss)
         }
}
