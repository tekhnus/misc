OBJECTS = target/loader.o target/kmain.o
LD = ld
CC = gcc
CFLAGS = -ffreestanding -Wall -Wextra -c -march=i386 -m32 -fno-stack-protector
LDFLAGS = -T src/link.ld -melf_i386
AS = nasm
ASFLAGS = -f elf

.PHONY: run clean

target/os.iso: target/kernel.elf src/menu.lst stage2_eltorito
	mkdir -p target/iso/boot/grub
	cp src/menu.lst target/iso/boot/grub
	cp stage2_eltorito target/iso/boot/grub
	cp target/kernel.elf target/iso/boot
	genisoimage -R -b boot/grub/stage2_eltorito -no-emul-boot -boot-load-size 4 -A os -input-charset utf8 -quiet -boot-info-table -o target/os.iso target/iso

target/kernel.elf: $(OBJECTS) src/link.ld
	$(LD) $(LDFLAGS) $(OBJECTS) -o target/kernel.elf

target/%.o: src/%.s
	mkdir -p target
	$(AS) $(ASFLAGS) $< -o $@


target/%.o: src/%.c
	mkdir -p target
	$(CC) $(CFLAGS) $< -o $@

run: target/os.iso autorun.rc
	bochs -f bochsrc.txt -q -rc autorun.rc

debug: target/os.iso
	bochs -f bochsrc.txt -q

clean:
	rm -rf target
