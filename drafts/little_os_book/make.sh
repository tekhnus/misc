#!/bin/bash
set -e
mkdir -p target
nasm -f elf32 -o target/loader.o src/loader.s
i386-elf-ld -T src/link.ld -melf_i386 target/loader.o -o target/kernel.elf
mkdir -p target/iso/boot/grub
cp src/menu.lst target/iso/boot/grub
cp stage2_eltorito target/iso/boot/grub
cp target/kernel.elf target/iso/boot
mkisofs -R -b boot/grub/stage2_eltorito -no-emul-boot -boot-load-size 4 -A os -input-charset utf8 -quiet -boot-info-table -o target/os.iso target/iso
bochs -f bochsrc.txt -q
