all: build/co_O0.txt build/co_O1.txt

build/co_O0: example_asm.c
	gcc -O0 -g example_asm.c -o build/co_O0

build/co_O1: example_asm.c
	gcc -O1 -g example_asm.c -o build/co_O1

build/co_O0.txt: build/co_O0
	objdump -d build/co_O0 --no-leading-addr >build/co_O0.txt


build/co_O1.txt: build/co_O1
	objdump -d build/co_O1 --no-leading-addr >build/co_O1.txt

.PHONY: all
