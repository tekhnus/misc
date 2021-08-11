all: build/example_asm

build/co.o: co.c
	mkdir -p build
	${CC} -c co.c -o build/co.o

build/example_asm.o: example_asm.c
	mkdir -p build
	${CC} -c example_asm.c -o build/example_asm.o

build/example_asm: build/co.o build/example_asm.o
	mkdir -p build
	${CC} build/co.o build/example_asm.o -o build/example_asm

clean:
	rm -rf build/*

.PHONY: all clean
