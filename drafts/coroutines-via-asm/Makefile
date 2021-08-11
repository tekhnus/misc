all: build/example

build/co.o: co.c
	mkdir -p build
	${CC} -c co.c -o build/co.o

build/example.o: example.c
	mkdir -p build
	${CC} -c example.c -o build/example.o

build/example: build/co.o build/example.o
	mkdir -p build
	${CC} build/co.o build/example.o -o build/example

clean:
	rm -rf build/*

.PHONY: all clean
