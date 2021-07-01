SRC_FILES := $(wildcard src/main/*.c)
OBJ_FILES := $(patsubst src/main/%.c,target/obj/%.o,$(SRC_FILES))

TEST_SRC_FILES := $(wildcard src/test/*.c)
TEST_OBJ_FILES := $(patsubst src/test/%.c,target/test-obj/%.o,$(TEST_SRC_FILES))

CFLAGS=-Isrc/main -std=c11 -Wpedantic -Wall -I/usr/local/opt/libffi/include -g -fPIC
LDLIBS=-ldl -lffi
LDFLAGS=-L/usr/local/opt/libffi/lib

repl: target/zlisp-cli target/zlisp-zlisp.so

run-repl: repl
	target/zlisp-cli src/main/repl.lisp

target/zlisp-cli: target/obj/zlisp-cli.o target/obj/zlisp.o
	gcc $(LDFLAGS) $(LDLIBS) -o $@ $^

target/zlisp-zlisp.so: target/obj/zlisp-zlisp.o target/obj/zlisp.o
	gcc -shared $(LDFLAGS) $(LDLIBS) -o $@ $^

target/obj/%.o: src/main/%.c
	mkdir -p $(dir $@)
	gcc $(CFLAGS) -c -o $@ $<

clean:
	rm -rf target/

.PHONY: run-prelude clean repl
