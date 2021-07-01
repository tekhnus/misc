SRC_FILES := $(wildcard src/main/*.c)
OBJ_FILES := $(patsubst src/main/%.c,target/obj/%.o,$(SRC_FILES))

TEST_SRC_FILES := $(wildcard src/test/*.c)
TEST_OBJ_FILES := $(patsubst src/test/%.c,target/test-obj/%.o,$(TEST_SRC_FILES))

CFLAGS=-Isrc/main -std=c11 -Wpedantic -Wall -I/usr/local/opt/libffi/include -g -fPIC
LDLIBS=-ldl -lffi
LDFLAGS=-L/usr/local/opt/libffi/lib

target/z-lisp: $(OBJ_FILES)
	gcc $(LDFLAGS) $(LDLIBS) -o $@ $^

target/z-lisp.so: $(OBJ_FILES)
	gcc -shared $(LDFLAGS) $(LDLIBS) -o $@ $^

target/obj/%.o: src/main/%.c
	mkdir -p $(dir $@)
	gcc $(CFLAGS) -c -o $@ $<



run-repl: target/z-lisp target/z-lisp.so
	target/z-lisp src/main/prelude.lisp src/main/repl.lisp

clean:
	rm -rf target/

.PHONY: run-prelude clean
