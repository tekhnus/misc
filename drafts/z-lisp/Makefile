SRC_FILES := $(wildcard src/main/*.c)
OBJ_FILES := $(patsubst src/main/%.c,target/obj/%.o,$(SRC_FILES))

TEST_SRC_FILES := $(wildcard src/test/*.c)
TEST_OBJ_FILES := $(patsubst src/test/%.c,target/test-obj/%.o,$(TEST_SRC_FILES))

CFLAGS=-Isrc/main

target/dzeta: $(OBJ_FILES)
	gcc $(CFLAGS) -o $@ $^

target/lex_test: target/test-obj/lex_test.o target/obj/dzeta_lex.o target/obj/dzeta_io.o
	gcc $(CFLAGS) -o $@ $^

target/obj/%.o: src/main/%.c
	mkdir -p $(dir $@)
	gcc $(CFLAGS) -c -o $@ $<

target/test-obj/%.o: src/test/%.c
	mkdir -p $(dir $@)
	gcc $(CFLAGS) -c -o $@ $<
