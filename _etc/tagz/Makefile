SRC_FILES := $(wildcard src/main/*/*/*.cpp)
OBJ_FILES := $(patsubst src/main/%.cpp,target/obj/%.o,$(SRC_FILES))

TEST_SRC_FILES := $(wildcard src/test/*.cpp)
TEST_OBJ_FILES := $(patsubst src/test/%.cpp,target/test-obj/%.o,$(TEST_SRC_FILES))

CXXFLAGS=-std=c++11 -Isrc/main

target/test: $(OBJ_FILES) $(TEST_OBJ_FILES)
	g++ $(CXXFLAGS) -o $@ $^

target/obj/%.o: src/main/%.cpp
	mkdir -p $(dir $@)
	g++ $(CXXFLAGS) -c -o $@ $<

target/test-obj/%.o: src/test/%.cpp
	mkdir -p $(dir $@)
	g++ $(CXXFLAGS) -c -o $@ $<
