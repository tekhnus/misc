SRC_FILES := $(wildcard src/main/*.cpp)
OBJ_FILES := $(patsubst src/main/%.cpp,target/obj/%.o,$(SRC_FILES)) target/obj/pascal.o target/obj/tokens.o

LDLIBS := -ll  # IMPORTANT: change this to -lfl on Linux.
CXXFLAGS := -std=c++11
YACCFLAGS := -d -v

.PHONY: all gen run-examples clean

all: target/pascal

target/pascal: $(OBJ_FILES)
	$(CXX) -o $@ $^ $(LDLIBS)

target/obj/%.o: src/main/%.cpp | target/obj gen
	$(CXX) -c -o $@ $(CXXFLAGS) $<

target/obj/%.o: target/gen/%.cpp | target/obj
	$(CXX) -c -o $@ $(CXXFLAGS) -Isrc/main $<

target/obj:
	mkdir -p target/obj

gen: target/gen/pascal.cpp target/gen/tokens.cpp

target/gen/pascal.cpp: src/main/pascal.y | target/gen
	$(YACC) $(YACCFLAGS) -o $@ $^

target/gen/tokens.cpp: src/main/tokens.l | target/gen
	$(LEX) $(LFLAGS) -o $@ $^

target/gen:
	mkdir -p target/gen

run-examples: src/examples/*.pas target/pascal
	for example in src/examples/*.pas ; do \
		echo === Running $${example} ; \
		target/pascal $${example} ; \
	done

clean:
	rm -rf target/
