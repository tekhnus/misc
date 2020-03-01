SRC_FILES := $(wildcard src/main/*.cpp)
OBJ_FILES := $(patsubst src/main/%.cpp,target/obj/%.o,$(SRC_FILES))

target/mandelbrot: $(OBJ_FILES)
	g++ -o $@ $^ -lncurses

target/obj/%.o: src/main/%.cpp target/obj
	g++ -c -o $@ $<

target/obj:
	mkdir -p target/obj
