LINK.o = $(LINK.cc)
CXXFLAGS=-std=c++17 -g -O0
example: example.o
example.o: example.cpp
clean:
	rm *.o example
format:
	clang-format-12 -i *.cpp utils/*.hpp
generate-database:
	make clean
	bear make
	compdb list >compile_commands_.json
	mv compile_commands_.json compile_commands.json
