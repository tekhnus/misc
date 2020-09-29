SRC_IMPL_FILES := $(wildcard src/main/impl/*.c)
OBJ_IMPL_FILES := $(patsubst src/main/%.c,target/obj/%.o,$(SRC_IMPL_FILES))

CFLAGS=-ansi -pedantic -Wall -Werror -fno-common
CXXFLAGS=-I/usr/local/include -I/usr/include/freetype2\
		 -std=c++11 -pedantic -Wall -Werror -fno-common
CONSOLE_LIBS=-ljack -lm
GUI_LIBS=-L/usr/local/lib\
		-lsfml-graphics -lsfml-audio -lsfml-network\
		-lsfml-window -lsfml-system -lSFGUI

.PHONY: all clean

all: target/organetto_console target/organetto_gui

target/organetto_console: $(OBJ_IMPL_FILES) target/obj/organetto_console.o
	$(CC) -o $@ $^ $(CONSOLE_LIBS)

target/organetto_gui: $(OBJ_IMPL_FILES) target/obj/organetto_gui.o
	$(CXX) -o $@ $^ $(GUI_LIBS) $(CONSOLE_LIBS)

target/obj/%.o: src/main/%.cpp | target/obj
	$(CXX) -c -o $@ $(CXXFLAGS) $<

target/obj/%.o: src/main/%.c | target/obj
	$(CC) -c -o $@ $(CFLAGS) $<

target/obj:
	mkdir -p target/obj/impl

clean:
	rm -rf target/
