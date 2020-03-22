SRC_MAIN_OMP_FILES := $(wildcard src/main/omp/*/main.c)
TARGET_OMP_FILES := $(patsubst src/main/omp/%/main.c,target/omp/%,$(SRC_MAIN_OMP_FILES))

CFLAGS=-Isrc/main

all: target/omp/heat_omp_static target/omp/heat_omp_dynamic target/omp/heat_omp_guided target/omp/hello_omp target/mpi/heat_mpi target/mpi/integrate_mpi

target/omp/heat_omp_static target/omp/heat_omp_dynamic target/omp/heat_omp_guided: target/omp/heat_omp_%: target/obj/omp/heat_omp/main_%.o
	mkdir -p $(dir $@)
	gcc $(CFLAGS) -fopenmp -o $@ $^

target/omp/hello_omp: target/obj/omp/hello_omp/main.o
	mkdir -p $(dir $@)
	gcc $(CFLAGS) -fopenmp -o $@ $^

target/mpi/%: target/obj/mpi/%/main.o
	mkdir -p $(dir $@)
	mpicc $(CFLAGS) -o $@ $^ -lm

target/obj/omp/heat_omp/main_static.o: src/main/omp/heat_omp/main.c
	mkdir -p $(dir $@)
	gcc $(CFLAGS) -fopenmp -DUSE_STATIC -DSTATIC_CHUNK=8 -c -o $@ $<

target/obj/omp/heat_omp/main_dynamic.o: src/main/omp/heat_omp/main.c
	mkdir -p $(dir $@)
	gcc $(CFLAGS) -fopenmp -DUSE_DYNAMIC -c -o $@ $<

target/obj/omp/heat_omp/main_guided.o: src/main/omp/heat_omp/main.c
	mkdir -p $(dir $@)
	gcc $(CFLAGS) -fopenmp -DUSE_GUIDED -c -o $@ $<

target/obj/omp/hello_omp/main.o: src/main/omp/hello_omp/main.c
	mkdir -p $(dir $@)
	gcc $(CFLAGS) -fopenmp -c -o $@ $<

target/obj/mpi/%/main.o: src/main/mpi/%/main.c
	mkdir -p $(dir $@)
	mpicc $(CFLAGS) -c -o $@ $<

.PHONY: all
