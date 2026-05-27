FC=gfortran
FFLAGS=-Wall -std=f2018 -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -flto -fopenmp -fopenmp-allocators
FOPT=-O3 -fstack-arrays -ftree-vectorize -fopt-info-vec -march=native -fopenmp

all:
	$(FC) $(FFLAGS) $(FOPT) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) $(FOPT) -c src/config.f90 -J obj/ -o obj/config.o
	$(FC) $(FFLAGS) $(FOPT) -c src/io_process.f90 -J obj/ -o obj/io_process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/process.f90 -J obj/ -o obj/process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app obj/environment.o obj/config.o obj/io_process.o obj/process.o obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; ./app;
	cat bin/output.txt

#FC=gfortran
#FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -Wno-unused-function -static-libgfortran -flto
#FOPT=-O3 -ftree-vectorize -fopt-info-vec
#
#all:
#	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
#	$(FC) $(FFLAGS) -c src/lab_io.f90 -J obj/ -o obj/lab_io.o
#	$(FC) $(FFLAGS) $(FOPT) -c src/lab_calculate.f90 -J obj/ -o obj/lab_calculate.o
#	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
#	$(FC) $(FFLAGS) $(FOPT) -o bin/app ./obj/environment.o obj/lab_calculate.o obj/lab_io.o obj/main.o
#
#clean:
#	rm -rf obj/*
#	rm -rf bin/*
#
#run:
#	cd ./bin; ./app;
#	cat bin/output.txt
