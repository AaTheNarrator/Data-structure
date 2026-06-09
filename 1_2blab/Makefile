
FC=flang

FFLAGS = -fimplicit-none -fopenmp
FOPT   = -O3 -fopenmp 

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
	cd ./bin; LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 LD_LIBRARY_PATH=/usr/lib/llvm-19/lib ./app; cat output.txt
