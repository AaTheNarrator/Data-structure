FC=flang

FFLAGS=-fimplicit-none -fopenmp 
FOPT=-O3 -fopenmp

all:
	$(FC) $(FFLAGS) $(FOPT) -c src/main.f90 -I obj/ -o obj/main.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/app obj/main.o

clean:
	rm -rf obj/*
	rm -rf bin/*

run:
	cd ./bin; LANG=en_US.UTF-8 LC_ALL=en_US.UTF-8 LD_LIBRARY_PATH=/usr/lib/llvm-19/lib ./app; cat output.txt
