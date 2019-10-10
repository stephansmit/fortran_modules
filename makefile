execname: math.o eosmodels.o main.o
	mpifort -o run math.o eosmodels.o main.o

math.o: math.f90
	mpifort -c math.f90
eosmodels_ig.o: eosmodels_ig.f90
	mpifort -c eosmodels_ig.f90
eosmodels_table.o: eosmodels_table.f90
	mpifort -c eosmodels_table.f90
eosmodels.o: eosmodels.f90
	mpifort -c eosmodels.f90
main.o: main.f90
	mpifort -c main.f90
clean:
	rm *.o *.mod run
