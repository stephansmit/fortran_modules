execname: eosmodels.o eosmodels_table.o eosmodels_ig.o  main.o
	gfortran -o run eosmodels.o eosmodels_table.o eosmodels_ig.o main.o

eosmodels_ig.o: eosmodels_ig.f90
	gfortran -c eosmodels_ig.f90
eosmodels_table.o: eosmodels_table.f90
	gfortran -c eosmodels_table.f90
eosmodels.o: eosmodels.f90
	gfortran -c eosmodels.f90
main.o: main.f90
	gfortran -c main.f90
clean:
	rm *.o *.mod run
