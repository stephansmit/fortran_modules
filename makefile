execname: turbmodels_mk.o turbmodels_sst.o turbmodels.o test.o main.o
	gfortran -o run turbmodels_mk.o turbmodels_sst.o turbmodels.o  test.o main.o

turbmodels.o: turbmodels.f90
	gfortran -c turbmodels.f90
turbmodels_mk.o: turbmodels.o turbmodels_mk.f90
	gfortran -c turbmodels_mk.f90
turbmodels_sst.o: turbmodels_sst.f90
	gfortran -c turbmodels_sst.f90
test.o: test.f90
	gfortran -c test.f90
main.o: main.f90
	gfortran -c main.f90
clean:
	rm *.o *.mod run
