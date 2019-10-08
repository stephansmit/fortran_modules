execname: turbmodels.o main.o turbmodels_mk.o turbmodels_sst.o
	gfortran -o run turbmodels.o turbmodels_mk.o turbmodels_sst.o main.o

%.o : %.mod

turbmodels.mod: turbmodels.o turbmodels.f90
	gfortran -c turbmodels.f90
turbmodels.o: turbmodels.f90
	gfortran -c turbmodels.f90
turbmodels_mk.mod: turbmodels_mk.o turbmodels_mk.f90
	gfortran -c turbmodels_mk.f90
turbmodels_mk.o: turbmodels_mk.f90
	gfortran -c turbmodels_mk.f90
turbmodels_sst.o: turbmodels_sst.f90
	gfortran -c turbmodels_sst.f90
turbmodels_sst.mod: turbmodels_sst.o turbmodels_sst.f90
	gfortran -c turbmodels_sst.f90
main.o: turbmodels.mod main.f90
	gfortran -c main.f90
clean:
	rm turbmodels.mod turbmodels_mk.o main.o turbmodels_sst.o run
