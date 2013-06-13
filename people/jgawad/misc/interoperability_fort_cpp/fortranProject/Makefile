
FC=ifort

FCFLAGS=-check all

FCLDFLAGS= -cxxlib -L../cppProjectLib -lcppProject

.PHONY: clean 

all	: fortranProject

fortranProject:	cppLibInterface.o main.o
	$(FC) -o $@ $^ $(FCFLAGS) $(FCLDFLAGS)

cppLibInterface.o	: cppLibInterface.f90
	$(FC) -c $< $(FCFLAGS)

main.o	: main.f90 cppLibInterface.o
	$(FC) -c $< $(FCFLAGS)


clean :
	-rm *.o fortranProject
