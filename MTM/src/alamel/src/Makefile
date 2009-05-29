#### Selection of toolset
## Note that only one toolset can be used 
######################################################################
#### Toolset:
## Intel Fortran compiler
FC=ifort
FFLAGS= -fpp -mixed_str_len_arg -extend-source 80 -warn all -O3 -fast
#FLDFLAGS= -O3 -fast
## Profile-guided optimization (optional, requires additional actions)
##FFLAGS += -pg
######################################################################
#### Toolset:
### GNU Fortran compiler
#FC=gfortran
#FFLAGS = -x f77-cpp-input -O3 -ffixed-line-length-80 -Wall
### gfortran specific optimization flags (optional)
##FFLAGS += -ftree-loop-linear -ftree-loop-im
######################################################################
#####
# enable gmetex extensions
#FFLAGS += -DGMETEX_COMM

#FLDFLAGS += -pg $(CPPLIB) $(GMETEXLIB) $(BOOSTLDFLAGS)
#FLDFLAGS +=  $(CPPLIB) $(GMETEXLIB) $(BOOSTLDFLAGS)
#####

SRCS=Alg0.for  Alg1.for  Alg2.for  Dynfil.for  Lp.for  Main1.for  Pancak2.for  Sliprat.for  Stelsel.for
OBJS=Alg0.o  Alg1.o  Alg2.o  Dynfil.o  Lp.o  Main1.o  Pancak2.o  Sliprat.o  Stelsel.o

MODS=*.mod 



PROG=alamel

.PHONY=clean

$(PROG)	:	$(OBJS)
	$(FC)  $(FLDFLAGS) -o $@ $(OBJS) 

%.o : %.for
	$(FC) -c $(FFLAGS) $< -o $@


clean: 
	rm -f $< $(OBJS) $(PROG) $(MODS)

		
