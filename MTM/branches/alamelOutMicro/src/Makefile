FC=ifort
#FC=gfortran

## Preprocessor flags:
##  * NORESFILE : no <prefix>.RES file is generated
##  * WITHCUBFILE  :  generate <prefix>.CUB file
##
##

FFLAGS= -fpp -extend-source 80 -O3 -ipo -vec-report0  -DNORESFILE -DNOCURFILE -DWITHCUBFILE # -warn all  -opt-report0

#FFLAGS= -pg   -O3
#FFLAGS= -O3


#FLDFLAGS= -pg $(CPPLIB) $(GMETEXLIB) $(BOOSTLDFLAGS)
FLDFLAGS=  $(CPPLIB) $(GMETEXLIB) $(BOOSTLDFLAGS)

SRCS=Alg0.for  Alg1.for  Alg2.for  Dynfil.for  Lp.for  Main1.for  Pancak2.for  Sliprat.for  Stelsel.for
OBJS=Alg0.o  Alg1.o  Alg2.o  Dynfil.o  Lp.o  Main1.o  Pancak2.o  Sliprat.o  Stelsel.o

MODS=*.mod 



PROG=alamel

.PHONY=clean

$(PROG)	:	$(OBJS)
	$(FC) $(FFLAGS)  $(FLDFLAGS) -o $@ $(OBJS) 

%.o : %.for
	$(FC) -c $(FFLAGS) $< -o $@


clean: 
	rm -f $< $(OBJS) $(PROG) $(MODS)

		
