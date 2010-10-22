FC=ifort
#FC=gfortran

## Preprocessor flags:
##  * NORESFILE : no <prefix>.RES file is generated
##  * NOCURFILE : no <prefix>.CUR file is generated
##  * WITHCUBFILE  :  generate <prefix>.CUB file
##  * MAINDIRECT  : assume that MAIN1.CTL contains actual configuration
##

# Configuration suitable for batch-mode, single step execution
ALAMELCFG= -DMAINDIRECT -DNORESFILE -DNOCURFILE -DWITHCUBFILE -DWITHSMTFILE
# Configuration suitable for alamel as subroutine
ALAMELCFG= -DALAMEL_SUBROUTINE -DNORESFILE -DNOCURFILE -DNOTWNFILE -DNOLSTFILE  
#
# Configuration suitable for output-rich, multiple-step execution
#ALAMELCFG= -DMAINDIRECT 


FFLAGS= -fpp -align dcommon -O3 -ipo -fast -vec-report0 $(ALAMELCFG) # -warn all  -opt-report0

FFLAGS_FIXED= -extend-source 80 

#FFLAGS= -pg   -O3
#FFLAGS= -O3


#FLDFLAGS= -pg $(CPPLIB) $(GMETEXLIB) $(BOOSTLDFLAGS)
FLDFLAGS=  $(CPPLIB) $(GMETEXLIB) $(BOOSTLDFLAGS)

SRCS=Alg0.for  Alg1.for  Alg2.for  Dynfil.for  Lp.for  Main1.for  Pancak2.for  Sliprat.for  Stelsel.for alamelConfig.f90 alamelInterface.f90 mainSub.f90
OBJS=Alg0.o  Alg1.o  Alg2.o  Dynfil.o  Lp.o  Main1.o  Pancak2.o  Sliprat.o  Stelsel.o alamelConfig.o alamelInterface.o mainSub.o

MODS=*.mod 



PROG=alamel

.PHONY=clean

$(PROG)	:	$(OBJS)
	$(FC) $(FFLAGS)  $(FLDFLAGS) -o $@ $(OBJS) 

%.o : %.for
	$(FC) -c $(FFLAGS_FIXED) $(FFLAGS) $< -o $@

%.o : %.f90
	$(FC) -c  $(FFLAGS) $< -o $@

clean: 
	rm -f $< $(OBJS) $(PROG) $(MODS)

# Explicit dependencies
mainSub.o	:	alamelConfig.o alamelInterface.o
Main1.o		:	Dynfil.o alamelConfig.o alamelInterface.o
Alg0.o		:	alamelConfig.o
Alg0.o		: 	Dynfil.o

