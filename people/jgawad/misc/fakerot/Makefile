

FNGDIR=$(HOME)/jgprojects/TWRMTMProject/code/trunk/tools/fng
FNGLIBDIR=$(FNGDIR)/release/lib
FNGINCLUDES= -I$(FNGDIR)/projects/libfngCRI/release/include

FCFLAGS= -fpp -O3 $(FNGINCLUDES) -standard-semantics -fast -openmp
FLDFLAGS= $(FNGLIBDIR)/libfngCRI.a

.PHONY  : clean

fakerot : fakerot.f90
	$(FC) $(FCFLAGS) -o $@ $< $(FLDFLAGS) 

clean	:
	-rm fakerot
