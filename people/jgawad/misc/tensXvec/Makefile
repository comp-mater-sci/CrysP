FC=ifort

FCFLAGS = -fpp -standard-semantics -openmp

FNGDIR=$(HOME)/jgprojects/TWRMTMProject/code/trunk/tools/fng
FNGINCLUDE=-I$(FNGDIR)/projects/libfngCRI/release/include -I$(FNGDIR)/projects/libfngCore/release/include
FNGLIB=-L$(FNGDIR)/release/lib -lfngCRI -lfngCore

.PHONY : clean

tensXvec : tensXvec.f90
	$(FC) $(FCFLAGS) $< $(FNGINCLUDE) -o $@ $(FNGLIB)

clean:
	-rm -f tensXvec
