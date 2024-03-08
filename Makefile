ifndef $(FC)
FC=gfortran
endif

ifndef $(FFLAGS)
	ifeq ($(FC),gfortran)
	FFLAGS=-g -Wall -ffree-line-length-none -fcheck=all -O3 -march=native -fbacktrace -fimplicit-none -cpp
	else
	FFLAGS=-g -O3 -fpp
	endif
endif

LFLAGS=-L${MKLROOT}/lib/intel64 -Wl,--no-as-needed -lmkl_gf_lp64 -lmkl_sequential -lmkl_core -lpthread -lm -ldl

incdir=./include
objdir=./obj

# Define the flag for module output
ifeq ($(FC),gfortran)
JFLAG=-J $(incdir)
else ifeq ($(FC),ifx)
JFLAG=-module $(incdir)
endif

altaysrc= $(wildcard AlTay/src/*.f90) $(wildcard AlTay/src/*/*.f90) $(wildcard AlTay/src/*/*/*.f90) $(wildcard AlTay/src/*/*/*/*.f90)
altayobj=$(patsubst AlTay/src/%.f90, $(objdir)/%.o, $(altaysrc))

VEFsrc= $(wildcard VEF/src/*.f90) $(wildcard VEF/src/*/*.f90)
VEFobj=$(patsubst VEF/src/%.f90, $(objdir)/%.o, $(VEFsrc))

all: VEF/bin/alamDMC $(altayobj) $(VEFobj)

include dependencylist

# Note, if you change objdir to something else, this command may fail
dependencylist: $(altaysrc) $(VEFsrc)
	./makefdeps.sh $(altaysrc) $(VEFsrc) > dependencylist
	sed -i 's/AlTay\/src/obj/g' dependencylist
	sed -i 's/VEF\/src/obj/g' dependencylist
	sed -i 's/criMacros.fpp/include\/criMacros.fpp/g' dependencylist
	sed -i 's/msgFormats.inc//g' dependencylist
	sed -i 's/mkl_rci.f90//g' dependencylist

# criMacros.fpp is defined in AlTay/include/criMacros.fpp, but more convenient to have it in standard include directory
$(incdir)/criMacros.fpp: AlTay/include/criMacros.fpp
	cp AlTay/include/criMacros.fpp $(incdir)/criMacros.fpp

# Compilation for Altay
$(objdir)/%.o: AlTay/src/%.f90
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) $(JFLAG) -I $(incdir) -c $< -o $@

# Compilation for VEF
$(objdir)/%.o: VEF/src/%.f90
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) $(JFLAG) -I"${MKLROOT}/include" -I $(incdir) -c $< -o $@

# Compilation for main VEF program: alamDMC
VEF/bin/alamDMC: $(objdir)/alamDMC.o $(altayobj) $(VEFobj)
	@mkdir -p $(@D)
	$(FC) $(FFLAGS) -I $(incdir) $^ -o $@ $(LFLAGS)

# clean
clean:
	rm -rf $(objdir)/*.o $(incdir)/*.mod $(incdir)/*.smod