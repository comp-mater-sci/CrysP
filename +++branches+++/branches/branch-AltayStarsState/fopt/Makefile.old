#
# $Id$
#
#### Selection of toolset
## Note that only one toolset can be used 
######################################################################
#### Toolset:
# Intel toolset
#
# Intel Fortran compiler
FC=ifort
#
# Intel library manager 
AR=xiar
#
# Documentation generator
DOXYGEN=doxygen
#
######################################################################
# Use per-host configuration plugin
# 
ROOTDIR=../../../..
include $(ROOTDIR)/build-tools/Makefile.host
$(info Configuration for $(BUILDHOST))
#
######################################################################

# Name of the library
LIBNAME=libopt.a

FCFLAGS=-fpp -implicitnone -standard-semantics -module $(INCLUDEDIR) 

ifdef DEBUG
LIBDIR=debug
DEBUGFLAGS= -O0 -g -debug full -check all
FCFLAGS+= $(DEBUGFLAGS)
else
LIBDIR=release
OPTFLAGS=-O3 -fast
FCFLAGS+= $(OPTFLAGS)
endif
#
######################################################################
LIB=$(LIBDIR)/$(LIBNAME)
OBJDIR=$(LIBDIR)/obj
INCLUDEDIR=$(LIBDIR)/include

LIBSRCS= \
nlls_tr.f90 \
nnls_mod.f90 \
jacobiFD.f90 \
objectiveFx.f90
# Build list of objects:
# Substitute .for extension
LIBOBJLIST=$(LIBSRCS:.f90=.o)
# Library objects
LIBOBJS = $(patsubst %, $(OBJDIR)/%, $(LIBOBJLIST))
# $(info $(LIBOBJS))

# MKL subroutines
#
# Configuration on dijkstra
# 
ifeq ($(BUILDHOST),dijkstra)
MKLINCLUDE= -I$(MKLROOT)/include/em64t/lp64
MKLPATH=$(MKLROOT)/lib/em64t
FLDFLAGS += $(MKLPATH)/libmkl_solver_lp64_sequential.a -Wl,--start-group $(MKLPATH)/libmkl_intel_lp64.a $(MKLPATH)/libmkl_sequential.a $(MKLPATH)/libmkl_core.a -Wl,--end-group -lpthread
LIBFCFLAGS += $(MKLINCLUDE) # -check bounds
endif
# 
# Configuration on VIC3
# 
ifeq ($(BUILDHOST),vic3)
# MKLROOT is set via environment variable
MKLPATH=$(MKLROOT)/lib/$(MKL_TARGET_ARCH)
MKLINCLUDE= -I$(MKLROOT)/include/$(MKL_TARGET_ARCH)/lp64
INCLUDE=  $(MKLINCLUDE) $(MODULESCFG)
MKLLDFLAGS += $(MKLPATH)/libmkl_solver_lp64_sequential.a -Wl,--start-group $(MKLPATH)/libmkl_intel_lp64.a $(MKLPATH)/libmkl_sequential.a $(MKLPATH)/libmkl_core.a -Wl,--end-group -lpthread
LIBFCFLAGS += $(MKLINCLUDE)
FLDFLAGS += $(MKLLDFLAGS)
endif
#
#

#
#
# Test program
TESTPROG=testNllsTR	

LIBDIRS = $(LIBDIR) $(OBJDIR) $(INCLUDEDIR)  

$(LIB)	:  $(LIBOBJS) | $(LIBDIRS)
	$(AR) rcs $(LIB) $(LIBOBJS)

$(OBJDIR)/%.o	: %.f90 | $(LIBDIRS)
	$(FC) $(FCFLAGS) -c $< -o $@

.PHONY	: clean mrproper

clean	: FORCE
	-rm -f $(LIBOBJS) $(LIB) $(INCLUDEDIR)/*.mod $(TESTPROG) 

mrproper : clean
	-rm -rf $(LIBDIRS)

FORCE	:

$(LIBDIR) $(OBJDIR) $(INCLUDEDIR) :
	[ -d $@ ] || mkdir -p $@


# Test program
test	: $(LIB) $(TESTPROG) 

$(TESTPROG) 	: testFunctions.f90 testNllsTR.f90 
	$(FC) $(FCFLAGS) -I$(INCLUDEDIR) $+ -o $@ $(LIB) $(FLDFLAGS) 
	
# Dependencies
$(OBJDIR)/nlls_tr.o	: $(OBJDIR)/objectiveFx.o
