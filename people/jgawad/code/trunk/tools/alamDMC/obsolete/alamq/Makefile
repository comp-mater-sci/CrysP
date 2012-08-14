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
ROOTDIR=../..
include $(ROOTDIR)/build-tools/Makefile.host
$(info Configuration for $(BUILDHOST))
#
######################################################################

FCFLAGS=-fpp -module $(INCLUDEDIR) 

ifdef DEBUG
ConfigurationName=debug
LIBDIR=debug
DEBUGFLAGS= -O0 -g -debug full -check all
FCFLAGS+= $(DEBUGFLAGS)
TRANSCONFIG=DEBUG=1
else
ConfigurationName=release
LIBDIR=release
OPTFLAGS=-O3 -fast
FCFLAGS+= $(OPTFLAGS)
endif
#
######################################################################
OBJDIR=$(ConfigurationName)/obj
INCLUDEDIR=$(ConfigurationName)/include

SRCS= \
alamYLP.f90 \
main.f90 \
qrsTypes.f90 \
vumat_libvec5.for

# Build list of objects:
# Substitute .for extension
OBJLISTA=$(SRCS:.f90=.o)
OBJLIST=$(OBJLISTA:.for=.o)
# Library objects
OBJS = $(patsubst %, $(OBJDIR)/%, $(OBJLIST))
$(info $(OBJS))

## Dependencies:
LIBALAMELDIR=../../../../MTM/branches/alamelSub/src/$(ConfigurationName)/lib
FCFLAGS+= -I$(LIBALAMELDIR)/include
FLDFLAGS+= $(LIBALAMELDIR)/libalamel.a

LIBOPTROOT=../fng/utils/optlib
LIBOPTDIR=$(LIBOPTROOT)/$(ConfigurationName)
FCFLAGS+= -I$(LIBOPTDIR)/include

# Name of the library
LIBOPTNAME=libopt.a
LIBOPTPATH=$(LIBOPTDIR)/$(LIBOPTNAME)
FLDFLAGS+= $(LIBOPTPATH)
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
ifeq ($(BUILDHOST),vic3)
MKLINCLUDE= -I$(MKLROOT)/include/em64t/lp64
MKLPATH=$(MKLROOT)/lib/em64t
FLDFLAGS += $(MKLPATH)/libmkl_solver_lp64_sequential.a -Wl,--start-group $(MKLPATH)/libmkl_intel_lp64.a $(MKLPATH)/libmkl_sequential.a $(MKLPATH)/libmkl_core.a -Wl,--end-group -lpthread
LIBFCFLAGS += $(MKLINCLUDE) # -check bounds
endif
#
#
# Test program
PROG=alamq	
 

$(PROG)	: $(OBJDIR) $(INCLUDEDIR) $(OBJS)
	$(FC) $(FCFLAGS) -I$(INCLUDEDIR) $(OBJS) -o $@  $(FLDFLAGS) 

$(OBJDIR)/%.o	: %.f90
	$(FC) -c $< $(FCFLAGS) -o $@

$(OBJDIR)/%.o	: %.for
	$(FC) -c $< -fixed $(FCFLAGS) -o $@

clean	: FORCE
	-rm -f $(OBJS) $(LIB) $(INCLUDEDIR)/*.mod $(TESTPROG) 
FORCE	:

$(LIBDIR) :
	[ -d $@ ] || mkdir -p $@

$(OBJDIR) :
	[ -d $@ ] || mkdir -p $@

$(INCLUDEDIR) :
	[ -d $@ ] || mkdir -p $@

$(OBJDIR)/alamYLP.o 	:  $(OBJDIR)/vumat_libvec5.o $(LIBOPTPATH)
$(OBJDIR)/main.o	:  $(OBJDIR)/alamYLP.o $(OBJDIR)/qrsTypes.o $(LIBOPTPATH)

$(LIBOPTPATH)		:
	make -C $(LIBOPTROOT) $(TRANSCONFIG)
