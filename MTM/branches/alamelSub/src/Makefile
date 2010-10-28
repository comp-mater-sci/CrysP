#
# $Id$
#
#### Selection of toolset
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

## Preprocessor flags:
##  * NORESFILE : no <prefix>.RES file is generated
##  * NOCURFILE : no <prefix>.CUR file is generated
##  * WITHCUBFILE  :  generate <prefix>.CUB file
##  * MAINDIRECT  : assume that MAIN1.CTL contains actual configuration
##
#
# Generic Fortran compiler options, valid for all configurations
#
FFLAGS= -fpp -align dcommon -vec-report0 
FFLAGS_FIXED= -extend-source 80 

ifdef DEBUG
OUTDIR=debug
DEBUGFLAGS= -O0 -g -debug full -check all
FCFLAGS+= $(DEBUGFLAGS)
else
OUTDIR=release
OPTFLAGS=-O3 -fast
FCFLAGS+= $(OPTFLAGS)
endif
#####################################################################
# CONFIGURATION OF LIBRARY
#
## Name of the library
LIB_NAME=libalamel.a
LIB_DIR=$(OUTDIR)/lib
LIB=$(LIB_DIR)/$(LIB_NAME)
LIB_OBJDIR=$(LIB_DIR)/obj
LIB_INCLUDEDIR=$(LIB_DIR)/include

# Configuration suitable for alamel as subroutine
LIB_CFG= -DALAMEL_SUBROUTINE -DNORESFILE -DNOCURFILE -DNOTWNFILE -DNOLSTFILE -DWITHCUBFILE -DWITHSMTFILE
#
LIB_SRCS= \
Alg0.for \
Alg1.for \
Alg2.for \
Dynfil.for \
Lp.for \
Main1.for \
Pancak2.for \
Sliprat.for \
Stelsel.for \
alamelSub.for \
alamelConfig.f90 \
alamelInterface.f90 \

# Build list of objects:
# Substitute .for and .f90 extension
LIB_FOBJLIST=$(LIB_SRCS:.for=.o)
LIB_OBJLIST=$(LIB_FOBJLIST:.f90=.o)
# Library objects
LIB_OBJS = $(patsubst %, $(LIB_OBJDIR)/%, $(LIB_OBJLIST))

#$(info $(LIB_OBJS))

LIB_FCFLAGS= $(FFLAGS) $(LIB_CFG) -module $(LIB_INCLUDEDIR) $(FCFLAGS)




#####################################################################
# CONFIGURATION OF STANDALONE PROGRAM

# Name of program
PROG_NAME=alamel
PROG_DIR=$(OUTDIR)/bin
PROG=$(PROG_DIR)/$(PROG_NAME)
PROG_OBJDIR=$(PROG_DIR)/obj
PROG_INCLUDEDIR=$(PROG_DIR)/include
ifeq ($(flavor),batch)
# Configuration suitable for batch-mode, single step execution
PROG_CFG= -DMAINDIRECT -DNORESFILE -DNOCURFILE  -DNOTWNFILE -DNOLSTFILE -DWITHCUBFILE -DWITHSMTFILE
else
# Configuration suitable for output-rich, multiple-step execution
PROG_CFG= -DMAINDIRECT 
endif
#
PROG_SRCS= \
Alg0.for \
Alg1.for \
Alg2.for \
Dynfil.for \
Lp.for \
Main1.for \
Pancak2.for \
Sliprat.for \
Stelsel.for 

# Build list of objects:
# Substitute .for and .f90 extension
PROG_OBJLIST=$(PROG_SRCS:.for=.o)
# Library objects
PROG_OBJS = $(patsubst %, $(PROG_OBJDIR)/%, $(PROG_OBJLIST))

$(info $(PROG_OBJS))

PROG_FCFLAGS= $(FCFLAGS) $(FFLAGS) $(PROG_CFG) -module $(PROG_INCLUDEDIR) 
PROG_LDFLAGS= $(FCFLAGS)
#####################################################################
# CONFIGURATION OF TEST PROGRAM

# Name of test program
TPROG_NAME=alamelSubTest
TPROG_DIR=$(OUTDIR)/test_bin
TPROG=$(TPROG_DIR)/$(TPROG_NAME)
TPROG_OBJDIR=$(TPROG_DIR)/obj
TPROG_INCLUDEDIR=$(TPROG_DIR)/include

TPROG_CFG= -DALAMEL_SUBROUTINE
TPROG_FCFLAGS= $(FFLAGS) $(TPROG_CFG) -module $(TPROG_INCLUDEDIR) -I$(LIB_INCLUDEDIR) $(FCFLAGS)
TPROG_LDFLAGS= $(LIB) $(FCFLAGS)
#####################################################################


MODS=*.mod 


.PHONY=clean

all	: lib test prog

# aliases for targets:
#
lib	: $(LIB)
prog	: $(PROG)
test	: $(LIB) $(TPROG)	

#
# Prog section
$(PROG)	: $(PROG_DIR) $(PROG_OBJDIR) $(PROG_INCLUDEDIR) $(PROG_OBJS)
	$(FC) -o $@ $(PROG_FCFLAGS) $(PROG_LDFLAGS) $(PROG_OBJS) 

$(PROG_OBJDIR)/%.o	: %.for
	$(FC) -o $@ -c $< $(PROG_FCFLAGS) $(FFLAGS_FIXED)  

$(PROG_DIR) :
	[ -d $@ ] || mkdir -p $@

$(PROG_OBJDIR) :
	[ -d $@ ] || mkdir -p $@

$(PROG_INCLUDEDIR) :
	[ -d $@ ] || mkdir -p $@

#
# Lib section
$(LIB)	: $(LIB_DIR) $(LIB_OBJDIR) $(LIB_INCLUDEDIR) $(LIB_OBJS)
	$(AR) rcs $(LIB) $(LIB_OBJS)

$(LIB_OBJDIR)/%.o	: %.f90
	$(FC) $(LIB_FCFLAGS) -c $< -o $@

$(LIB_OBJDIR)/%.o	: %.for
	$(FC) $(LIB_FCFLAGS) $(FFLAGS_FIXED) -c $< -o $@

$(LIB_DIR) :
	[ -d $@ ] || mkdir -p $@

$(LIB_OBJDIR) :
	[ -d $@ ] || mkdir -p $@

$(LIB_INCLUDEDIR) :
	[ -d $@ ] || mkdir -p $@

#
# Testprog section
$(TPROG) 	:  mainSub.f90 $(LIB) $(TPROG_DIR) $(TPROG_OBJDIR) $(TPROG_INCLUDEDIR) 
	$(FC) $(TPROG_FCFLAGS) $< -o $@ $(LIB) $(FLDFLAGS) 

$(TPROG_DIR) :
	[ -d $@ ] || mkdir -p $@

$(TPROG_OBJDIR) :
	[ -d $@ ] || mkdir -p $@

$(TPROG_INCLUDEDIR) :
	[ -d $@ ] || mkdir -p $@



clean	: FORCE
	-rm -f $(LIB_OBJS) $(LIB) $(LIB_INCLUDEDIR)/* $(TPROG) $(PROG) $(PROG_OBJS) $(PROG_INCLUDEDIR)/* 
FORCE	:


# Explicit dependencies for library
#
#$(LIB_OBJDIR)/.o	: $(addprefix $(LIB_OBJDIR)/, )	
$(LIB_OBJDIR)/alamelSub.o	: $(addprefix $(LIB_OBJDIR)/, alamelConfig.o alamelInterface.o Dynfil.o)	
$(LIB_OBJDIR)/Alg0.o		: $(addprefix $(LIB_OBJDIR)/, alamelConfig.o)	
$(LIB_OBJDIR)/Dynfil.o		: $(addprefix $(LIB_OBJDIR)/, alamelConfig.o alamelInterface.o)	
$(LIB_OBJDIR)/Pancak2.o		: $(addprefix $(LIB_OBJDIR)/, Dynfil.o)	

# Explicit dependencies for test program
#
#$(TPROG_OBJDIR)/.o	: $(addprefix $(TPROG_OBJDIR)/, )	

# Explicit dependencies for standalone program
#
#$(PROG_OBJDIR)/.o	: $(addprefix $(PROG_OBJDIR)/, )	
$(PROG_OBJDIR)/Main1.o	: $(addprefix $(PROG_OBJDIR)/, Dynfil.o )	

