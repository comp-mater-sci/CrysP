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
FFLAGS= -fpp -align dcommon -vec-report0 -standard-semantics # -assume std_mod_proc_name
FFLAGS_FIXED= -extend-source 80 
FFLAGS_FREE= -stand f08

ifdef DEBUG
OUTDIR=debug
DEBUGFLAGS= -O0 -g -debug full -check all -ftrapuv  
FCFLAGS+= $(DEBUGFLAGS)
else
OUTDIR=release
OPTFLAGS=-O3 -fast -no-ipo
FCFLAGS+= $(OPTFLAGS)
endif
#
# Append extra flags
ifdef EXTFLAGS
     FCFLAGS+= $(EXTFLAGS)
     FLDFLAGS+= $(EXTFLAGS)
endif
#
### Commonly used functions

# Builds a list of object names from a list of .for or .f90 files
fxx2o = $(subst .for,.o, $(filter %.for,$(1))) $(subst .f90,.o, $(filter %.f90,$(1)))
# Builds (relative) object paths from a list of .for or .90 files
fxx2opaths = $(patsubst %, $(2)/%,$(call fxx2o,$(1)))

# Group of core source files
CORE_SRCS = \
Alg0A.for \
Alg1.for \
Alg2.for \
Dynfil.for \
macroKinematic.f90 \
microstr.f90 \
ioconfig.f90 \
miscutils.f90 \
Pancak2.for \
Sliprat.for \
TBH.for \
cubAccess.f90 \
curAccess.f90 \
smtAccess.f90 \
texFormats.f90 \
texFormatConstants.f90 \
hardVoce.f90 \
altayHard.f90 \
KOST1x.f90 \
KOST1xState.f90



#####################################################################
# CONFIGURATION OF LIBRARY
#
## Name of the library
LIB_NAME=libaltay.a
LIB_DIR=$(OUTDIR)/lib
LIB=$(LIB_DIR)/$(LIB_NAME)
LIB_OBJDIR=$(LIB_DIR)/obj
LIB_INCLUDEDIR=$(LIB_DIR)/include

# Configuration suitable for altay as subroutine
#LIB_CFG= -DALTAY_SUBROUTINE -DNORESFILE -DNOCURFILE -DNOTWNFILE -DNOLSTFILE -DWITHCUBFILE -DWITHSMTFILE
LIB_CFG= -DALTAY_SUBROUTINE -DRCM_ENABLED -DNO_STDOUT -DPEBP_ENABLED
#
LIB_SRCS= \
$(CORE_SRCS) \
altayConfig.f90  \
altayInterface.f90 \
altayRCM.f90 \
altaySub.f90


LIB_SRCDIR=src

LIB_SRCLIST=$(append $(LIB_SRCDIR), $(LIB_SRCS))

# Build list of objects:
# Library objects
LIB_OBJS =  $(call fxx2opaths,$(LIB_SRCS),$(LIB_OBJDIR))

$(info $(LIB_OBJS))
LIB_FCFLAGS= $(FFLAGS) $(LIB_CFG) -module $(LIB_INCLUDEDIR) -I$(LIB_SRCDIR) $(FCFLAGS)




#####################################################################
# CONFIGURATION OF STANDALONE PROGRAM

# Name of program
PROG_NAME=altay
PROG_SRCDIR=src
PROG_DIR=$(OUTDIR)/bin
PROG=$(PROG_DIR)/$(PROG_NAME)
PROG_OBJDIR=$(PROG_DIR)/obj
PROG_INCLUDEDIR=$(PROG_DIR)/include
# Configuration suitable for output-rich, multiple-step execution
PROG_CFG= -DMAINDIRECT -DFINALCUB_ENABLED -DPEBP_ENABLED -DFINALBPM_ENABLED
#
PROG_SRCS= \
$(CORE_SRCS) \
MainA1.for

# Build list of objects:
# Substitute .for and .f90 extension
PROG_OBJS=$(call fxx2opaths,$(PROG_SRCS),$(PROG_OBJDIR))

$(info $(PROG_OBJS))

PROG_FCFLAGS= $(FCFLAGS) $(FFLAGS) $(PROG_CFG) -module $(PROG_INCLUDEDIR) 
PROG_LDFLAGS= $(FCFLAGS) $(FLDFLAGS)
#####################################################################
# CONFIGURATION OF TEST PROGRAM

# Name of test program
TPROG_NAME=altaySubTest
TPROG_SRCDIR=src-test
TPROG_DIR=$(OUTDIR)/test_bin
TPROG=$(TPROG_DIR)/$(TPROG_NAME)
TPROG_OBJDIR=$(TPROG_DIR)/obj
TPROG_INCLUDEDIR=$(TPROG_DIR)/include

TPROG_SRCS= \
mainSub.f90 \
testTexAccess.f90 \
verifySub.f90

# Build list of objects:
TPROG_OBJS = $(call fxx2opaths,$(TPROG_SRCS),$(TPROG_OBJDIR))

TPROG_CFG= -DALTAY_SUBROUTINE
TPROG_FCFLAGS= $(FFLAGS) $(TPROG_CFG) -module $(TPROG_INCLUDEDIR) -I$(LIB_INCLUDEDIR) -I$(TPROG_SRCDIR) $(FCFLAGS)
TPROG_LDFLAGS= $(LIB) $(FCFLAGS)
#####################################################################


MODS=*.mod 





LIB_DIRS = $(LIB_DIR) $(LIB_INCLUDEDIR) $(LIB_OBJDIR) 
PROG_DIRS = $(PROG_DIR) $(PROG_INCLUDEDIR) $(PROG_OBJDIR)
TPROG_DIRS = $(TPROG_DIR) $(TPROG_OBJDIR) $(TPROG_INCLUDEDIR)
OUT_DIRS = $(LIB_DIRS) $(PROG_DIRS) $(TPROG_DIRS)

# Build-up of the directory structure
define create-dir
@echo $@
[ -d $@ ] || mkdir -p $@
endef

.PHONY: clean info

all	: lib test prog

# aliases for targets:
#
lib	: $(LIB) | dirstructure
prog	: $(PROG) | dirstructure
test	: $(LIB) $(TPROG) | dirstructure	

info	: 
	@echo Available targets:
	@echo "lib - builds AlTay library (libaltay)"
	@echo "prog - builds AlTay stand-alone program"
	@echo "test - build a test program"

dirstructure : $(OUT_DIRS)

$(OUT_DIRS)	:
	-$(create-dir)

#
# Prog section
$(PROG)	:  $(PROG_OBJS)| $(PROG_DIR) $(PROG_OBJDIR) $(PROG_INCLUDEDIR)
	$(FC) -o $@ $(PROG_FCFLAGS) $(PROG_LDFLAGS) $(PROG_OBJS) 

$(PROG_OBJDIR)/%.o	: $(PROG_SRCDIR)/%.for | $(PROG_DIRS)
	$(FC) -o $@ -c $< $(PROG_FCFLAGS) $(FFLAGS_FIXED)  

$(PROG_OBJDIR)/%.o	: $(PROG_SRCDIR)/%.f90 | $(PROG_DIRS)
	$(FC) -o $@ -c $< $(PROG_FCFLAGS) $(FFLAGS_FREE)

#
# Lib section
$(LIB)	:$(LIB_OBJS) | $(LIB_DIRS)
	$(AR) rcs $(LIB) $(LIB_OBJS)

$(LIB_OBJDIR)/%.o	: $(LIB_SRCDIR)/%.f90 | $(LIB_DIRS)
	$(FC) -c $< -o $@ $(LIB_FCFLAGS) $(FFLAGS_FREE)

$(LIB_OBJDIR)/%.o	: $(LIB_SRCDIR)/%.for | $(LIB_DIRS)
	$(FC) -c $< -o $@ $(LIB_FCFLAGS) $(FFLAGS_FIXED)


#
# Testprog section
$(TPROG_OBJDIR)/%.o	: $(TPROG_SRCDIR)/%.f90  $(LIB) | $(TPROG_DIRS)
	$(FC) -c $< -o $@ $(TPROG_FCFLAGS) $(FFLAGS_FREE)


$(TPROG) 	:  $(LIB) $(TPROG_OBJS) | $(TPROG_DIR) $(TPROG_OBJDIR) $(TPROG_INCLUDEDIR) 
	$(FC) -o $@ $(TPROG_FCFLAGS) $^ $(LIB) $(FLDFLAGS) 





clean	: FORCE
	-rm -f $(LIB_OBJS) $(LIB) $(LIB_INCLUDEDIR)/* $(TPROG) $(TPROG_OBJS) $(TPROG_INCLUDEDIR)/* $(PROG) $(PROG_OBJS) $(PROG_INCLUDEDIR)/* 
FORCE	:

RCMINCLUDE = src/altayRCM.fpp

#
# Explicit dependency: settings
#
$(LIB_OBJS) $(PROG_OBJS) $(TPROG_OBJS)	: Makefile

#
# Explicit dependencies for library
#
#$(LIB_OBJDIR)/.o	: $(addprefix $(LIB_OBJDIR)/, )	
$(LIB_OBJDIR)/altaySub.o	: $(addprefix $(LIB_OBJDIR)/,altayConfig.o altayInterface.o altayRCM.o ioconfig.o texFormats.o) $(RCMINCLUDE)
$(LIB_OBJDIR)/Alg0A.o		: $(addprefix $(LIB_OBJDIR)/,altayHard.o KOST1x.o KOST1xState.o curAccess.o Dynfil.o altayConfig.o altayRCM.o ioconfig.o) $(RCMINCLUDE)
$(LIB_OBJDIR)/Alg1.o		: $(addprefix $(LIB_OBJDIR)/,altayRCM.o ioconfig.o) $(RCMINCLUDE)
$(LIB_OBJDIR)/Alg2.o		: $(addprefix $(LIB_OBJDIR)/,altayRCM.o) $(RCMINCLUDE)
$(LIB_OBJDIR)/Dynfil.o		: $(addprefix $(LIB_OBJDIR)/,altayConfig.o ioconfig.o miscutils.o)
$(LIB_OBJDIR)/microstr.o	: $(addprefix $(LIB_OBJDIR)/,miscutils.o)
$(LIB_OBJDIR)/Pancak2.o		: $(addprefix $(LIB_OBJDIR)/,Dynfil.o altayRCM.o ioconfig.o microstr.o) $(RCMINCLUDE)
$(LIB_OBJDIR)/cubAccess.o	: $(addprefix $(LIB_OBJDIR)/,Dynfil.o)
$(LIB_OBJDIR)/curAccess.o	: $(addprefix $(LIB_OBJDIR)/,Dynfil.o)
$(LIB_OBJDIR)/smtAccess.o	: $(addprefix $(LIB_OBJDIR)/,Dynfil.o) $(RCMINCLUDE)
$(LIB_OBJDIR)/Sliprat.o		: $(addprefix $(LIB_OBJDIR)/,altayRCM.o ioconfig.o)
$(LIB_OBJDIR)/TBH.o		: $(addprefix $(LIB_OBJDIR)/,altayRCM.o ioconfig.o) $(RCMINCLUDE)
$(LIB_OBJDIR)/texFormats.o	: $(addprefix $(LIB_OBJDIR)/,texFormatConstants.o Dynfil.o curAccess.o cubAccess.o smtAccess.o)
$(LIB_OBJDIR)/KOST1xState.o	: $(addprefix $(LIB_OBJDIR)/,KOST1x.o)
$(LIB_OBJDIR)/altayHard.o	: $(addprefix $(LIB_OBJDIR)/,ioconfig.o altayConfig.o hardVoce.o KOST1x.o)
$(LIB_OBJDIR)/hardVoce.o	: $(addprefix $(LIB_OBJDIR)/,ioconfig.o )
$(LIB_OBJDIR)/altayConfig.o	: $(addprefix $(LIB_OBJDIR)/,texFormatConstants.o hardVoce.o KOST1x.o)
#
# Explicit dependencies for test program
#
$(TPROG_OBJDIR)/mainSub.o	: $(addprefix $(TPROG_OBJDIR)/, testTexAccess.o verifySub.o)
#
# Explicit dependencies for standalone program
#
$(PROG_OBJDIR)/MainA1.o		: $(addprefix $(PROG_OBJDIR)/,ioconfig.o texFormats.o) $(RCMINCLUDE)
$(PROG_OBJDIR)/altaySub.o	: $(addprefix $(PROG_OBJDIR)/,altayConfig.o altayInterface.o ioconfig.o texFormats.o) $(RCMINCLUDE)
$(PROG_OBJDIR)/Alg0A.o		: $(addprefix $(PROG_OBJDIR)/,altayHard.o KOST1x.o KOST1xState.o curAccess.o Dynfil.o altayConfig.o ioconfig.o) $(RCMINCLUDE)
$(PROG_OBJDIR)/Alg1.o		: $(addprefix $(PROG_OBJDIR)/,ioconfig.o) $(RCMINCLUDE)
#$(PROG_OBJDIR)/Alg2.o		: $(addprefix $(PROG_OBJDIR)/,) $(RCMINCLUDE)
$(PROG_OBJDIR)/Dynfil.o		: $(addprefix $(PROG_OBJDIR)/,altayConfig.o ioconfig.o miscutils.o)
$(PROG_OBJDIR)/microstr.o	: $(addprefix $(PROG_OBJDIR)/,miscutils.o)
$(PROG_OBJDIR)/Pancak2.o	: $(addprefix $(PROG_OBJDIR)/,Dynfil.o ioconfig.o microstr.o) $(RCMINCLUDE)
$(PROG_OBJDIR)/cubAccess.o	: $(addprefix $(PROG_OBJDIR)/,Dynfil.o)
$(PROG_OBJDIR)/curAccess.o	: $(addprefix $(PROG_OBJDIR)/,Dynfil.o)
$(PROG_OBJDIR)/smtAccess.o	: $(addprefix $(PROG_OBJDIR)/,Dynfil.o) $(RCMINCLUDE)
$(PROG_OBJDIR)/Sliprat.o	: $(addprefix $(PROG_OBJDIR)/,ioconfig.o)
$(PROG_OBJDIR)/TBH.o		: $(addprefix $(PROG_OBJDIR)/,ioconfig.o) $(RCMINCLUDE)
$(PROG_OBJDIR)/texFormats.o	: $(addprefix $(PROG_OBJDIR)/,texFormatConstants.o Dynfil.o curAccess.o cubAccess.o smtAccess.o)
$(PROG_OBJDIR)/KOST1xState.o	: $(addprefix $(PROG_OBJDIR)/,KOST1x.o)
$(PROG_OBJDIR)/altayHard.o	: $(addprefix $(PROG_OBJDIR)/,ioconfig.o hardVoce.o KOST1x.o)
$(PROG_OBJDIR)/hardVoce.o	: $(addprefix $(PROG_OBJDIR)/,ioconfig.o )
$(PROG_OBJDIR)/altayConfig.o	: $(addprefix $(PROG_OBJDIR)/,texFormatConstants.o hardVoce.o KOST1x.o)



