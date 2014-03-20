#
# $Id$
#

FC=ifort

ROOTDIR=../../../..
TOOLSDIR=$(ROOTDIR)/code/branch/build-tools/
VUMATDIR=../../vumat

# Use per-host configuration plugin
include $(TOOLSDIR)/Makefile.host
$(info Configuration for $(BUILDHOST))
include $(TOOLSDIR)/Makefile.knownhosts
include $(TOOLSDIR)/Makefile.utils
#
FCFLAGS=-fpp -module $(INCLUDEDIR) -I$(INCLUDEDIR) -standard-semantics  -DKfngConstants=fngConstants
#
ifdef DEBUG
ConfigurationName=debug
FCFLAGS+= -g -O0 -debug all -check all -traceback
else
ConfigurationName=release
FCFLAGS+= -O3 -openmp -no-ipo
endif


OBJDIR=$(ConfigurationName)/obj
INCLUDEDIR=$(ConfigurationName)/include



SRCS = \
hardAltay.f90 \
hardGeneric.f90 \
hardSwift.f90 \
polyApproximation.f90 \
polyHardUtils.f90 \
updateData.f90 \
vumat_polyhard.f90

OBJS=$(SRCS:%.f90=$(OBJDIR)/%.o)
$(info 'Object files ' $(OBJS))

## Dependencies:

LIBALTAYDIR=$(ROOTDIR)/MTM/branches/AlTaySub/$(ConfigurationName)/lib
LIBALTAYPATH=$(LIBALTAYDIR)/libaltay.a
FCFLAGS+= -I$(LIBALTAYDIR)/include
FLDFLAGS+= $(LIBALTAYPATH)

FNGROOTDIR=$(ROOTDIR)/code/trunk/tools/fng
LIBFNGDIR=$(FNGROOTDIR)/$(ConfigurationName)/lib
FNGCRIINCLUDE= -I$(FNGROOTDIR)/projects/libfngCRI/$(ConfigurationName)/include -I$(FNGROOTDIR)/projects/libfngCore/$(ConfigurationName)/include

FCFLAGS+=$(FNGCRIINCLUDE)
FLDFLAGS+= -L$(LIBFNGDIR) -lfngCRI -lfngCore

LIBOPTROOT=$(FNGROOTDIR)/utils/optlib
LIBOPTDIR=$(LIBOPTROOT)/$(ConfigurationName)
FCFLAGS+= -I$(LIBOPTDIR)/include


LIBALAMDMCDIR=$(ROOTDIR)/code/trunk/tools/alamDMC/libAlamDMC/$(ConfigurationName)
FCFLAGS += -I$(LIBALAMDMCDIR)/include
FLDFLAGS += -L$(LIBALAMDMCDIR) -lalamDMC

# Name of the library
LIBOPTNAME=libopt.a
LIBOPTPATH=$(LIBOPTDIR)/$(LIBOPTNAME)
FLDFLAGS+= $(LIBOPTPATH)


FCFLAGS += $(MKLINCLUDE)
FLDFLAGS += $(MKLPATH)/libmkl_lapack95_lp64.a $(MKLLDFLAGS) 

########################################
OUT_DIRS = $(OBJDIR) $(INCLUDEDIR)



$(OUT_DIRS)	:OBJDIR=$(ConfigurationName)/obj
INCLUDEDIR=$(ConfigurationName)/include

TargetPath=$(ConfigurationName)/polyHard

.PHONY : clean dirstructure

build	:	$(ConfigurationName)/polyHard


$(TargetPath)	: polyHard.f90 $(OBJS)
	$(FC) $<  -o $@ $(OBJS) $(FCFLAGS) $(FLDFLAGS) -lfngCore -lfngCRI

$(OBJDIR)/%.o	: %.f90 | dirstructure
	$(FC) -c $< $(FCFLAGS) -o $@

dirstructure : | $(OUT_DIRS)

$(OUT_DIRS)	:
	[ -d $@ ] || mkdir -p $@

clean	:
	-rm $(OBJDIR)/* $(INCLUDEDIR)/*	$(TargetPath)
# $(OBJDIR)/
#
$(OBJDIR)/polyHardUtils.o 	: $(addprefix $(OBJDIR)/, hardAltay.o hardGeneric.o hardSwift.o updateData.o vumat_polyhard.o)
$(OBJDIR)/hardAltay.o 		: $(addprefix $(OBJDIR)/, updateData.o)
#
#
# Make a soft link to vumat_polyhard.f90
#vumat_polyhard.f90	:	
#	[ -f $@ ] || ln -s $(ROOTDIR)/code/branch/vumat_skyfall/$@  $@
